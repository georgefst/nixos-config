{- | A minimal driver for Analog Devices' SigmaDSP parts - specifically the ADAU1451 on Sol's
HiFiBerry DAC+ DSP - over Linux's @spidev@ interface.

This exists because the DSP is the only thing in the box that can see the Toslink input: the optical
signal goes straight into the ADAU1451 and out to the DAC without ever touching the Pi, so ALSA and
PipeWire cannot attenuate it. See the long comment in @modules/sol.nix@ for the full picture,
including how the register addresses were discovered.

The reference implementation is HiFiBerry's @hifiberry-dsp@, which we package (see
@nix/hifiberry-dsp.nix@) but deliberately do not run as a service. It is pure Python with no
underlying C library, and the part of it we actually need is tiny: this module is a direct
translation of about thirty lines of it. Every non-obvious constant below is cited against a
permalink into the exact revision we have pinned as a flake input.

Note that upstream's own volume support is a 1Hz polling thread that synchronises a *synthetic*
ALSA control with the DSP register
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/alsa/alsasync.py#L62-L66>);
writing the register ourselves is both simpler and immediate.
-}
module SigmaDSP (
    Address,
    Percent,
    withDevice,
    readMemory,
    writeMemory,
    readGain,
    writeGain,
    readInt,
    writeInt,
    percentToAmplification,
    amplificationToPercent,
) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign (Ptr, allocaBytes, castPtr, fillBytes, peekArray, pokeArray, pokeByteOff, ptrToWordPtr)
import Foreign.C (CInt (..), CULong (..), throwErrnoIfMinus1_)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, openFd)
import System.Posix.Types (Fd (Fd))

-- | A DSP memory address. Parameter RAM is 0x0000-0xdfff; hardware registers start at 0xf000.
type Address = Word16

{- | A volume, as a whole percentage in [0, 100].

Integral rather than fractional because that is what upstream uses throughout - see
'amplificationToPercent', which mirrors a Python function returning @int@.
-}
type Percent = Int

-- $spi

{- | @SPI_IOC_MESSAGE(1)@.

This is @_IOW('k', 0, struct spi_ioc_transfer[1])@. The @_IOC@ encoding is
@(dir \<\< 30) .|. (size \<\< 16) .|. (type \<\< 8) .|. nr@, so with @dir = 1@ (write), @size = 32@
(one @struct spi_ioc_transfer@), @type = 0x6b@ ('k') and @nr = 0@ we get 0x40206b00. aarch64 uses
the @asm-generic@ definitions, so this is the same value it would be on x86_64.
-}
spiIocMessage1 :: CULong
spiIocMessage1 = 0x40206B00

-- | @sizeof(struct spi_ioc_transfer)@ on a 64-bit kernel.
spiIocTransferSize :: Int
spiIocTransferSize = 32

{- | Bus speed. Matches upstream's @spi.max_speed_hz@
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/hardware/spi.py#L25-L38>),
which also sets 8 bits per word and SPI mode 0 - the latter being the spidev default, so we only
have to state the former.
-}
speedHz :: Word32
speedHz = 1_000_000

{- | @ioctl@ is variadic in C, but every caller passes exactly one pointer argument, and on Linux
(both aarch64 and x86_64) that is passed in a register regardless, so a fixed arity is safe.
-}
foreign import ccall unsafe "ioctl"
    c_ioctl :: CInt -> CULong -> Ptr () -> IO CInt

{- | Open the SPI device for the duration of an action.

We open per operation rather than holding the descriptor in 'George.Core.AppState'. Volume changes
happen at human rates, so two extra syscalls are irrelevant, and in exchange the x86_64 @vms.sol@
build - which has no @/dev/spidev0.0@ at all - degrades to a logged error from an individual action
rather than failing at startup.
-}
withDevice :: FilePath -> (Fd -> IO a) -> IO a
withDevice path = bracket (openFd path ReadWrite defaultFileFlags) closeFd

{- | One full-duplex SPI transfer: send these bytes, receive exactly as many back.

It has to be a single @SPI_IOC_MESSAGE@ rather than a @write@ followed by a @read@, because
chip-select must stay asserted across both the address header and the data. That is the only real
reason this module needs the FFI at all.
-}
transfer :: Fd -> [Word8] -> IO [Word8]
transfer (Fd fd) out =
    allocaBytes len \txPtr ->
        allocaBytes len \rxPtr -> do
            pokeArray txPtr out
            allocaBytes spiIocTransferSize \msg -> do
                -- everything we don't set (delay_usecs, cs_change, {tx,rx}_nbits, word_delay_usecs,
                -- pad) must be zero
                fillBytes msg (0 :: Word8) spiIocTransferSize
                -- `tx_buf` and `rx_buf` are `__u64` even on 32-bit hosts
                pokeByteOff msg 0 (fromIntegral (ptrToWordPtr txPtr) :: Word64)
                pokeByteOff msg 8 (fromIntegral (ptrToWordPtr rxPtr) :: Word64)
                pokeByteOff msg 16 (fromIntegral len :: Word32)
                pokeByteOff msg 20 speedHz
                pokeByteOff msg 26 (8 :: Word8) -- bits_per_word
                throwErrnoIfMinus1_ "ioctl(SPI_IOC_MESSAGE)" $ c_ioctl fd spiIocMessage1 (castPtr msg)
            peekArray len rxPtr
  where
    len = length out

{- | Read @n@ bytes of DSP memory.

The wire format is a three byte header - read\/write flag, then the address big-endian - followed by
the payload, and since the transfer is full duplex the reply is offset by that same three bytes.
See
<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/hardware/spi.py#L53-L68>.

Note that a "cell" is four bytes in parameter RAM but only two in the register space at 0xf000 and
above
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/hardware/adau145x.py#L143-L152>);
callers pass the length explicitly, and everything we touch is RAM.
-}
readMemory :: Fd -> Address -> Int -> IO ByteString
readMemory fd addr n = B.pack . drop 3 <$> transfer fd (header 1 addr <> replicate n 0)

-- | Write bytes to DSP memory. See 'readMemory' for the framing.
writeMemory :: Fd -> Address -> ByteString -> IO ()
writeMemory fd addr d = void . transfer fd $ header 0 addr <> B.unpack d

header :: Word8 -> Address -> [Word8]
header rw addr = [rw, fromIntegral (addr `shiftR` 8), fromIntegral (addr .&. 0xff)]

{- | Read a parameter RAM cell as a fixed point number.

Mirrors @Adau145x.decimal_val@
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/hardware/adau145x.py#L123-L141>),
including its sign convention: the 32-bit word is 8.24 fixed point, and anything at or above 128 is
read as negative. Gains are never negative in practice, so that branch is here purely for fidelity.
-}
readGain :: Fd -> Address -> IO Double
readGain fd addr = do
    w <- readInt fd addr
    let f = fromIntegral w / 2 ^ (24 :: Int)
    pure if f >= 128 then f - 256 else f

{- | Write a fixed point number to a parameter RAM cell.

Mirrors @Adau145x.decimal_repr@
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/hardware/adau145x.py#L106-L121>).

Note 'truncate' rather than 'round': upstream ends with @int(f)@, which truncates towards zero. The
difference is one part in 2^24 and inaudible, but there is no reason to diverge. The negative branch
(@f = 256 + f@) is again included only for fidelity.
-}
writeGain :: Fd -> Address -> Double -> IO ()
writeGain fd addr g = writeInt fd addr . truncate $ (if g < 0 then 256 + g else g) * 2 ^ (24 :: Int)

-- | Read a parameter RAM cell as a raw 32-bit integer - used for the mute flags.
readInt :: Fd -> Address -> IO Word32
readInt fd addr = beWord32 <$> readMemory fd addr 4
  where
    beWord32 = B.foldl' (\acc b -> (acc `shiftL` 8) .|. fromIntegral b) 0

-- | Write a raw 32-bit integer to a parameter RAM cell.
writeInt :: Fd -> Address -> Word32 -> IO ()
writeInt fd addr w =
    writeMemory fd addr . B.pack $ [fromIntegral (w `shiftR` s) | s <- [24, 16, 8, 0]]

{- | Convert a volume percentage to a linear amplification factor, on a logarithmic taper.

Mirrors @percent2amplification@
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/filtering/volume.py#L61-L66>),
i.e. @a * exp(b * percent / 100)@.

The coefficients are the @dbrange <= 60@ row of @log_coefficients@
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/filtering/volume.py#L37-L58>),
60dB being the default that both @dsptoolkit@ and the ALSA sync thread use. A profile can override
it with @volumeControlRangeDb@ metadata, but ours does not set that, so the range is fixed here
rather than made configurable.

Matching upstream's curve exactly is deliberate: it means @dsptoolkit get-volume@ and the
@/tmp/dsp@-style helpers agree with us about what a given register value means.

Zero is special-cased to true silence, since the curve itself bottoms out at 'taperA' (-60dB)
rather than at nothing.
-}
percentToAmplification :: Percent -> Double
percentToAmplification p
    | p <= 0 = 0
    | otherwise = taperA * exp (taperB * fromIntegral p / 100)

{- | The inverse of 'percentToAmplification'.

Mirrors @amplification2percent@
(<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/filtering/volume.py#L69-L78>).
Both Python's @round@ and Haskell's 'round' break ties to even, so this agrees to the last unit.

Only the two endpoints are clamped, exactly as upstream: an amplification below 'taperA' yields a
negative percentage, and it is the caller's job to clamp (as the ALSA sync thread does at
<https://github.com/hifiberry/hifiberry-dsp/blob/e62f25d9cbaa788257e5af3f41554760a79185df/src/hifiberrydsp/alsa/alsasync.py#L185-L191>).
-}
amplificationToPercent :: Double -> Percent
amplificationToPercent amp
    | amp <= 0 = 0
    | amp >= 1 = 100
    | otherwise = round $ (log (amp / taperA) / taperB) * 100

-- | Coefficients for a 60dB range - see 'percentToAmplification'.
taperA, taperB :: Double
(taperA, taperB) = (0.001, 6.908)
