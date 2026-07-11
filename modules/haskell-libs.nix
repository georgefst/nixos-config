pkgs: hpkgs: with hpkgs; [
  aeson
  aeson-optics
  ansi-terminal
  async
  blaze-html
  blaze-markup
  cassava
  clay
  colour
  diagrams-contrib
  diagrams-core
  diagrams-graphviz
  diagrams-lib
  diagrams-rasterific
  diagrams-svg
  evdev
  exceptions
  extra
  fgl
  file-embed
  file-io
  fsnotify
  generic-optics
  gloss
  graphviz
  http-client-tls
  JuicyPixels
  lens
  lifx-lan
  linear
  lucid2
  massiv
  megaparsec
  monad-bayes
  monad-loops
  neat-interpolation
  network
  nonempty-containers
  openapi3
  optics
  optics-extra
  optparse-applicative
  optparse-generic
  pandoc
  pandoc-types
  pretty-simple
  prettyprinter
  prettyprinter-graphviz
  prettyprinter-lucid
  process-extras
  random
  safe
  servant
  servant-client
  servant-server
  shake
  streamly
  tagsoup
  uuid
  vector
  Vis
  # also, given that all this stuff is _largely_ for `hs-scripts`, I should think about alternatives there
  # how easy would it be to generate a GHC environment file pointing to paths in Nix store?
  # seems a bit icky, but no worse for stability or usability than using one on non-Nix
  # although actually, I do use the global packages elsewhere, e.g. for non-VCS-ed scripts in sync folder
  # webcolor-labels
  wai
  wai-app-static
  warp
  # https://github.com/georgefst/colour-parsers (not on Hackage)
  (pkgs.haskell.lib.dontCheck
    (callCabal2nix "colour-parsers"
      (pkgs.fetchFromGitHub {
        owner = "georgefst";
        repo = "colour-parsers";
        rev = "57ee42e3bebb461a2a9cc0e1bd8c23b648d95147";
        sha256 = "nZSRNKL/A05yS401RP8tvcG2Ms4uNtYxoUjj15QaGIE=";
      })
      { }
    )
  )
  # https://github.com/lexi-lambda/freer-simple/pull/45
  (callCabal2nix "freer-simple"
    (pkgs.fetchFromGitHub {
      owner = "georgefst";
      repo = "freer-simple";
      rev = "e1d88c1ee036115ef527bda8c66da997962b3f34";
      sha256 = "/AnRoCx5IRf9Q8+fLk+Wilo16LNxhRxYvCLkuBIWIy0=";
    })
    { }
  )
  # https://github.com/haskell-game/webcolor-labels/issues/3
  (callCabal2nix "webcolor-labels"
    (pkgs.fetchFromGitHub {
      owner = "haskell-game";
      repo = "webcolor-labels";
      rev = "279a8ef59f0dd2c77f51d606315d90431d548db0";
      sha256 = "Y0o+MBSuzgCRtgYc+Vo3zt0GqWW6jjtGwqG/IB6LuYA=";
    })
    { }
  )
]
