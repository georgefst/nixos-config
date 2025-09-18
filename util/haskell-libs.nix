hpkgs: with hpkgs; [
  aeson
  aeson-optics
  ansi-terminal
  async
  cassava
  colour
  diagrams-contrib
  diagrams-core
  diagrams-graphviz
  diagrams-lib
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
  monad-loops
  network
  nonempty-containers
  optics
  optics-extra
  optparse-applicative
  optparse-generic
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
  uuid
  vector
  # not working - see unnamed FF tab group
  # oh wtf, this works on Crow but not Fry? guess it must be a hardware thing
  # nope, it works on Fry as well
  # wtf changed from like two days ago? only thing that looks remotely relevant is adding Gloss...
  # keep an eye on it, and I guess when sure it's fine do `gio trash /sync/tmp/GlossNotGloss.hs`
  # or I guess move it back to Fry `hs-scripts`
  # and save the tabs somewhere just in case
  Vis
  wai
  wai-app-static
  warp
  websockets
]
