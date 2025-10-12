exts: with exts; [
  akashagarwal.markdown-meaningful-wordcount
  arrterian.nix-env-selector
  asuka.insertnumbers
  brunnerh.insert-unicode
  gruntfuggly.todo-tree
  esbenp.prettier-vscode
  haskell.haskell
  janw4ld.lambda-black
  jnoortheen.nix-ide
  jsynowiec.vscode-insertdatestring
  justusadam.language-haskell
  ms-vsliveshare.vsliveshare
  nwolverson.ide-purescript
  nwolverson.language-purescript
  # works well with just:
  # `nix flake init -t github:ipetkov/crane#quick-start`
  # and add `pkgs.rust-analyzer` to `buildInputs`
  # (should be by default really: https://github.com/ipetkov/crane/discussions/912)
  rust-lang.rust-analyzer
  tamasfe.even-better-toml
]
