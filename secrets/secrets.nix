let
  keys = builtins.concatLists
    (map (d: builtins.attrValues (d.ssh or { }))
      (builtins.attrValues (import ../nix/devices.nix)));
in
builtins.listToAttrs (map
  (s: {
    name = s + ".age";
    value.publicKeys = keys;
  }) [
  "wifi"
  "github.key"
  "mailgun.key"
  "mailgun.sandbox"
  "gather.id"
])
