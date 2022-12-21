{
  inputs.nixpkgs.url = "nixpkgs/nixos-22.11";
  outputs = { self, nixpkgs }: {
    nixosConfigurations = {
      clark = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [ ./clark.nix ];
      };
    };
  };
}
