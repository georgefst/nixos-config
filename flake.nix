{
  inputs.nixpkgs.url = "nixpkgs/nixos-22.11";
  outputs = { self, nixpkgs }: rec {
    nixosConfigurations = {
      clark = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./clark.nix
        ];
      };
    };
    images.clark = nixosConfigurations.clark.config.system.build.sdImage;
  };
}
