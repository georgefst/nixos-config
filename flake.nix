{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    clark.url = "path:./clark/haskell";
    tennis-scraper.url = "/home/gthomas/code/tennis-scraper";
  };
  outputs = { self, nixpkgs, clark, tennis-scraper }: rec {
    nixosConfigurations = {
      clark = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./clark.nix
        ];
        specialArgs = {
          extraPkgs = {
            clark = clark.packages.aarch64-linux.default;
            tennis-scraper = tennis-scraper.packages.aarch64-linux.default;
          };
        };
      };
    };
    images.clark = nixosConfigurations.clark.config.system.build.sdImage;
  };
}
