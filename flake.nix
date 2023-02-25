{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    tennis-scraper.url = "/home/gthomas/code/tennis-scraper";
  };
  outputs = { self, nixpkgs, tennis-scraper }: rec {
    nixosConfigurations = {
      clark = nixpkgs.lib.nixosSystem {
        system = "aarch64-linux";
        modules = [
          "${nixpkgs}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"
          ./clark.nix
        ];
        specialArgs = {
          extraPkgs = {
            tennis-scraper = tennis-scraper.packages.aarch64-linux.default;
          };
        };
      };
    };
    images.clark = nixosConfigurations.clark.config.system.build.sdImage;
  };
}
