{
  description = "djm's nix configurations";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.11";
    home-manager = {
      url = "github:nix-community/home-manager/release-25.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    
  };
  outputs = { self, nixpkgs, home-manager, ... }: {
    nixosConfigurations.nixbox = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hosts/nixbox/configuration.nix
        
        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.marie = import ./hosts/nixbox/home.nix;
            backupFileExtension = "backup";
          };
        }
      ];
    };

    nixosConfigurations.nixpad = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        ./hosts/nixpad/configuration.nix

        home-manager.nixosModules.home-manager
        {
          home-manager = {
            useGlobalPkgs = true;
            useUserPackages = true;
            users.marie = import ./hosts/nixpad/home.nix;  # or whatever your username is on the laptop
            backupFileExtension = "backup";
          };
        }
      ];
    };
  };
}
