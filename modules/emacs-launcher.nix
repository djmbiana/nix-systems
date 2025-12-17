{ config, lib, pkgs, ... }:

{
  options = {};
  
  config = {
    environment.systemPackages = [
      (pkgs.buildGoModule {
        pname = "emacs-launcher";
        version = "1.0.0";
        src = /home/marie/nix-systems/config/scripts;
        vendorHash = null;
        subPackages = [ "." ];
      })
    ];
  };
}
