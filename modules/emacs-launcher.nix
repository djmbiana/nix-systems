{ config, lib, pkgs, ... }:

{
  options = {};
  
  config = {
    home.packages = [
      (pkgs.buildGoModule {
        pname = "emacs-launcher";
        version = "1.0.0";
        src = ../config/scripts/emacs-launcher.go;
        vendorHash = null;
        subPackages = [ "." ];
      })
    ];
  };
}
