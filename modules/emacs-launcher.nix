{ config, pkgs, ... }:

let
  emacs-launcher = pkgs.buildGoModule {
    pname = "emacs-launcher";
    version = "1.0.0";
    src = ../scripts;  # Relative path to scripts directory
    vendorHash = null;

    buildPhase = ''
      go build -o emacs-launcher emacs-launcher.go
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp emacs-launcher $out/bin/
    '';
  };
in
{
  home.packages = [
    emacs-launcher
    pkgs.xdotool
  ];
}
