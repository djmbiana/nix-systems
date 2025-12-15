{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gimp
    kdePackages.kdenlive
    cider-2
    libreoffice-qt
    zoom-us
    obs-studio
  ];
}
