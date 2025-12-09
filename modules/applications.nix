{ pkgs, ... }:

{
  home.packages = with pkgs; [
    gimp
    kdePackages.kdenlive
    cider-2
    winboat
    obs-studio
  ];
}
