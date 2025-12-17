{ lib, buildGoModule }:

buildGoModule {
  pname = "emacs-launcher";
  version = "1.0.0";
  
  src = /home/marie/nix-systems/config/scripts;
  
  vendorHash = null;
  
  subPackages = [ "." ];
  
  meta = with lib; {
    description = "Hyprland-aware Emacs command launcher";
    license = licenses.mit;
  };
}
