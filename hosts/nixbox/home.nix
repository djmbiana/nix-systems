{ config, pkgs, ... }:

let
  dotfiles = "${config.home.homeDirectory}/nix-systems/config";
  create_symlink = path: config.lib.file.mkOutOfStoreSymlink path;
in 

{
  imports = [
    ../../modules/applications.nix
    ../../modules/emacs-launcher.nix
  ];

  home.username = "marie";
  home.homeDirectory = "/home/marie";
  programs.git.enable = true;
  home.stateVersion = "25.11";

  xdg.configFile."niri" = {
    source = create_symlink "${dotfiles}/niri/";
    recursive = true;
  };

  xdg.configFile."alacritty" = {
    source = create_symlink "${dotfiles}/alacritty/";
    recursive = true;
  };

  xdg.configFile."foot" = {
    source = create_symlink "${dotfiles}/foot/";
    recursive = true;
  };

  xdg.configFile."nvim" = {
    source = create_symlink "${dotfiles}/nvim/";
    recursive = true;
  };
  

 home.packages = with pkgs; [
   neovim
   emacs
   unzip
   stylua
   ripgrep
   ffmpeg
   ffmpegthumbnailer
   pwvucontrol
   nil
   nixpkgs-fmt
   imagemagick
   poppler-utils
   fd
   gnutar
   cmake
   libtool
   libvterm
   vips
   gnumake
   nodejs
   gcc
   fd
   lsd
   fastfetch
   posy-cursors
   ungoogled-chromium
 ];

 programs = {
    zsh = {
        enable = true;
        syntaxHighlighting.enable = true;
        oh-my-zsh = {
            enable = true;
            theme = "";
            plugins = [
                "git"
                "npm"
                "history"
                "node"
                "rust"
                "deno"
            ];
        };
        # Custom shell aliases (must be at the zsh level)
        shellAliases = {
          e = "emacsclient";
          ls = "lsd -Fal";
          nr = "sudo nixos-rebuild switch --flake ~/nix-systems#nixbox";
          nru = "sudo nixos-rebuild switch --flake ~/nix-systems#nixbox --upgrade";
          nrt = "sudo nixos-rebuild switch --flake ~/nix-systems#nixbox --show-trace";
          hmu = "home-manager switch --flake ~/nix-systems#nixbox";
          vim = "nvim";
        };
        sessionVariables = {
          MANPAGER = "nvim +Man!";
          BAT_THEME = "gruvbox-dark";
        };

      initContent = ''
        autoload -U colors && colors
        PROMPT="%B%{$fg[red]%}[%{$fg[yellow]%}%n%{$fg[green]%}@%{$fg[blue]%}%M \
        %{$fg[magenta]%}%~%{$fg[red]%}]%{$reset_color%}$%b "
      '';
    };

    direnv = {
      enable = true;
      enableZshIntegration = true;   # if you use zsh
      nix-direnv.enable = true;
    };
  };
}
