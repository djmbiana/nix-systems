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


  gtk = {
    enable = true;
    theme = {
      name = "Arc-Dark";
      package = pkgs.arc-theme;
    };
    iconTheme = {
      name = "Tela-dark";  # You already have tela-icon-theme
      package = pkgs.tela-icon-theme;
    };
    gtk3.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
    gtk4.extraConfig = {
      gtk-application-prefer-dark-theme = 1;
    };
  };


  home.pointerCursor = {
    package = pkgs.bibata-cursors;
    name = "Bibata-Modern-Ice";  # Change to Bibata-Modern-Ice for white cursor
    size = 24;
    gtk.enable = true;
    x11.enable = true;
  };


  xdg.configFile."hypr" = {
    source = create_symlink "${dotfiles}/hypr/";
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
   zip 
   unzip
   p7zip
   file-roller
   stylua
   ripgrep
   mpv
   yt-dlp
   cmatrix
   xfce.thunar-volman
   xfce.thunar-archive-plugin
   xfce.thunar-media-tags-plugin
   arc-theme
   tela-icon-theme
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
   rofi
   gnumake
   nodejs
   gcc
   fd
   lsd
   fastfetch
   bibata-cursors
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
          ns1 = "cd ~/nix-systems/hosts/nixbox/";
          nsc = "cd ~/nix-systems/config/";
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
