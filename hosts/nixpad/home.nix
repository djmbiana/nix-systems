{ config, pkgs, ... }:

let
  dotfiles = "${config.home.homeDirectory}/nix-systems/config";
  create_symlink = path: config.lib.file.mkOutOfStoreSymlink path;
in 

{
  home.username = "marie";
  home.homeDirectory = "/home/marie";
  programs.git.enable = true;
  home.stateVersion = "25.11";

  xdg.configFile."alacritty" = {
    source = create_symlink "${dotfiles}/alacritty/";
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
   file-roller
   stylua
   ripgrep
   mpv
   yt-dlp
   cmatrix
   pipes
   btop
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
          nr = "sudo nixos-rebuild switch --flake ~/nix-systems#nixpad";
          nru = "sudo nixos-rebuild switch --flake ~/nix-systems#nixpad --upgrade";
          nrt = "sudo nixos-rebuild switch --flake ~/nix-systems#nixpad --show-trace";
          hmu = "home-manager switch --flake ~/nix-systems#nixpad";
          vim = "nvim";
          ns1 = "cd ~/nix-systems/hosts/nixpad";
          nsc = "cd ~/nix-systems/config/";
          ff = "fastfetch";
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
