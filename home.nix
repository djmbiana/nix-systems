{ config, pkgs, ... }:

let
  dotfiles = "${config.home.homeDirectory}/nix-systems/config";
  create_symlink = path: config.lib.file.mkOutOfStoreSymlink path;
in 

{
  imports = [
    ./modules/applications.nix
  ];

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
   stylua
   ripgrep
   ffmpeg
   nil
   nixpkgs-fmt
   cmake
   libtool
   libvterm
   gnumake
   nodejs
   gcc
   fd
   lsd
   fastfetch
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
          ls = "lsd -Fal";
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
