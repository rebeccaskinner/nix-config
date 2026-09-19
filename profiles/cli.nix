# Core command-line environment.
#
# Everything here should be useful on any machine, headless or not: the
# shell configuration, the terminal multiplexer, and the small set of
# utilities that get reached for constantly. Development tooling lives in
# profiles/development, and anything that needs a display lives in
# profiles/general-desktop.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    bat
    bottom
    curl
    dnsutils
    file
    htop
    rename
    renameutils
    ripgrep
    tmux
    unzip
    vim
    wget
  ];

  home-manager.users.${primaryUser} = {
    imports = [
      ../configs/bash.nix
      ../configs/dircolors.nix
      ../configs/direnv.nix
      ../configs/fzf.nix
      ../configs/gpg.nix
      ../configs/tmux.nix
    ];
  };
}
