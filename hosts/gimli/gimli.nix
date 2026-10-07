# Home Manager settings for the primary user on gimli. Most of the
# environment comes from the profiles imported in ./default.nix; what is
# left here is mac-specific.
{ pkgs, primaryUser, ... }:
let
  utils = import ../../utils;
  nvim = import ../../development-environment/nvim { inherit pkgs utils; };
in
{
  programs.home-manager.enable = true;

  home.username = primaryUser;
  home.homeDirectory = "/Users/${primaryUser}";

  imports = nvim.imports;

  # profiles.emacs owns EDITOR.
  programs.neovim.defaultEditor = pkgs.lib.mkForce false;

  programs.bash = {
    shellAliases.vim = "nvim";
    bashrcExtra = ''
      PATH=/opt/homebrew/bin:/opt/homebrew/sbin:''${PATH};
      [ -z "''${MANPATH-}" ] || export MANPATH=":''${MANPATH#:}";
      export INFOPATH="/opt/homebrew/share/info:''${INFOPATH:-}";
    '';
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";
}
