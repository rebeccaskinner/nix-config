# Home Manager settings for the primary user on julia. Everything beyond
# identity comes from the profiles imported in ./default.nix.
{ ... }:
{
  programs.home-manager.enable = true;

  home.username = "rebecca";
  home.homeDirectory = "/home/rebecca";

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
