{ primaryUser
, ... }:
{
  programs.home-manager.enable = true;
  home.username = "${primaryUser}";
  home.homeDirectory = "/home/${primaryUser}";
  home.stateVersion = "24.11";
}
