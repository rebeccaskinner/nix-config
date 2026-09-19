# Language-agnostic tools that are useful while writing code: API and
# storage clients, a git GUI, network inspection, and coding agents.
{ pkgs, primaryUser, ... }:
{
  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark;
  };

  users.users.${primaryUser} = {
    extraGroups = [ "wireshark" ];
    packages = with pkgs; [
      claude-code
      gitg
      httpie
      s3cmd
    ];
  };
}
