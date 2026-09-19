# Steam. The NixOS module handles the 32-bit libraries, firewall rules for
# remote play and local transfers, and the FHS environment games expect.
{ ... }:
{
  programs.steam.enable = true;
}
