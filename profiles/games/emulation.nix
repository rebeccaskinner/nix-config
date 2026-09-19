# Console emulators. RetroArch, with a curated set of libretro cores, covers
# the cartridge and disc systems through the Dreamcast era; the standalone
# emulators cover the consoles where they are clearly better than the core.
{ pkgs, primaryUser, ... }:
let
  retroarch = pkgs.retroarch.withCores (cores: with cores; [
    beetle-pce-fast
    beetle-psx-hw
    beetle-saturn
    dosbox-pure
    flycast
    genesis-plus-gx
    mupen64plus
    nestopia
    snes9x
    stella
  ]);
in
{
  users.users.${primaryUser}.packages = [ retroarch ] ++ (with pkgs; [
    dolphin-emu
    melonDS
    mgba
    pcsx2
    ppsspp
  ]);
}
