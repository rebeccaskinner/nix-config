{ pkgs, ...}:
let
  fetchDungeonDraft = import ./fetchDungeonDraft.nix { inherit pkgs; };

  dungeonDraft = pkgs.stdenv.mkDerivation {
    name = "DungeonDraft";
    src = fetchDungeonDraft {
      inputFile = ./Dungeondraft-1.0.2.1-Linux64.zip;
      fileSha = "a97de866e5a0514e7492b8c6b228c466e241866746fc4aed6718aa6d691d2880";
    };
    nativeBuildInputs = [
      pkgs.unzip
      pkgs.autoPatchelfHook
    ];
    buildInputs = with pkgs; [
      zlib
      xorg.libXcursor
      xorg.libXinerama
      xorg.libXrandr
      xorg.libXrender
      xorg.libX11
      xorg.libXi
      libpulseaudio
      alsa-lib
      libGL
      stdenv.cc.cc.lib
    ];
    sourceRoot = ".";
    installPhase =
    ''
      target=$out/opt/Dungeondraft
      mkdir -p $target
      mkdir $out/bin
      mkdir -p $out/share/applications
      chmod +x Dungeondraft.x86_64
      cp -av data_Dungeondraft $target/
      cp -av Dungeondraft.x86_64 $target/
      cp -av Dungeondraft.pck $target/
      ln -sf $target/Dungeondraft.x86_64 $out/bin/dungeondraft
      substitute ./Dungeondraft.desktop $out/share/applications/Dungeondraft.desktop --replace "/opt/Dungeondraft" "$out/opt/Dungeondraft"
    '';
  };

in dungeonDraft
