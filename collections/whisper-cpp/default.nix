{ lib
, fetchFromGitHub
, SDL2
, makeWrapper
, wget
, cudatoolkit
, nvidia_x11
, gcc
}:

gcc.stdenv.mkDerivation rec {
  pname = "whisper-cpp";
  version = "1.4.2";

  src = fetchFromGitHub {
    owner = "ggerganov";
    repo = "whisper.cpp";
    rev = "f9ca90256bf691642407e589db1a36562c461db7";
    hash = "sha256-Z7f5QcySxJHps/9FMcrw/Hmp+byVyL5I2SD4jUlLrd4=";
  };

  # The upstream download script tries to download the models to the
  # directory of the script, which is not writable due to being
  # inside the nix store. This patch changes the script to download
  # the models to the current directory of where it is being run from.
  # patches = [ ./download-models.patch ];

  nativeBuildInputs = [ makeWrapper ];

  buildInputs = [ SDL2 cudatoolkit nvidia_x11 gcc ];

  preBuild = ''
    export CUDA_PATH=${cudatoolkit};
    export WHISPER_CUBLAS=1;
  '';

  env = {
    EXTRA_LDFLAGS="-L/lib -L${nvidia_x11}/lib";
    EXTRA_CCFLAGS="-I/usr/include";
  };

  makeFlags = [ "main" "stream" ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp ./main $out/bin/whisper-cpp
    cp ./stream $out/bin/whisper-cpp-stream

    runHook postInstall
  '';

  meta = with lib; {
    description = "Port of OpenAI's Whisper model in C/C++";
    longDescription = ''
      To download the models as described in the project's readme, you may
      use the `whisper-cpp-download-ggml-model` binary from this package.
    '';
    homepage = "https://github.com/ggerganov/whisper.cpp";
    license = licenses.mit;
    platforms = platforms.all;
    maintainers = with maintainers; [ dit7ya hughobrien ];
  };
}
