{ lib
, fetchFromGitHub
, SDL2
, makeWrapper
, wget
, nvidia_x11
, gcc
, cudaPackages
}:

gcc.stdenv.mkDerivation rec {
  pname = "whisper-cpp";
  version = "1.4.2";

  src = fetchFromGitHub {
    owner = "ggerganov";
    repo = "whisper.cpp";
    rev = "c7b6988678779901d02ceba1a8212d2c9908956e"; # "f9ca90256bf691642407e589db1a36562c461db7";
    hash = "sha256-hIEIu7feOZWqxRskf6Ej7l653/9KW8B3cnpPLoCRBAc="; # "sha256-Z7f5QcySxJHps/9FMcrw/Hmp+byVyL5I2SD4jUlLrd4=";
  };

  # The upstream download script tries to download the models to the
  # directory of the script, which is not writable due to being
  # inside the nix store. This patch changes the script to download
  # the models to the current directory of where it is being run from.
  # patches = [ ./download-models.patch ];

  nativeBuildInputs = [ makeWrapper ];

  # buildInputs = [ SDL2 cudatoolkit libcublas cudaPackages.cuda_cudart nvidia_x11 gcc ];
  buildInputs =
    [ SDL2 nvidia_x11 gcc

      cudaPackages.cuda_cuobjdump
      cudaPackages.cuda_gdb
      cudaPackages.cuda_nvcc
      cudaPackages.cuda_nvdisasm
      cudaPackages.cuda_nvprune
      cudaPackages.cuda_cccl
      cudaPackages.cuda_cudart
      cudaPackages.cuda_cupti
      cudaPackages.cuda_cuxxfilt
      cudaPackages.cuda_nvml_dev
      cudaPackages.cuda_nvrtc
      cudaPackages.cuda_nvtx
      cudaPackages.cuda_profiler_api
      cudaPackages.cuda_sanitizer_api
      cudaPackages.libcublas
      cudaPackages.libcufft
      cudaPackages.libcurand
      cudaPackages.libcusolver
      cudaPackages.libcusparse
      cudaPackages.libnpp

    ];

  env = {
    WHISPER_CUDA="1";
    EXTRA_LDFLAGS="-L/lib -L${cudaPackages.cuda_cudart.lib}/lib";
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
