{ lib
, fetchFromGitHub
, SDL2
, makeWrapper
, wget
, nvidia_x11
, gcc
, cmake
, ninja
, pkg-config
, cudaPackages
}:

gcc.stdenv.mkDerivation rec {
  pname = "whisper-cpp";
  version = "1.8.0";

  src = fetchFromGitHub {
    owner = "ggerganov";
    repo = "whisper.cpp";
    rev = "8c0855fd6bb115e113c0dca6255ea05f774d35f7";
    hash = "sha256-K/op1nCFDcUzG8bryocuSS/XfLIm+qrzgQYbqOg/YSI=";
  };

  # The upstream download script tries to download the models to the
  # directory of the script, which is not writable due to being
  # inside the nix store. This patch changes the script to download
  # the models to the current directory of where it is being run from.
  # patches = [ ./download-models.patch ];

  nativeBuildInputs = [ makeWrapper cmake ninja pkg-config];

  # buildInputs = [ SDL2 cudatoolkit libcublas cudaPackages.cuda_cudart nvidia_x11 gcc ];
  buildInputs =
    [ SDL2 nvidia_x11 gcc cmake

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
    CUDACXX = "${cudaPackages.cuda_nvcc}/bin/nvcc";
  };
  CC  = "${gcc}/bin/cc";
  CXX = "${gcc}/bin/c++";

  cmakeFlags =
    [ "-DGGML_CUDA=ON"
      "-DWHISPER_BUILD_EXAMPLES=OFF"
      "-DCMAKE_BUILD_TYPE=Release"
      "-DTARGET=build"
      "-DWHISPER_BUILD_TESTS=OFF"
    ];

  outputs = [ "out" "dev" ];

  postInstall =
  ''
  mkdir -p $out/bin
  for b in stream whisper-cli; do
    if [ -x "build/bin/$b" ]; then install -m755 bin/$b $out/bin/; fi
    if [ -x "build/$b" ]; then install -m755 "$b" $out/bin/; fi
  done

  # CUDA runtime wrapper so libcuda.so is found on NixOS:
  libPath=${lib.makeLibraryPath [ cudaPackages.cudatoolkit nvidia_x11 ]}
  for b in $out/bin/*; do
    if [ -x "$b" ] && [ ! -L "$b" ]; then
      wrapProgram "$b" --set LD_LIBRARY_PATH "$libPath"
    fi
  done
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
