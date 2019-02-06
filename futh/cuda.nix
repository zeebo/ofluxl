{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "cuda-env-shell";
  buildInputs = with pkgs; [
    autoconf
    binutils
    cudatoolkit
    curl
    freeglut
    git
    gitRepo
    gnumake
    gnupg
    gperf
    libGLU_combined
    linuxPackages.nvidia_x11
    m4
    ncurses5
    procps
    stdenv.cc
    unzip
    utillinux
    xorg.libX11
    xorg.libXext
    xorg.libXi
    xorg.libXmu
    xorg.libXrandr
    xorg.libXv
    zlib
  ];
  shellHook = ''
    export CUDA_PATH=${pkgs.cudatoolkit}
    export EXTRA_LDFLAGS="-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib"
    export EXTRA_CCFLAGS="-I/usr/include"
  '';
}