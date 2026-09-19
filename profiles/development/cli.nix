{pkgs, primaryUser, ...}:
{
  users.users.${primaryUser}.packages = with pkgs; [
    binutils
    curl
    diffutils
    elfutils
    file
    gdb
    gnumake
    jq
    ltrace
    man-pages
    man-pages-posix
    patch
    pkg-config
    shellcheck
    strace
    wget
  ];

  home-manager.users.${primaryUser} = {
    imports = [
      ../../configs/git.nix
    ];
  };
}
