# General-purpose command-line development tools and git.
#
# Hosts can set the git author email by passing `gitEmailAddress` through
# specialArgs; otherwise the default in ../../configs/git.nix applies.
{ pkgs, lib, primaryUser, ... }@args:
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
    programs.git.settings.user.email =
      lib.mkIf (args ? gitEmailAddress) args.gitEmailAddress;
  };
}
