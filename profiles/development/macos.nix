# Development tools for macOS: the parts of ./cli.nix and ./dev-tools.nix
# that work on darwin. The linux-only debugging and tracing tools (gdb,
# strace, ltrace, elfutils) are left out, as is the wireshark NixOS module;
# wireshark is installed as a plain package instead.
#
# Hosts can set the git author email by passing `gitEmailAddress` through
# specialArgs; otherwise the default in ../../configs/git.nix applies.
{ pkgs, lib, primaryUser, ... }@args:
{
  users.users.${primaryUser}.packages = with pkgs; [
    curl
    file
    gnumake
    httpie
    jq
    man-pages-posix
    pkg-config
    s3cmd
    shellcheck
    wget
    wireshark
  ];

  home-manager.users.${primaryUser} = {
    imports = [
      ../../configs/git.nix
    ];
    programs.git.settings.user.email =
      lib.mkIf (args ? gitEmailAddress) args.gitEmailAddress;
  };
}
