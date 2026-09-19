{pkgs, ...}:
{
  home.packages = with pkgs; [
    bat
    curl
    dnsutils
    file
    renameutils
    ripgrep
    tmux
    unzip
    vim
  ];
}
