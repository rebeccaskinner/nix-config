{config, pkgs, ...}:
{
  programs.bash = {
    enable = true;
    enableVteIntegration = true;
    shellAliases = {
      ll = "ls -alF";
      la = "ls -A";
      ls = "ls --color=tty --classify";
      gitk = "gitk --all &";
      qiv = "qiv -t -I";
      emacs = "emacsclient -nw";
    };
    initExtra = ''
      export PS1="\n\033[0;35m\]\u@\h:\w λ \[\033[0m\]"
    '';
  };
}
