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
      icat = "kitty +kitten icat";
    };
    initExtra = ''
      function get_PS1() {
        case $TERM in
          xterm-kitty)
            echo "\n\[\e[0;35m\]\[\e]0;\u@\h:\w\a\]\u@\h:\w λ \[\e[0m\]"
            ;;
          eterm-color)
            echo "\u@\h:\w λ "
            ;;
          *)
            echo "\u@\h:\w $ "
            ;;
        esac
      }
      export PS1="$(get_PS1)"
    '';
  };
}
