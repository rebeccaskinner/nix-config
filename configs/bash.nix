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
      emacs-client = "emacsclient -nw";
      icat = "kitty +kitten icat";
      kitty-ssh = "kitty +kitten ssh";
      vim = "nvim";
    };
    initExtra = ''
      function get_github_url() {
          githubURL=$(git remote get-url origin | sed "s/git@github.com:/https:\/\/github.com\//g" | sed 's/.git$//g')
          echo "$githubURL"
      }

      function get_nix_sha256_from_head() {
          rev=$(git rev-parse HEAD)
          githubURL=$(get_github_url)
          sha=$(nix-prefetch-url --unpack "$githubURL/archive/$rev.tar.gz")
          echo "rev = \"$rev\""
          echo "sha256 = \"$sha\""
      }

      function get_PS1() {
        case $TERM in
          tmux-256color | xterm-256color | xterm-kitty)
            echo "\n\[\e[0;35m\]\[\e]0;\u@\h:\w\a\]\u@\h:\w λ \[\e[0m\]"
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
