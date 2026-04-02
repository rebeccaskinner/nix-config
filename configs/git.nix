{...}:
{
  programs.git = {
    enable = true;
    lfs.enable = true;
    settings = {
      user = {
        email = "rebecca@rebeccaskinner.net";
        name = "rebecca skinner";
      };
      alias = {
        co = "checkout";
        br = "!git --no-pager branch";
        ff = "merge --ff-only";
        what-changed = "!git --no-pager diff --name-only";
        st = "status";
      };
      init = {
        defaultBranch = "main";
      };
      github = {
        user = "rebeccaskinner";
      };

    };
  };
}
