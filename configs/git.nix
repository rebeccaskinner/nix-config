{...}:
{
  programs.git = {
    enable = true;
    lfs.enable = true;
    userEmail = "rebecca@rebeccaskinner.net";
    userName = "rebecca skinner";
    aliases = {
      co = "checkout";
      br = "!git --no-pager branch";
      ff = "merge --ff-only";
      what-changed = "!git --no-pager diff --name-only";
      st = "status";
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
      github = {
        user = "rebeccaskinner";
      };
    };
  };
}
