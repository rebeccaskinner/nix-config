{config, pkgs, ...}:
{
  programs.git = {
    enable = true;
    userEmail = "rebecca@rebeccaskinner.net";
    userName = "rebecca skinner";
    aliases = {
      co = "checkout";
      br = "branch";
      ff = "merge --ff-only";
      st = "status";
    };
    extraConfig = {
      init = {
        defaultBranch = "main";
      };
    };
  };
}
