{ llm-agents, pkgs, primaryUser, ...}:
{
  users.users.${primaryUser}.packages = with llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
    chatgpt
    claude-desktop
  ];
}
