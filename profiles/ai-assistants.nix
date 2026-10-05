{ llm-agents, primaryUser, ...}:
{
  users.users.${primaryUser}.packages = [
    llm-agents.chatgpt
    llm-agents.claude-desktop
  ];
}
