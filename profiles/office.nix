# Office suite. LibreOffice finds hunspell dictionaries in the user profile,
# so the English dictionary is installed alongside it for spellcheck.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    hunspellDicts.en_US
    libreoffice
  ];
}
