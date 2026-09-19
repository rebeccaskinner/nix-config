# Applications and settings for any machine with a display, independent of
# which desktop environment it runs: terminal, browsers, communication,
# desktop audio control, everyday image and document tools, and fonts.
# Media playback and encoding live in profiles/multimedia; editing-heavy
# tools like office suites and TeX have their own profiles.
{ pkgs, primaryUser, ... }:
let
  browsers = with pkgs; [
    firefox
  ];

  communication = with pkgs; [
    signal-desktop
    slack
    thunderbird
  ];

  audio = with pkgs; [
    alsa-utils
    pamixer
    pavucontrol
    pulsemixer
  ];

  imagesAndDocuments = with pkgs; [
    drawio
    evince
    gimp
    graphicsmagick
    inkscape
    mat2
    qiv
    scrot
  ];

  utilities = with pkgs; [
    anki
    baobab
    kazam
    kiwix
    kiwix-tools
  ];

  nerdFonts = with pkgs.nerd-fonts; [
    liberation
    noto
    roboto-mono
    symbols-only
    terminess-ttf
    ubuntu
    ubuntu-mono
    ubuntu-sans
  ];

  standardFonts = with pkgs; [
    aegyptus
    dina-font
    fira-code
    fira-code-symbols
    font-awesome
    google-fonts
    hasklig
    liberation_ttf
    material-design-icons
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-color-emoji
    proggyfonts
    sigi
    source-code-pro
    symbola
  ];
in
{
  fonts = {
    packages = nerdFonts ++ standardFonts;
    fontconfig = {
      enable = true;
      antialias = true;
    };
  };

  users.users.${primaryUser}.packages = builtins.concatLists [
    browsers
    communication
    audio
    imagesAndDocuments
    utilities
  ];

  home-manager.users.${primaryUser} = {
    imports = [
      ../configs/chromium.nix
      ../configs/kitty.nix
      ../configs/nextcloud-client.nix
      ../configs/polkit-gnome.nix
    ];

    home.sessionVariables.GTK_THEME = "Adwaita:dark";
  };
}
