{
  services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 0.8;
    backend = "glx";
    fade = true;
    fadeDelta = 3;
    opacityRules = [ "100:name *= 'rofi'" ];
    shadow = true;
    shadowOpacity = 0.75;
    settings = {
      corner-radius = 10;
      blur = {
        background = true;
        method = "gaussian";
        size = 10;
        deviation = 5.0;
      };
    };
  };
}
