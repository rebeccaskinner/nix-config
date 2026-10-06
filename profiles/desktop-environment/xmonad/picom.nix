{
  services.picom = {
    enable = true;
    activeOpacity = 1.0;
    inactiveOpacity = 1.0;
    backend = "glx";
    fade = true;
    fadeDelta = 3;
    opacityRules = [ "100:name *= 'rofi'" ];
    shadow = true;
    shadowOpacity = 0.75;
    settings = {
      corner-radius = 10;
      blur = {
        background = false;
        method = "gaussian";
        size = 10;
        deviation = 5.0;
      };
    };
  };
}
