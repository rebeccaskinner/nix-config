{
  services.picom = {
    enable = true;
    activeOpacity = "1.0";
    inactiveOpacity = "0.8";
    backend = "glx";
    fade = true;
    fadeDelta = 3;
    opacityRule = [ "100:name *= 'rofi'" ];
    shadow = true;
    shadowOpacity = "0.75";
  };
}
