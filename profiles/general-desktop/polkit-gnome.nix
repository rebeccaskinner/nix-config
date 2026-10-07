{pkgs, ...}:
{
  systemd.user.services.polkit-gnome-agent = {
    Unit = {
      Description = "Polkit GNOME Authentication Agent";
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1";
      Restart = "on-failure";
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
