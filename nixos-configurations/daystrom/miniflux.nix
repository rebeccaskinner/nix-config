{ pkgs, config, lib, ... }:
{
  environment.etc."miniflux-temp-password".text = "changeMe!0000?";
  services.miniflux = {
    enable = true;
    adminUsername = "rebecca";
    adminPasswordFile = "/etc/miniflux-temp-password";
    database.createLocally = true;
  };

  ## Stub for automatically adding additional users if necessary
  # systemd.services.miniflux.serviceConfig.ExecStartPost = lib.mkAfter [
  #   (pkgs.writeShellScript "miniflux-create-users" ''
  #     set -euo pipefail
  #     export DATABASE_URL="postgres://miniflux:$(<${config.services.miniflux.database.passwordFile})@localhost/miniflux?sslmode=disable"

  #     create_user() {
  #       if ! ${pkgs.miniflux}/bin/miniflux -list-users | grep -q "^$1 "; then
  #         echo "Creating user: $1"
  #         ${pkgs.miniflux}/bin/miniflux -create-user -username "$1" -password "$2"
  #       else
  #         echo "User $1 already exists"
  #       fi
  #     }

  #     create_user alice "$(cat /run/secrets/miniflux-alice-password)"
  #     create_user bob "$(cat /run/secrets/miniflux-bob-password)"
  #   '')
  # ];
}
