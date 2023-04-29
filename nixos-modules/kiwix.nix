{ config, pkgs, ... }:
{
  environment.systemPackages = with pkgs; [ kiwix kiwix-tools ];
}
