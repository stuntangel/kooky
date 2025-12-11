{ config, lib, pkgs, ... }:

let
  cfg = config.programs.kooky;

in
{
  options.programs.kooky = {
    enable = lib.mkEnableOption "Kooky Emacs distribution system-wide";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.kooky;
      description = "The Kooky package to use";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # System fonts for Emacs
    fonts.packages = with pkgs; [
      fira-code
      jetbrains-mono
      (nerdfonts.override { fonts = [ "FiraCode" "JetBrainsMono" ]; })
    ];
  };
}
