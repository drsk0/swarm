{ config, lib, pkgs, ... }:

with lib;
let
  projectConfig = import ./config.nix;
  cfg = config.services.swarm;
in
{
  options = {
    services.swarm = {

      package = mkOption {
        type = types.package;
        default = pkgs.swarm;
        defaultText = "pkgs.swarm";
        description = ''
          The swarm backend server.
        '';
      };

      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to run the swarm backend.
        '';
      };

      port = mkOption {
        type = types.port;
        default = 8000;
        description = ''
          The port where the swarm API will be served.
        '';
      };

      serviceAccountJsonPath = mkOption {
        type = types.nullOr types.path;
        default = "/run/keys/service-account-json";
        example = "/run/keys/service-account-json";
        description = ''
          Path to firebase service account .json file.
        '';
        };

    };
  };

  config = mkIf cfg.enable {

    # swarm user
    users.users.swarm = {
      isSystemUser = true;
      description = "The swarm system user";
      extraGroups = [ "swarm" "wheel" ];
    };
    users.groups.swarm = {
    };

    systemd.services.swarm =
      {
        description = "swarm backend server";
        wantedBy = [ "multi-user.target" ];
        wants = [ ];
        after = [ "network.target" ];
        serviceConfig =
          {
            User = "swarm";
            Group = "swarm";
            ExecStart = ''
              ${cfg.package}/bin/swarm \
                --port=${toString cfg.port} \
                --db=/run/swarm/db \
                --access-log=/var/log/swarm/access.log \
                --error-log=/var/log/swarm/error.log \
                --service-account-file=${cfg.serviceAccountJsonPath}
            '';
            # Restart = "on-failure";
            # RestartSec = "2s";
            LogsDirectory = "swarm";
            RuntimeDirectory = ["swarm" "keys"];
            RuntimeDirectoryPreserve = true;

            # Shut down swarm using SIGINT
            KillSignal = "SIGINT";
            KillMode = "mixed";

            # Give swarm a decent amount of time to clean up after
            # receiving systemd's SIGINT.
            TimeoutSec = 20;
          };
      };
  };
}
