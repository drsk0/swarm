{ config, pkgs, lib, domain, swarm, ... }:
let
  projectConfig = import ./config.nix;
  inherit (lib) fileContents;
in
{

  # deployment on digital ocean
  deployment = {
    targetEnv = "droplet";
    droplet.enableIpv6 = true;
    droplet.region = projectConfig.serverRegion;
    droplet.size = projectConfig.serverSize;
    keys = {
      service-account-json = {
        text = fileContents ../secrets/swarm-90059-firebase-adminsdk-s0gjo-d9f876cb1e.json;
        user = "swarm";
        group = "swarm";
        permissions = "0640";
      };
      acme-dns-creds = {
        text = fileContents ../secrets/godaddy-api-creds;
        user = "acme";
        group = "acme";
        permissions = "0640";
      };
    };
  };

  environment.systemPackages = [ pkgs.neovim pkgs.zenith pkgs.tmux ];
  networking.firewall.allowedTCPPorts = [ 22 443 ];
  # networking.firewall.allowedUDPPortRanges = [ {from=0; to=65535;} ];

  imports = [./swarm-service.nix];

  # ssh
  services.openssh = {
    enable = true;
    challengeResponseAuthentication = false;
    passwordAuthentication = true; # originally true
    permitRootLogin = "yes";
  };

  users.groups.acme.members = [ "nginx" ];
  users.users.swarm.group = "swarm";
  users.groups.swarm = {};

  security.acme = {
    acceptTerms = true;
    email = "admin+acme@${domain}";
    certs."${domain}" = {
      domain = "*.${domain}";
      extraDomainNames = [ domain ];
      dnsProvider = "godaddy";
      credentialsFile = "/run/keys/acme-dns-creds";
      dnsPropagationCheck = true;
    };
  };

  # nginx
  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedTlsSettings = true;
    virtualHosts."${domain}" = {
      useACMEHost = domain;
      forceSSL = true;
      root = config.nixpkgs.pkgs.site;
      locations."/api/query" = {
        proxyPass = "http://127.0.0.1:8000";
        # extraConfig = ''
          # proxy_read_timeout 300s;
          # proxy_connect_timeout 75s;
        # '';
      };
      locations."/api/subscribe" = {
        proxyWebsockets = true;
        proxyPass = "http://127.0.0.1:8000";
        # extraConfig = ''
          # proxy_read_timeout 300s;
          # proxy_connect_timeout 75s;
        # '';
        # extraConfig = ''
          # proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          # proxy_set_header Host $host;

          # proxy_pass_header Authorization;
          # proxy_pass_header Swarm;

          # proxy_http_version 1.1;
          # proxy_set_header Connection "upgrade";
          # proxy_set_header Upgrade $http_upgrade;
        # '';
      };
      # extraConfig = "error_page 404 /404.html;";
    };
  };

  # fail2ban
  services.fail2ban.enable=true;
      services.fail2ban.jails.ssh-iptables2 = ''
        filter   = sshd[mode=aggressive]
        action = iptables-multiport[name=SSH, port="22", protocol=tcp]
        maxretry = 10
        '';
      services.fail2ban.jails.nginx-botsearch = ''
        filter   = nginx-botsearch
        action = iptables-multiport[name=NGINXBOT, port="443", protocol=tcp]
        '';
      services.fail2ban.jails.nginx-http-auth = ''
        filter   = nginx-http-auth
        action = iptables-multiport[name=NGINXAUTH, port="443", protocol=tcp]
        '';

    services.swarm = {
      enable = true;
      port = 8000;
      serviceAccountJsonPath = "/run/keys/service-account-json";
      package = swarm;
    };
}
