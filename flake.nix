{
  description = "swarm";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nixops-plugged.url = "/home/drsk/nixops-plugged";
  };

  outputs = { self, nixpkgs, flake-utils, nixops-plugged }@inputs:
    let
      domain = "swarmapp.org";
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config = { allowUnfree = true; allowBroken = true; };
        overlays = [ self.overlay ];
      };
      getPatches = dir:
        let files = builtins.attrNames (builtins.readDir dir);
        in map (f: dir + ("/" + f)) files;

      dartVersion = "2.15.0";
      dartSourceBase = "https://storage.googleapis.com/dart-archive/channels";
      dartForFlutter = pkgs.dart.override {
        version = dartVersion;
        sources = {
          "${dartVersion}-x86_64-linux" = pkgs.fetchurl {
            url = "${dartSourceBase}/stable/release/${dartVersion}/sdk/dartsdk-linux-x64-release.zip";
            sha256 = "sha256-U1V1OPmFeNZCdBxFy7yqAbAE0cxh9f2UqKQmreJu9YA=";
          };
        };
      };
    in
    {
      overlay = final: prev: rec {
        myHaskellPackages = prev.haskellPackages.override {
          overrides = hself: hsuper: {
            "morpheus-graphql-subscriptions" =
              hsuper.morpheus-graphql-subscriptions.overrideAttrs (p: {
                buildInputs =
                  p.buildInputs ++ [ hsuper.monad-control hsuper.lifted-base ];
                propagatedBuildInputs =
                  p.propagatedBuildInputs ++ [ hsuper.monad-control hsuper.lifted-base ];
                patches =
                  [ ./nix/morpheus-graphql-subscriptions/monad_base_control.patch ];
              });
            "swarm" =
              hsuper.callCabal2nix
                "swarm"
                (./.)
                { };
          };
        };
        myFlutter = prev.pkgs.flutterPackages.mkFlutter rec {
          dart = dartForFlutter;
          version = "2.8.1";
          pname = "flutter";
          src = prev.pkgs.fetchurl {
            url =
              "https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_2.8.1-stable.tar.xz";
            sha256 = "sha256-R+zcxUgcUaj7Mj8VT4BEyzCdVfqGFKl8ibx8COQ6vgE=";
          };

          patches = getPatches ./nix/flutter/patches;
        };
        site = pkgs.callPackage ./site/site.nix {pkgs=final;};
      };

      site = pkgs.site;

      nixopsConfigurations.default = {
        inherit nixpkgs;
        network.description = domain;
        network.enableRollback = true;
        resources.sshKeyPairs.ssh-key = { };
        defaults.nixpkgs.pkgs = pkgs;
        defaults._module.args = {
          inherit domain;
          swarm = pkgs.myHaskellPackages.swarm;
        };
        backend = import ./deployment/backend.nix;
      };

      devShell.${system} = pkgs.myHaskellPackages.shellFor {
        packages = p: [ p."swarm" ];
        buildInputs = [
          # haskell dev
          pkgs.myHaskellPackages.cabal-install
          pkgs.myHaskellPackages.hindent
          pkgs.myHaskellPackages.ghci
          pkgs.myHaskellPackages.haskell-language-server
          # flutter
          pkgs.myFlutter
          pkgs.insomnia
          pkgs.openjdk
          pkgs.google-chrome-dev
          pkgs.androidStudioPackages.dev
          pkgs.wget
          dartForFlutter
          # firebase
          pkgs.nodePackages.firebase-tools
          # depolyment
          nixops-plugged.defaultPackage.${system}
          pkgs.nodePackages.node2nix
          pkgs.git-crypt
          # graphics/site
          pkgs.nodejs
          pkgs.inkscape
          pkgs.montserrat
          pkgs.imagemagick
          # debug
          pkgs.wireshark
          # nix
          pkgs.rnix-lsp
        ];
        withHoogle = true;
        shellHook = ''
          export ANDROID_JAVA_HOME=${pkgs.jdk.home}
          export ANDROID_SDK_ROOT=~/Android/Sdk/
          export FLUTTER_SDK=${pkgs.myFlutter.unwrapped}
          export CHROME_EXECUTABLE=${pkgs.google-chrome-dev}/bin/google-chrome-unstable
          export _JAVA_AWT_WM_NONREPARENTING=1
        '';
      };

      defaultPackage.${system} = pkgs.myHaskellPackages.swarm;
    };
}
