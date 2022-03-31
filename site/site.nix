{ pkgs, stdenv, ... }:

let
  nodeDependencies = (pkgs.callPackage ./default.nix {inherit pkgs;}).shell.nodeDependencies;
in

stdenv.mkDerivation {
  name = "swarm-site";
  src = ./.;
  buildInputs = [pkgs.nodejs];
  installPhase = ''
    mkdir -p $out/dist/
    ln -fs ${nodeDependencies}/lib/node_modules ./node_modules
    export PATH="${nodeDependencies}/bin:$PATH"
    export NODE_ENV=production
    cp -r public/* $out/
    postcss ./src/styles.css -o $out/dist/styles.css
  '';
}
