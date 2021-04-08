{ compiler ? "ghc8104", sources ? import ./nix/sources.nix }:

let
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  hpkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: {
      "do-spaces" = self.callCabal2nix "do-spaces" (gitignore ./.) {};
    };
  };

  shell = hpkgs.shellFor {
    packages = p: [ p."do-spaces" ];
    buildInputs = with pkgs.haskellPackages; [
      (
        pkgs.haskell-language-server.override { supportedGhcVersions = [ "8104" ]; }
      )
      cabal-install
      hlint
      floskell
      implicit-hie
      cabal-fmt
    ];
    withHoogle = false;
  };

in
{
  inherit shell;
  "do-spaces" = hpkgs."do-spaces";
}
