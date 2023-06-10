{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.fir.url = "gitlab:sheaf/fir";
  inputs.fir.flake = false;
  inputs.typelits-witnesses.url = "github:mstksg/typelits-witnesses";
  inputs.typelits-witnesses.flake = false;
  inputs.haskus.url = "github:haskus/packages";
  inputs.haskus.flake = false;

  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      haskellPackages = pkgs.haskell.packages.ghc92.override {
        overrides = self: super: with pkgs.haskell.lib; {
          mwe = super.callCabal2nix "mwe" ./. { };
          fir = dontCheck (super.callCabal2nix "fir" inputs.fir { });
          haskus-utils-variant = dontCheck (super.callCabal2nix "haskus-utils-variant" "${inputs.haskus}/haskus-utils-variant" { });
          typelits-witnesses = super.callCabal2nix "typelits-witnesses" inputs.typelits-witnesses { };

        };
      };
    in
    {
      packages.${system}.default = haskellPackages.mwe;
      devShells.${system}.default = haskellPackages.shellFor {
        packages = ps: with ps; [ mwe ];
        nativeBuildInputs = with haskellPackages; [
          cabal-install
          haskell-language-server
        ];
      };
    };
}
