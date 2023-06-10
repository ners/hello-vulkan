{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.fir.url = "gitlab:sheaf/fir";
  inputs.fir.flake = false;
  inputs.typelits-witnesses.url = "github:mstksg/typelits-witnesses";
  inputs.typelits-witnesses.flake = false;
  inputs.haskus.url = "github:haskus/packages";
  inputs.haskus.flake = false;
  inputs.bindings-GLFW.url = "github:bsl/bindings-GLFW";
  inputs.bindings-GLFW.flake = false;

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
          bindings-GLFW = pkgs.lib.pipe (super.callCabal2nixWithOptions "bindings-GLFW" inputs.bindings-GLFW "-fsystem-GLFW" { }) [
            (compose.addPkgconfigDepends [ pkgs.glfw-wayland ])
            dontCheck
          ];
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

          pkgs.vulkan-tools
          pkgs.vulkan-loader
          pkgs.vulkan-validation-layers
          pkgs.vulkan-tools-lunarg
        ];
        LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath [
          pkgs.vulkan-tools pkgs.vulkan-loader pkgs.vulkan-validation-layers pkgs.vulkan-tools-lunarg pkgs.vulkan-extension-layer
        ]}";
        VK_LAYER_PATH = "${pkgs.vulkan-validation-layers}/share/vulkan/explicit_layer.d";
        shellHook = ''
          export XDG_DATA_DIRS="${pkgs.vulkan-extension-layer}/share":$XDG_DATA_DIRS
        '';
      };
    };
}
