{
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  inputs.fir = {
    url = "gitlab:sheaf/fir";
    flake = false;
  };
  inputs.typelits-witnesses = {
    url = "github:mstksg/typelits-witnesses";
    flake = false;
  };
  inputs.haskus = {
    url = "github:haskus/packages";
    flake = false;
  };
  inputs.bindings-GLFW = {
    url = "github:bsl/bindings-GLFW";
    flake = false;
  };
  inputs.eff = {
    url = "github:lexi-lambda/eff/ghc-9.6";
    flake = false;
  };

  outputs = inputs:
    let
      lib = inputs.nixpkgs.lib;
      attrsToList = with lib; mapAttrsToList nameValuePair;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (map f xs);
      foreachAttrs = attrs: f: with lib; pipe attrs [
        attrsToList
        (xs: foreach xs ({ name, value }: f name value))
      ];
    in
    foreachAttrs inputs.nixpkgs.legacyPackages (system: pkgs:
      let
        haskellPackages = pkgs.haskell.packages.ghc96.override {
          overrides = self: super: with pkgs.haskell.lib; {
            mwe = self.callCabal2nix "mwe" ./. { };
            fir = dontCheck (self.callCabal2nix "fir" inputs.fir { });
            eff = disableLibraryProfiling (self.callCabal2nix "eff" "${inputs.eff}/eff" { });
            haskus-utils-variant = dontCheck (self.callCabal2nix "haskus-utils-variant" "${inputs.haskus}/haskus-utils-variant" { });
            typelits-witnesses = self.callCabal2nix "typelits-witnesses" inputs.typelits-witnesses { };
            bsb-http-chunked = dontCheck (super.bsb-http-chunked);
            fourmolu = super.fourmolu_0_12_0_0;
            bindings-GLFW = pkgs.lib.pipe inputs.bindings-GLFW [
              (x: self.callCabal2nixWithOptions "bindings-GLFW" x "-fsystem-GLFW" { })
              (compose.addPkgconfigDepends (with pkgs; [
                glfw-wayland
                wayland.dev
                libffi
              ]))
              dontCheck
            ];
          };
        };
      in

      {
        formatter.${system} = pkgs.nixpkgs-fmt;
        packages.${system}.default = haskellPackages.mwe;
        devShells.${system}.default = haskellPackages.shellFor {
          packages = ps: with ps; [ mwe ];
          nativeBuildInputs = with haskellPackages; [
            cabal-install
            fourmolu
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
          #withHoogle = true;
        };
      });
}
