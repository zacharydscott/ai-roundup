{
  description = "AI Roundup — feed-watching agent built on Conspire";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      hpkgs = pkgs.haskellPackages;
      # The same set kudzu-agent pins, because the Haskell dependency closure is
      # shared through ../conspire: a different zlib or zstd here would mean two
      # incompatible builds of the same libraries in one cabal.project.
      libs = [ pkgs.zlib pkgs.zstd pkgs.xz pkgs.bzip2 ];
    in {
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          hpkgs.ghc
          pkgs.cabal-install
          pkgs.haskell-language-server
          pkgs.ghcid
          # For inspecting the store by hand. The library itself is not needed:
          # direct-sqlite compiles its own copy of the SQLite C source, so the
          # build does not depend on this being here.
          pkgs.sqlite
        ] ++ libs;

        nativeBuildInputs = libs;

        shellHook = ''
          export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath libs}
          export PKG_CONFIG_PATH="${pkgs.lib.makeSearchPathOutput "dev" "lib/pkgconfig" libs}"
          export LIBRARY_PATH="${pkgs.lib.makeLibraryPath libs}"
          export C_INCLUDE_PATH="${pkgs.lib.makeSearchPathOutput "dev" "include" libs}"
        '';
      };
    };
}
