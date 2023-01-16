{ purs-nix, ps-tools, pkgs, system }:

let
  ps-nix = purs-nix { inherit system; };
  ps =
    ps-nix.purs
      { dir = ./.;
        dependencies = [
          "argparse-basic"
          "console"
          "language-cst-parser"
          "node-fs-aff"
          "unordered-collections"
          "profunctor-lenses"
        ];
      };
in
{
  app = ps.app { name = "exact-deps"; };
  shell =
    pkgs.mkShell
      { packages = [
          pkgs.nodejs
          (ps.command{})
          ps-nix.esbuild
          ps-tools.purescript
          ps-tools.purs-tidy
        ];
      };
  inherit (ps) exact-deps;
}
