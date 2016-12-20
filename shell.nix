# How to employ Nix to compose/deploy the build environment:
#
# 1. Get derivation path:
#
# $ STOREDRVPATH=$(nix-instantiate --no-build-output --cores 4 -E 'import ./shell.nix { closure-generation-mode = true; repo-commit-id="commit-id"; repo-commit-sha256="nix-hash"; }')
#
# 2. Export closure on the source host:
#
# $ nix-store --export $(nix-store --query --requisites --include-outputs ${STOREDRVPATH}) > ~/barrelfish.closure
#
# 3. Import closure on the target host (and transfer the value of STOREDRVPATH):
#
# $ nix-store --import barrelfish.closure
#
# 4. Enter shell :
#
# $ nix-shell ${STOREDRVPATH}

{ nixpkgs ? import <nixpkgs> {}
, ghc-version ? "ghc801"

, closure-generation-mode ? false
, propagate-deps ? closure-generation-mode
, use-repo-source ? closure-generation-mode
, repo-url ? ""
, repo-commit-id ? ""
, repo-commit-sha256 ? ""
}:

let

  inherit (nixpkgs) pkgs;

  f = import ./.; # your default.nix

in

  pkgs.callPackage f {
    haskell = pkgs.haskell;

    inherit ghc-version;
    inherit propagate-deps use-repo-source repo-url repo-commit-id repo-commit-sha256;
  }
