{ stdenv, fetchgit, haskell, pythonPackages

, autoconf
, automake
, binutils
, coreutils
, cpio
, curl
, gcc5
, gdb
, git
, gmp
, gnugrep
, gnused
, m4
, qemu

, ghc-version ? "ghc801"

, propagate-deps ? false
, use-repo-source ? false
, repo-url ? ""
, repo-commit-id ? ""
, repo-commit-sha256 ? ""
}:

let src-repo = fetchgit {
      url    = repo-url;
      rev    = repo-commit-id;
      sha256 = repo-commit-sha256;
    };
    ghc        = haskell.packages."${ghc-version}".ghcWithPackages
                     (haskellPackages: with haskellPackages; [
			async
			bytestring-trie
			ghc-paths
			ghc-mtl
			haskell-src-exts
			parsec
			random
                     ]);
    deps       = [
	autoconf
	automake
	binutils
	coreutils
	cpio
	curl
	gcc5
	gdb
	git
	gmp
	gnugrep
	gnused
	m4
	qemu

	pythonPackages.pexpect

	# custom GHC
	ghc
    ];
in
stdenv.mkDerivation {
	version = "2016-11-09";
	name    = "barrelfish";
	src     = if use-repo-source
		  then src-repo
		  else builtins.filterSource
		       (path: type: baseNameOf path != ".git" && baseNameOf path != "build")
		       ./.;

	buildInputs = deps;
	propagatedBuildInputs = if propagate-deps
	                        then deps
				else [];

	description = "The Barrelfish OS";
	license = "MIT";
}
