let
  inherit (import <nixpkgs> { }) symlinkJoin;
  inherit
    (import
      (builtins.fetchTarball
        "https://github.com/willmcpherson2/nixture/archive/main.tar.gz"))
    ghc ghcjs optimiseHaskellJs compileJinja;
in
symlinkJoin {
  name = "example";
  paths = [
    (ghc.callCabal2nix "server" ./server { })
    (optimiseHaskellJs (ghcjs.callCabal2nix "hs-script" ./client/hs-script { }))
    (compileJinja ./client/index.html.j2)
  ];
}
