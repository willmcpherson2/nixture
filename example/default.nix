let
  inherit (import <nixpkgs> { }) symlinkJoin;
  inherit (import ./..) ghc ghcjs optimiseHaskellJs compileJinja;
in
symlinkJoin {
  name = "example";
  paths = [
    (ghc.callCabal2nix "server" ./server { })
    (optimiseHaskellJs (ghcjs.callCabal2nix "hs-script" ./client/hs-script { }))
    (compileJinja ./client/index.html)
  ];
}
