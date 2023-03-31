let
  inherit (import <nixpkgs> { }) pkgs runCommand;
in
rec {
  inherit (pkgs) closurecompiler pandoc j2cli;

  reflex =
    import
      (builtins.fetchTarball
        "https://github.com/reflex-frp/reflex-platform/archive/123a6f487ca954fd983f6d4cd6b2a69d4c463d10.tar.gz")
      { };

  ghc = reflex.ghc8_6;

  ghcjs = reflex.ghcjs8_6;

  haskell-language-server =
    (import
      (builtins.fetchTarball
        "https://github.com/NixOS/nixpkgs/archive/0756b8a7bf25e7a5b4fbfa578e95a28cb5ef6380.tar.gz")
      { }).haskell-language-server.override { supportedGhcVersions = [ "865" ]; };

  optimiseHaskellJs = drv:
    runCommand
      "optimise-haskell-js"
      { buildInputs = [ closurecompiler ]; }
      ''
        mkdir -p $out
        for dir in $(find ${drv} -name "*.jsexe"); do
          filename="$(basename "$dir" .jsexe).js"
          closure-compiler \
            --warning_level QUIET \
            --compilation_level ADVANCED_OPTIMIZATIONS \
            --jscomp_off=checkVars \
            --externs=$dir/all.js.externs \
            $dir/all.js > $out/$filename &
        done
        wait
      '';

  compileNixture = root:
    runCommand
      "compile-nixture"
      { buildInputs = [ j2cli pandoc ]; }
      ''
        mkdir -p $out
        cp -r ${root}/* $out
        cd $out

        for file in $(find . -type f -name "*.org"); do
          pandoc -f org -t html -o "$file".html "$file" 
        done

        for file in $(find ~+ -type f -name "*.j2"); do
          no_extension=$(echo "$file" | sed -n "s/\(.*\)\..*/\1/p")
          j2 -o "$no_extension" "$file"
        done
      '';
}
