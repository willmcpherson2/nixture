{ pkgs ? import <nixpkgs> { } }:
rec {
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
    pkgs.runCommand
      "optimise-haskell-js"
      { buildInputs = [ pkgs.closurecompiler ]; }
      ''
        mkdir -p $out/static
        for dir in $(find ${drv} -name "*.jsexe"); do
          filename="$(basename "$dir" .jsexe).js"
          closure-compiler \
            --warning_level QUIET \
            --compilation_level ADVANCED_OPTIMIZATIONS \
            --jscomp_off=checkVars \
            --externs=$dir/all.js.externs \
            $dir/all.js > $out/static/$filename &
        done
        wait
      '';

  compileNixture = root:
    pkgs.runCommand
      "compile-nixture"
      { buildInputs = [ pkgs.j2cli pkgs.pandoc ]; }
      ''
        mkdir -p $out/static
        cp -r --no-preserve=mode ${root}/* $out/static
        cd $out/static

        for file in $(find ~+ -type f -name "*.org"); do
          pandoc -f org -t html -o "$file".html "$file"
        done

        for file in $(find ~+ -type f -name "*.j2"); do
          no_extension=$(echo "$file" | sed -n "s/\(.*\)\..*/\1/p")
          j2 -o "$no_extension" "$file"
        done
      '';
}
