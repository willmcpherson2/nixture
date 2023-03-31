let
  inherit (import <nixpkgs> { }) pkgs lib runCommand writeTextDir symlinkJoin;
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

  compileOrg = file: outPath:
    runCommand
      "compile-org"
      { buildInputs = [ pandoc ]; }
      ''
        mkdir -p "$(dirname "$out/${outPath}")"
        pandoc -f org -t html ${file} > $out/${outPath}
      '';

  compileJinjaInput = root: file:
    let
      dir = builtins.dirOf file;
      nameAndExtension = builtins.match "([^\.]*)\.(.*)" (baseNameOf file);
      name = builtins.elemAt nameAndExtension 0;
      extension = builtins.elemAt nameAndExtension 1;
      pathFromRoot = file:
        builtins.substring
          (builtins.stringLength (toString root) + 1)
          (builtins.stringLength (toString file))
          (toString file);
      cases = {
        org = compileOrg file (pathFromRoot (dir + "/${name}.html"));
      };
      plain = writeTextDir (pathFromRoot file) (builtins.readFile file);
      results =
        if builtins.isList nameAndExtension
          && builtins.length nameAndExtension == 2
          && cases ? "${extension}"
        then [ cases."${extension}" ]
        else [ ];
    in
    results ++ [ plain ];

  compileJinjaInputs = dir:
    symlinkJoin {
      name = "jinja-inputs";
      paths =
        builtins.concatMap
          (compileJinjaInput dir)
          (lib.filesystem.listFilesRecursive dir);
    };

  compileJinja = template:
    let
      dir = builtins.dirOf template;
      outName = lib.strings.removeSuffix ".j2" (builtins.baseNameOf template);
      jinja-inputs = compileJinjaInputs dir;
    in
    runCommand
      "compile-jinja"
      { buildInputs = [ j2cli ]; }
      ''
        mkdir -p $out
        cp -r ${jinja-inputs}/* .
        j2 ${template} > $out/${outName}
      '';
}
