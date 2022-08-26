{ pkgs, ghc, bootstrapFile }:
let
  hackage = "https://hackage.haskell.org";

  fetchHackageSdist = { package, version, sha256 }:
    builtins.fetchurl {
      inherit sha256;
      name = "sdist-${package}-${version}";
      url =
        "${hackage}/package/${package}-${version}/${package}-${version}.tar.gz";
    };

  fetchHackageCabal = { package, version, revision, sha256 }:
    builtins.fetchurl {
      inherit sha256;
      name = "cabal-file-${package}-${version}-revision-${revision}.cabal";
      url =
        "${hackage}/package/${package}-${version}/revision/${revision}.cabal";
    };

  parseBootstrapFile = f:
    map (d:
      d // {
        pkg_id = "${d.package}-${d.version}";
        revision = toString d.revision;
      }) (pkgs.lib.importJSON f).dependencies;

  sourceFor = d:
    if d.source == "local" then
      ./${d.package}
    else if d.source == "hackage" then
      let
        src = fetchHackageSdist {
          inherit (d) package version;
          sha256 = d.src_sha256;
        };
        cabalFile = fetchHackageCabal {
          inherit (d) package version revision;
          sha256 = d.cabal_sha256;
        };
      in pkgs.runCommand "sdist-${d.pkg_id}-with-revision-${d.revision}" { } ''
        set -xe
        mkdir $out

        tar xzf ${src} --strip-components=1 -C $out
        cp -v ${cabalFile} $out/${d.package}.cabal
      ''
    else
      abort "source ${d.source} not implemented";

  # This produces a Setup executable from the Cabal version shipped
  # with the compiler.
  simpleSetup = let
    src = builtins.toFile "Setup.hs" ''
      import Distribution.Simple
      main = defaultMain
    '';
  in pkgs.runCommand "cabal-setup-simple" { nativeBuildInputs = [ ghc ]; } ''
    mkdir $out
    ghc -outputdir $out --make ${src} -o $out/Setup
  '';

  setupFor = d:
    let
      srcDir = sourceFor d;
      customSetup =
        pkgs.runCommand "${d.pkg_id}-setup" { nativeBuildInputs = [ ghc ]; } ''
          set -xe
          mkdir $out
          cd $(mktemp -d)
          ghc -outputdir $out --make ${srcDir}/Setup.hs -o $out/Setup
        '';

    in if builtins.pathExists "${srcDir}/Setup.hs" then
      "${customSetup}/Setup"
    else
      "${simpleSetup}/Setup";

  dependencies = parseBootstrapFile bootstrapFile;

  buildDepScript = d:
    let
      setup = setupFor d;
      srcDir = sourceFor d;
    in ''
      echo "Building ${d.pkg_id}"

      pushd ${srcDir}

      ${setup} configure \
        --builddir=$out/${d.pkg_id} \
        --package-db=$out/packages.conf \
        --prefix=$out/${d.pkg_id} \
        --flags="${pkgs.lib.concatStringsSep " " d.flags}"

      ${setup} build \
        --builddir=$out/${d.pkg_id}

      ${setup} install \
        --builddir=$out/${d.pkg_id}

      popd
    '';
in pkgs.runCommandCC "cabal-install" {
  # note that this has to include all system dependencies
  nativeBuildInputs = [ ghc pkgs.binutils pkgs.zlib ];
} ''
  set -ex
  mkdir $out

  ghc-pkg init $out/packages.conf

  ${pkgs.lib.concatMapStringsSep "\n" buildDepScript dependencies}
''
