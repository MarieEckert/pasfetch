{
  stdenv,
  fpc,
  gnumake,
  asciidoctor
}:

stdenv.mkDerivation {
  pname = "pasfetch";
  version = "2.1.0-r3";

  src = ../.;

  strictDeps = true;
  dontConfigure = true;

  nativeBuildInputs = [
    fpc
    gnumake
    asciidoctor
  ];

  buildPhase = ''
    runHook preBuild

    make release -j

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    make install PREFIX="$out"

    runHook postInstall
  '';
}
