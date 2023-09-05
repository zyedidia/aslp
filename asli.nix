{ lib,
  fetchFromGitHub,
  ocaml,
  pkgs,
  ocamlPackages
}:


ocamlPackages.buildDunePackage {
  pname = "asli";
  version = "0.2.0";

  minimalOCamlVersion = "4.09";

  src = pkgs.nix-gitignore.gitignoreSource ["*.nix" "result" "result-*"] ./.;

  checkInputs = [ ocamlPackages.alcotest ];
  buildInputs = [ pkgs.z3 ];
  nativeBuildInputs = (with pkgs; [ ott gnused ]) ++ (with ocamlPackages; [ menhir ]);
  propagatedBuildInputs = with ocamlPackages; [ linenoise pprint zarith z3 ocaml_pcre ];
  doCheck = lib.versionAtLeast ocaml.version "4.09";

  configurePhase = ''
    export ASLI_OTT=${pkgs.ott.out + "/share/ott"}
    mkdir -p $out/asl
    cp -rv prelude.asl mra_tools tests $out/asl
  '';

  outputs = [ "out" ];

  meta = {
    homepage = "https://github.com/inhabitedtype/angstrom";
    description = "OCaml parser combinators built for speed and memory efficiency";
    license = lib.licenses.bsd3;
    maintainers = with lib.maintainers; [ sternenseemann ];
  };
}
