{ mkDerivation, aeson, async, base, base16-bytestring, bytestring
, containers, cryptohash-sha256, data-fix, deepseq, dhall
, directory, filepath, ghc-lib-parser, ghc-lib-parser-ex, hnix
, katip, lib, lsp, lsp-types, megaparsec, mtl, parser-combinators
, pretty-simple, prettyprinter, prettyprinter-ansi-terminal
, process, QuickCheck, scientific, ShellCheck, stm, syb
, tasty-bench, temporary, text, time, transformers
}:
mkDerivation {
  pname = "narsil";
  version = "0.1.0.0";
  src = ../../.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring containers cryptohash-sha256 data-fix
    dhall directory filepath hnix katip lsp lsp-types megaparsec mtl
    parser-combinators pretty-simple prettyprinter
    prettyprinter-ansi-terminal process scientific ShellCheck stm text
    transformers
  ];
  executableHaskellDepends = [
    aeson async base base16-bytestring bytestring containers
    cryptohash-sha256 data-fix dhall directory filepath ghc-lib-parser
    ghc-lib-parser-ex hnix katip lsp-types megaparsec mtl process
    QuickCheck ShellCheck syb text transformers
  ];
  testHaskellDepends = [
    aeson async base bytestring containers data-fix deepseq dhall
    directory filepath hnix lsp-types megaparsec mtl process QuickCheck
    ShellCheck stm temporary text transformers
  ];
  benchmarkHaskellDepends = [
    base containers data-fix deepseq directory filepath hnix process
    tasty-bench text time
  ];
  doHaddock = false;
  homepage = "https://github.com/hypermodern-consulting/narsil";
  description = "A type checker, linter, and language server for Nix";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}
