{ mkDerivation, aeson, base, brick, bytestring, lens, lens-aeson
, scientific, stdenv, text, wreq
}:
mkDerivation {
  pname = "weather";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base brick bytestring lens lens-aeson scientific text wreq
  ];
  description = "Command line weather app";
  license = stdenv.lib.licenses.bsd3;
}
