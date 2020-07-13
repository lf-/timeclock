{ mkDerivation, base, stdenv, time, unix }:
mkDerivation {
  pname = "timeclock";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base time unix ];
  license = stdenv.lib.licenses.gpl3;
}
