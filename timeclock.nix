{ mkDerivation, base, directory, stdenv, time, unix }:
mkDerivation {
  pname = "timeclock";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base directory time unix ];
  license = stdenv.lib.licenses.gpl3;
}
