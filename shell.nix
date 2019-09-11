let
  release = import ./release.nix;
in
release.input-devices.env
