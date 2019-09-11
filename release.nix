let
  overlay = self: super: {
    haskellPackages =
      super.haskellPackages.extend (haskellPackagesSelf: haskellPackagesSuper: {
        input-devices =
          haskellPackagesSelf.callCabal2nix "input-devices" ./. {};
      }
    );
  };

  channel = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  gitRevision = rev: "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs";
    url = gitRevision channel.rev;
    inherit (channel) sha256;
  };

  pkgs = import nixpkgs { overlays = [ overlay ]; };
in
{
  inherit (pkgs.haskellPackages) input-devices;
}
