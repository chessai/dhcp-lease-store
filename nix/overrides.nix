{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  ip = dontCheck super.ip;

  dhcp-lease-store = (
    with rec {
      dhcp-lease-storeSource = pkgs.lib.cleanSource ../.;
      dhcp-lease-storeBasic  = self.callCabal2nix "dhcp-lease-store" dhcp-lease-storeSource { };
    };
    overrideCabal dhcp-lease-storeBasic (old: {
    })
  );
}
