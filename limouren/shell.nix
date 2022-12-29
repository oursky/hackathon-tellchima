{ pkgs ? import
    (fetchTarball {
      name = "nixpkgs";
      url = "https://github.com/NixOS/nixpkgs/archive/37c2766.tar.gz";
      sha256 = "0my1hkwnihnpfk0mf61b7vzbd5kfzyprq9xmsv5jdd274jp0y2zs";
    })
    {
      config.allowBroken = true;
    }
}:

pkgs.mkShell {
  packages = [
    pkgs.elixir_1_14
    pkgs.postgresql_15

    pkgs.darwin.apple_sdk.frameworks.CoreFoundation
    pkgs.darwin.apple_sdk.frameworks.CoreServices
  ];
}
