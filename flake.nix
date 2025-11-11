{
  description = "Peridot devenv";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs =
    { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      build-tools = pkgs.writeShellApplication {
        name = "build-tools";
        runtimeInputs = [
          pkgs.rustup
          # for buidling cdeps
          pkgs.cmake
          pkgs.ninja
        ];
        text = "cd tools; cargo build; cd ..";
      };
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        buildInputs = [
          pkgs.rustup
          # for building cdeps
          pkgs.cmake
          pkgs.ninja
          # helper scripts
          build-tools
        ];
      };
    };
}
