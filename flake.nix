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
        text = ''pushd "$PROJECT_ROOT"/tools; cargo build; popd'';
      };
    in
    {
      devShells."${system}".default = pkgs.mkShell {
        buildInputs = [
          pkgs.rustup
          # for building cdeps
          pkgs.cmake
          pkgs.ninja
          pkgs.pkg-config
          pkgs.clang
          pkgs.llvmPackages.libclang
          # required libs for building engine
          pkgs.pipewire
          pkgs.udev
          pkgs.wayland
          pkgs.pulseaudio
          pkgs.vulkan-loader
          # helper scripts
          build-tools
        ];

        shellHook = ''
          export PROJECT_ROOT=$(dirname $(realpath ./flake.nix))
          # set library search paths for thirdparty
          export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PROJECT_ROOT/thirdparty/slang/source-repo/build/RelWithDebInfo/lib:$PROJECT_ROOT/thirdparty/ktx/source-repo/build
          # peridot specific env vars for development
          export PERIDOT_CLI_BUILTIN_ASSETS_PATH=$PROJECT_ROOT/builtin-assets
          export PERIDOT_CLI_CRADLE_BASE=$PROJECT_ROOT/cradle
        '';

        # このへんはないとエラーになる
        LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
      };
    };
}
