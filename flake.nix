{
  description = "Peridot devenv";

  inputs = {
    nixpkgs.url = "nixpkgs";
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
      deps = [
        pkgs.rustup
        # for building cdeps
        pkgs.cmake
        pkgs.ninja
        pkgs.pkg-config
        pkgs.clang
        pkgs.llvmPackages.libclang
        # required libs for building engine
        pkgs.udev
        pkgs.wayland
        pkgs.pulseaudio
        pkgs.pipewire
        pkgs.vulkan-loader
        pkgs.fontconfig
        pkgs.freetype
        pkgs.harfbuzz
        pkgs.dbus
        pkgs.libxkbcommon
        pkgs.icu
        # required for some asset processing
        pkgs.shaderc
        pkgs.shader-slang
        # required for workflow generator(also included in githooks)
        pkgs.stack
        # building browser-based tools
        pkgs.bun
        # helper scripts
        build-tools
        # debugging
        pkgs.lldb
        pkgs.vulkan-validation-layers
      ];
      nativeDeps = [ pkgs.pkg-config ];
      shellSetCommonEnvVars = ''
        export PROJECT_ROOT=$(dirname $(realpath ./flake.nix))
        # set library search paths for thirdparty
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PROJECT_ROOT/thirdparty/slang/source-repo/build/RelWithDebInfo/lib:$PROJECT_ROOT/thirdparty/ktx/source-repo/build:${pkgs.vulkan-loader.outPath}/lib
        # peridot specific env vars for development
        export PERIDOT_CLI_BUILTIN_ASSETS_PATH=$PROJECT_ROOT/builtin-assets
        export PERIDOT_CLI_CRADLE_BASE=$PROJECT_ROOT/cradle
      '';
      LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
    in
    {
      devShells."${system}" = {
        default = pkgs.mkShell {
          buildInputs = deps;
          nativeBuildInputs = nativeDeps;
          shellHook = shellSetCommonEnvVars;

          # このへんはないとエラーになる
          inherit LIBCLANG_PATH;
        };
        fish =
          let
            fishPrehook = pkgs.writeScriptBin "startup" ''
              # prepend devenv prompt
              functions -c fish_prompt __peridot_fish_prompt_org
              function fish_prompt
                # preserve status code
                set -l last_status $status
                printf "[Peridot] "
                echo "exit $last_status" | .
                __peridot_fish_prompt_org
              end
            '';
          in
          pkgs.mkShell {
            buildInputs = deps ++ [ pkgs.fish ];
            nativeBuildInputs = nativeDeps;
            shellHook = ''
              ${shellSetCommonEnvVars}

              exec ${pkgs.fish.outPath}/bin/fish -C "source ${fishPrehook}/bin/startup"
            '';

            # このへんはないとエラーになる
            inherit LIBCLANG_PATH;
          };
      };
    };
}
