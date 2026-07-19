{
  description = "Peridot devenv";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";
  };
  outputs =
    { nixpkgs, ... }:
    let
      target-systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      build-tools =
        pkgs:
        pkgs.writeShellApplication {
          name = "build-tools";
          runtimeInputs = [
            pkgs.rustup
            # for buidling cdeps
            pkgs.cmake
            pkgs.ninja
          ];
          text = ''pushd "$PROJECT_ROOT"/tools; cargo build; popd'';
        };
      common-deps = pkgs: [
        pkgs.rustup
        # for building cdeps
        pkgs.cmake
        pkgs.ninja
        pkgs.pkg-config
        pkgs.clang
        pkgs.llvmPackages.libclang
        # required libs for building engine
        pkgs.vulkan-loader
        pkgs.fontconfig
        pkgs.freetype
        pkgs.harfbuzz
        pkgs.dbus
        pkgs.libxkbcommon
        pkgs.icu76
        # required for some asset processing
        pkgs.shaderc
        pkgs.shader-slang
        # required for workflow generator(also included in githooks)
        pkgs.stack
        # building browser-based tools
        pkgs.bun
        # helper scripts
        (build-tools pkgs)
        # debugging
        pkgs.lldb
        pkgs.vulkan-validation-layers
      ];
      native-deps = pkgs: [ pkgs.pkg-config ];
      shell-set-common-env-vars = pkgs: ''
        export PROJECT_ROOT=$(dirname $(realpath ./flake.nix))
        # set library search paths for thirdparty
        export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PROJECT_ROOT/thirdparty/slang/source-repo/build/RelWithDebInfo/lib:$PROJECT_ROOT/thirdparty/ktx/source-repo/build:${pkgs.vulkan-loader.outPath}/lib
        # peridot specific env vars for development
        export PERIDOT_CLI_BUILTIN_ASSETS_PATH=$PROJECT_ROOT/builtin-assets
        export PERIDOT_CLI_CRADLE_BASE=$PROJECT_ROOT/cradle
      '';
      libclang-path = pkgs: "${pkgs.llvmPackages.libclang.lib}/lib";
    in
    builtins.foldl' (a: b: a // b) { } (
      map (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          platform-deps =
            if system == "x86_64-linux" then
              [
                # required libs for building engine (linux specific)
                pkgs.udev
                pkgs.wayland
                pkgs.pulseaudio
                pkgs.pipewire
              ]
            else
              [ ];
          platform-extra-setup-script = if system == "aarch64-darwin" then
            ''
              echo "export PATH=${pkgs.rustup.outPath}/bin:\$PATH" > editor/mac/marble-editor/.build.envrc
            ''
          else "";
          LIBCLANG_PATH = libclang-path pkgs;

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
        {
          devShells."${system}" = {
            default = pkgs.mkShell {
              buildInputs = common-deps pkgs ++ platform-deps;
              nativeBuildInputs = native-deps pkgs;
              shellHook = ''
                ${shell-set-common-env-vars pkgs}
                ${platform-extra-setup-script}
              '';

              # このへんはないとエラーになる
              inherit LIBCLANG_PATH;
            };
            fish = pkgs.mkShell {
              buildInputs = common-deps pkgs ++ platform-deps ++ [ pkgs.fish ];
              nativeBuildInputs = native-deps pkgs;
              shellHook = ''
                ${shell-set-common-env-vars pkgs}
                ${platform-extra-setup-script}

                exec ${pkgs.fish.outPath}/bin/fish -C "source ${fishPrehook}/bin/startup"
              '';

              # このへんはないとエラーになる
              inherit LIBCLANG_PATH;
            };
          };
        }
      ) target-systems
    );
}
