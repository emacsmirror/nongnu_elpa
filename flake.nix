{
  description = "Hermes Agent frontend for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkHermes = system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = pkgs.emacs30-nox or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          source = lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let name = baseNameOf path;
              in !(name == ".test-results"
                   || name == ".eca"
                   || lib.hasSuffix ".elc" name
                   || lib.hasSuffix "~" name);
          };

          keymapPopupVersion = "0.3.1";

          keymapPopupSrc = pkgs.fetchzip {
            url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar";
            hash = "sha256-hoH9SJ8LQS/uWNmwvauBJwMnnr4+DwhJpUFuHOihldM=";
          };

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = keymapPopupSrc;
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            keymapPopup
          ]);

          tests = pkgs.stdenv.mkDerivation {
            pname = "hermes-el-tests";
            version = "git";
            src = source;
            nativeBuildInputs = [
              emacsWithPackages
              pkgs.gnumake
            ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              unset EMACSDATA EMACSDOC EMACSLOADPATH EMACSPATH GREP_OPTIONS
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              make check EMACS=emacs
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              touch $out/tests-passed
              runHook postInstall
            '';
          };
        in {
          inherit emacs emacsWithPackages keymapPopup keymapPopupSrc pkgs tests;
        };
    in {
      checks = forAllSystems (system:
        let hermes = mkHermes system;
        in {
          test = hermes.tests;
        });

      devShells = forAllSystems (system:
        let hermes = mkHermes system;
        in {
          default = hermes.pkgs.mkShell {
            packages = with hermes.pkgs; [
              hermes.emacsWithPackages
              git
              gnumake
            ];

            shellHook = ''
              echo "hermes-el dev shell: run make check"
            '';
          };
        });
    };
}
