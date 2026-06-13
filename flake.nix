{
  description = "XMPP client for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [
        # Note: Most of the testing I've done is x86_64-linux and
        # aarch64-linux.  If you find any issues with darwin feel free
        # to report/submit a PR.
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkJabber = system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          emacs = pkgs.emacs30 or pkgs.emacs29 or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          source = lib.cleanSourceWith {
            src = ./.;
            filter = path: type:
              let name = baseNameOf path;
              in !(name == ".test-results"
                   || lib.hasSuffix ".elc" name
                   || lib.hasSuffix ".so" name
                   || lib.hasSuffix ".dylib" name
                   || lib.hasSuffix "~" name);
          };

          keymapPopupVersion = "0.3.1";

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar";
              hash = "sha256-C+ECWpChsO6MUG+oAPJDhZruWphkxy7VLe9YFAzShFQ=";
            };
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            epkgs.fsm
            keymapPopup
            epkgs.package-lint
            epkgs.relint
          ]);

          moduleCFlags = "-I${emacs}/include -fPIC -Wall -Wno-pointer-sign -Wno-unused-function -I.";

          omemoModule = pkgs.stdenv.mkDerivation {
            pname = "emacs-jabber-omemo-module";
            version = "git";
            src = source;
            nativeBuildInputs = [ pkgs.gnumake pkgs.pkg-config ];
            buildInputs = [ pkgs.mbedtls ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              mkdir -p out
              CFLAGS="${moduleCFlags}" make -C src INSTALL_DIR="$PWD/out"
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out/lib/emacs-jabber
              cp out/jabber-omemo-core.* $out/lib/emacs-jabber/
              runHook postInstall
            '';
          };

          tests = pkgs.stdenv.mkDerivation {
            pname = "emacs-jabber-tests";
            version = "git";
            src = source;
            nativeBuildInputs = [ emacsWithPackages pkgs.gnumake pkgs.pkg-config ];
            buildInputs = [ pkgs.mbedtls ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              CFLAGS="${moduleCFlags}" \
                EMACS_CMD=emacs \
                JABBER_ENV_WRAPPED=1 \
                make test
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
          inherit emacs emacsWithPackages keymapPopup omemoModule pkgs tests;
        };
    in {
      packages = forAllSystems (system:
        let jabber = mkJabber system;
        in {
          default = jabber.omemoModule;
          omemo-module = jabber.omemoModule;
        });

      checks = forAllSystems (system:
        let jabber = mkJabber system;
        in {
          omemo-module = jabber.omemoModule;
          test = jabber.tests;
        });

      devShells = forAllSystems (system:
        let jabber = mkJabber system;
        in {
          default = jabber.pkgs.mkShell {
            packages = with jabber.pkgs; [
              cacert
              gcc
              git
              gnumake
              jabber.emacsWithPackages
              mbedtls
              pkg-config
            ];

            shellHook = ''
              export EMACS_CMD=emacs
              export CFLAGS="-I${jabber.emacs}/include''${CFLAGS:+ $CFLAGS}"
            '';
          };
        });
    };
}
