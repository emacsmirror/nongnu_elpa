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

      keymapPopupVersion = "0.4.0";

      # Build everything for one concrete Emacs.  Called once per
      # variant (full build, and emacs-nox) so the test matrix can
      # exercise both.
      mkVariant = pkgs: emacs:
        let
          lib = pkgs.lib;
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

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = pkgs.fetchurl {
              url = "https://elpa.gnu.org/packages/keymap-popup-${keymapPopupVersion}.tar";
              hash = "sha256-ZySAozyALV4fSfqNtFd3YOtW7ZBSFpCr+hAdnPm9v0E=";
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

          # Run a Makefile test target in a sandbox that mirrors a
          # buildd: clean HOME/XDG, the module built from source.
          mkTests = { pname, target }: pkgs.stdenv.mkDerivation {
            inherit pname;
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
                make ${target}
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
          inherit emacs emacsWithPackages keymapPopup omemoModule;
          # Per-file: one Emacs per test file (fast, good isolation).
          tests = mkTests { pname = "emacs-jabber-tests"; target = "test"; };
          # Combined: every file in one Emacs, suite run twice -- mirrors
          # dh_elpa_test and catches cross-test state pollution.
          testsOneshot = mkTests { pname = "emacs-jabber-tests-oneshot"; target = "test-oneshot"; };
        };

      mkJabber = system:
        let
          pkgs = import nixpkgs { inherit system; };
        in {
          inherit pkgs;
          full = mkVariant pkgs pkgs.emacs31-pgtk;
          # emacs-nox has no image support and does not preload many
          # libraries (e.g. `image'); this is what Debian ships, so it
          # catches build-only-on-nox bugs the full build hides.
          nox = mkVariant pkgs pkgs.emacs-nox;
        };
    in {
      packages = forAllSystems (system:
        let jabber = mkJabber system;
        in {
          default = jabber.full.omemoModule;
          omemo-module = jabber.full.omemoModule;
        });

      checks = forAllSystems (system:
        let jabber = mkJabber system;
        in {
          omemo-module = jabber.full.omemoModule;
          # Test matrix: {full, nox} x {per-file, combined-twice}.
          test = jabber.full.tests;
          test-nox = jabber.nox.tests;
          test-oneshot = jabber.full.testsOneshot;
          test-oneshot-nox = jabber.nox.testsOneshot;
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
              jabber.full.emacsWithPackages
              mbedtls
              pkg-config
            ];

            shellHook = ''
              export EMACS_CMD=emacs
              export CFLAGS="-I${jabber.full.emacs}/include''${CFLAGS:+ $CFLAGS}"
            '';
          };
        });
    };
}
