{
  description = "XMPP client for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  # Immutable 23.11 release: the exact declared minimum, not the final 29.x.
  inputs.nixpkgs-emacs291.url = "github:NixOS/nixpkgs/057f9aecfb71c4437d2b27d3323df7f93c010b7e";
  inputs.keymap-popup = {
    url = "git+https://git.thanosapollo.org/emacs-keymap-popup.git";
    flake = false;
  };

  outputs = { self, nixpkgs, nixpkgs-emacs291, keymap-popup }:
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

      keymapPopupVersion = "0.4.2";

      # Build everything for one concrete Emacs.  Called once per
      # variant (full build, and emacs-nox) so the test matrix can
      # exercise both.
      mkVariant = pkgs: emacs:
        let
          lib = pkgs.lib;
          emacsPackages = pkgs.emacsPackagesFor emacs;

          # Closed-world release/build inputs shared with the Make matrix.
          manifest = builtins.fromJSON (builtins.readFile ./admin/source-manifest.json);
          # Git flakes expose only tracked files here, before the closed filter.
          ordinaryTests = map (name: "tests/${name}") (builtins.filter
            (name: builtins.match "jabber-test-.*\\.el" name != null)
            (builtins.attrNames (builtins.readDir ./tests)));
          missingTests = builtins.filter (name: !(builtins.elem name manifest)) ordinaryTests;
          source = if missingTests != [] then
            throw "Ordinary test manifest incomplete: ${builtins.concatStringsSep ", " missingTests}"
          else lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions (map (name: ./. + "/${name}") manifest);
          };

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = keymap-popup;
            packageRequires = [ ];
          };

          emacsWithPackages = emacsPackages.emacsWithPackages (epkgs: [
            epkgs.fsm
            keymapPopup
            epkgs.package-lint
            epkgs.relint
          ]);

          # Only source dependencies: each matrix lane gets its own copy,
          # never Elisp bytecode produced by a different Emacs.
          testDependencies = pkgs.runCommand "jabber-test-dependency-sources" {} ''
            mkdir -p $out/fsm $out/keymap-popup
            tar -xf ${emacsPackages.fsm.src} --strip-components=1 -C $out/fsm --wildcards '*/fsm.el'
            cp ${keymap-popup}/*.el $out/keymap-popup/
          '';

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

          # Recursive Make and the matrix runner use their own job variables.
          testJobBudget = ''
            jobs="''${NIX_BUILD_CORES:-1}"
            case "$jobs" in *[!0-9]*) jobs=1 ;; esac
            if ! [ "$jobs" -gt 0 ] 2>/dev/null; then
              jobs=1
            fi
          '';

          # Run a Makefile test target in a sandbox that mirrors a
          # buildd: clean HOME/XDG, the module built from source.
          mkTests = { pname, target }: pkgs.stdenv.mkDerivation {
            inherit pname;
            version = "git";
            src = source;
            nativeBuildInputs = [ emacsWithPackages pkgs.gnumake pkgs.pkg-config pkgs.gnupg ];
            buildInputs = [ pkgs.mbedtls ];
            dontConfigure = true;

            buildPhase = ''
              runHook preBuild
              export HOME="$TMPDIR/home"
              export XDG_CACHE_HOME="$TMPDIR/cache"
              export XDG_CONFIG_HOME="$TMPDIR/config"
              export XDG_DATA_HOME="$TMPDIR/share"
              export XDG_STATE_HOME="$TMPDIR/state"
              # Keep first-attempt ERT diagnostics, including successful runs.
              export JABBER_MATRIX_EVIDENCE=1
              mkdir -p "$HOME" "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME" \
                "$XDG_DATA_HOME" "$XDG_STATE_HOME"
              ${testJobBudget}
              CFLAGS="${moduleCFlags}" \
                EMACS_CMD=emacs \
                JABBER_ENV_WRAPPED=1 \
                make ${target} JOBS="$jobs"
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              mkdir -p $out
              touch $out/tests-passed
              if [ -d .test-results ]; then
                cp -r .test-results $out/test-results
              fi
              runHook postInstall
            '';
          };
        in {
          inherit emacs emacsWithPackages keymapPopup omemoModule source testDependencies;
          matrix = pkgs.stdenv.mkDerivation {
            pname = "emacs-jabber-matrix-${emacs.version}";
            version = "git";
            src = source;
            nativeBuildInputs = [ emacs pkgs.python3 pkgs.gnumake pkgs.pkg-config pkgs.gnupg ];
            buildInputs = [ pkgs.mbedtls ];
            dontConfigure = true;
            buildPhase = ''
              export JABBER_MATRIX_DEPS=${testDependencies}
              export CFLAGS="${moduleCFlags}"
              ${testJobBudget}
              MATRIX_JOBS="$jobs" python3 admin/test-matrix --lane ${if emacs.version == "29.1" then "minimum" else "default"} \
                --expected ${emacs.version} --root "$TMPDIR/lane"
            '';
            installPhase = ''
              mkdir -p $out
              cp "$TMPDIR/lane/runtime.json" "$TMPDIR/lane/completion.json" \
                "$TMPDIR/lane/passed" $out/
              cp -r "$TMPDIR/lane/source/.test-results" $out/test-results
            '';
          };
          matrixShell = pkgs.mkShell {
            packages = [ emacs pkgs.python3 pkgs.gnumake pkgs.pkg-config pkgs.gnupg ];
            buildInputs = [ pkgs.mbedtls ];
            shellHook = ''
              export JABBER_MATRIX_DEPS=${testDependencies}
              export JABBER_MATRIX_VERSION=${emacs.version}
              export CFLAGS="${moduleCFlags}"
            '';
          };
          compiler = mkTests { pname = "emacs-jabber-compiler"; target = "do-lint-byte-comp do-lint-native-comp lint-compile-check lint-package-lint"; };
          # Per-file: one Emacs per test file (fast, good isolation).
          tests = mkTests { pname = "emacs-jabber-tests"; target = "test"; };
          # Combined: every file in one Emacs, suite run twice -- mirrors
          # dh_elpa_test and catches cross-test state pollution.
          testsOneshot = mkTests { pname = "emacs-jabber-tests-oneshot"; target = "test-oneshot"; };
        };

      mkJabber = system:
        let
          pkgs = import nixpkgs { inherit system; };
          pkgs291 = import nixpkgs-emacs291 { inherit system; };
        in {
          inherit pkgs;
          full = mkVariant pkgs pkgs.emacs;
          minimum = mkVariant pkgs pkgs291.emacs29-nox;
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
          keymap-popup = jabber.full.keymapPopup;
          omemo-module = jabber.full.omemoModule;
        });

      checks = forAllSystems (system:
        let jabber = mkJabber system;
        in {
          omemo-module = jabber.full.omemoModule;
          compiler = jabber.full.compiler;
          matrix-runner = jabber.pkgs.runCommand "jabber-matrix-runner" {
            nativeBuildInputs = [ jabber.pkgs.python3 ];
          } ''
            python3 ${jabber.full.source}/admin/test-matrix-runner.py
            touch $out
          '';
          matrix-minimum = jabber.minimum.matrix;
          matrix-default = jabber.full.matrix;
          # Test matrix: {full, nox} x {per-file, combined-twice}.
          test = jabber.full.tests;
          test-nox = jabber.nox.tests;
          test-oneshot = jabber.full.testsOneshot;
          test-oneshot-nox = jabber.nox.testsOneshot;
        });

      devShells = forAllSystems (system:
        let jabber = mkJabber system;
        in {
          matrix-minimum = jabber.minimum.matrixShell;
          matrix-default = jabber.full.matrixShell;
          default = jabber.pkgs.mkShell {
            packages = with jabber.pkgs; [
              cacert
              gcc
              git
              gnumake
              gnupg
              jabber.full.emacsWithPackages
              mbedtls
              pkg-config
            ];

            shellHook = ''
              export EMACS_CMD=emacs
              export CFLAGS="-I${jabber.full.emacs}/include''${CFLAGS:+ $CFLAGS}"
              # The module is loaded by Emacs outside this shell, after
              # its store paths may have been garbage-collected.
              export MBED_STATIC=1
            '';
          };
        });
    };
}
