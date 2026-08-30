{
  description = "Hermes Agent frontend for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.keymap-popup = {
    url = "git+https://git.thanosapollo.org/emacs-keymap-popup";
    flake = false;
  };

  outputs =
    { self, nixpkgs, keymap-popup }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      mkHermes =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          version = "0.4.2";

          emacs = pkgs.emacs30-nox or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;
          # Keep in sync with SRCS in the Makefile.
          elispFiles = [
            "lisp/hermes-promise.el"
            "lisp/hermes-notifications.el"
            "lisp/hermes-session-title.el"
            "lisp/hermes-transport.el"
            "lisp/hermes-transport-cli.el"
            "lisp/hermes-dashboard-api.el"
            "lisp/hermes-dashboard-transport.el"
            "lisp/hermes-dashboard-rpc.el"
            "lisp/hermes-chat-format.el"
            "lisp/hermes-chat-render.el"
            "lisp/hermes-chat-buffer.el"
            "lisp/hermes-chat-prompts.el"
            "lisp/hermes-chat-dashboard.el"
            "lisp/hermes-chat-models.el"
            "lisp/hermes-chat-handoff.el"
            "lisp/hermes-chat-slash.el"
            "lisp/hermes-chat.el"
            "lisp/hermes-browser.el"
            "lisp/hermes-sessions.el"
            "lisp/hermes-inventory.el"
            "lisp/hermes-rollback.el"
            "lisp/hermes-subagents.el"
            "lisp/hermes-cron.el"
            "lisp/hermes-profiles.el"
            "lisp/hermes-messaging.el"
            "lisp/hermes-kanban-log.el"
            "lisp/hermes-kanban-events.el"
            "lisp/hermes-kanban.el"
            "lisp/hermes-mcp.el"
            "lisp/hermes-config.el"
            "lisp/hermes-system.el"
            "lisp/hermes-command-palette.el"
            "lisp/hermes-exec.el"
            "lisp/hermes-onboarding.el"
            "lisp/hermes-capabilities.el"
            "lisp/hermes.el"
          ];
          allElispFiles = map (name: "lisp/${name}") (
            lib.filter (
              name:
              !(lib.hasPrefix "." name) && lib.hasSuffix ".el" name
            ) (builtins.attrNames (builtins.readDir ./lisp))
          );
          elispFileArgs =
            assert lib.assertMsg
              (lib.sort builtins.lessThan elispFiles == lib.sort builtins.lessThan allElispFiles)
              "flake elispFiles must list every lisp/*.el source";
            lib.concatStringsSep " " elispFiles;
          # Closed-world: only Makefile, explicit Lisp paths, top-level tests/*.el.
          # No directory trees — nested/generated/private files stay out of src.
          # Dot-prefixed basenames stay out of scans and release membership.
          testElFiles = [
            "hermes-browsers-tests.el"
            "hermes-capabilities-tests.el"
            "hermes-chat-dashboard-tests.el"
            "hermes-chat-handoff-tests.el"
            "hermes-chat-models-tests.el"
            "hermes-chat-prompts-tests.el"
            "hermes-chat-reducer-tests.el"
            "hermes-chat-tests.el"
            "hermes-command-palette-tests.el"
            "hermes-config-tests.el"
            "hermes-cron-tests.el"
            "hermes-dashboard-tests.el"
            "hermes-exec-tests.el"
            "hermes-inventory-tests.el"
            "hermes-kanban-tests.el"
            "hermes-mcp-tests.el"
            "hermes-messaging-tests.el"
            "hermes-notifications-tests.el"
            "hermes-onboarding-tests.el"
            "hermes-promise-tests.el"
            "hermes-sessions-tests.el"
            "hermes-system-tests.el"
            "hermes-test-helpers.el"
            "hermes-transport-tests.el"
            "hermes-ui-tests.el"
          ];
          allTestElFiles =
            let
              entries = builtins.readDir ./tests;
            in
            map (name: "tests/${name}") (
              lib.filter (
                name:
                entries.${name} == "regular"
                && !(lib.hasPrefix "." name)
                && lib.hasSuffix ".el" name
              ) (builtins.attrNames entries)
            );
          testFileArgs =
            assert lib.assertMsg
              (lib.sort builtins.lessThan (map (name: "tests/${name}") testElFiles)
                == lib.sort builtins.lessThan allTestElFiles)
              "flake testElFiles must list every tests/*.el source";
            testElFiles;
          releaseFileset = lib.fileset.unions (
            [ ./Makefile ]
            ++ map (path: ./. + "/${path}") elispFiles
            ++ map (name: ./tests + "/${name}") testFileArgs
          );
          source = lib.fileset.toSource {
            root = ./.;
            fileset = releaseFileset;
          };

          keymapPopupVersion = "0.4.0";
          keymapPopupSrc = keymap-popup;

          keymapPopup = emacsPackages.trivialBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = keymapPopupSrc;
            packageRequires = [ ];
          };
          websocket = emacsPackages.websocket;
          markdownMode = emacsPackages.markdown-mode;

          hermesEl = emacsPackages.trivialBuild {
            pname = "hermes-el";
            inherit version;
            src = source;
            packageRequires = [ keymapPopup websocket markdownMode ];

            buildPhase = ''
              runHook preBuild
              emacs -Q --batch -l loaddefs-gen \
                --eval '(loaddefs-generate "lisp" "lisp/hermes-autoloads.el")'
              emacs -Q --batch -l package \
                --eval '(with-temp-buffer
                           (insert-file-contents "lisp/hermes.el")
                           (let ((description (package-buffer-info)))
                             ;; keymap-popup is a propagated trivialBuild library,
                             ;; not an ELPA directory package in Nix.
                             (setf (package-desc-reqs description)
                                   (assq-delete-all
                                    (quote keymap-popup)
                                    (package-desc-reqs description)))
                             (package-generate-description-file
                              description "lisp/hermes-pkg.el")))'
              emacs -l package -f package-initialize -L lisp --batch \
                -f batch-byte-compile ${elispFileArgs}
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              lispdir=$out/share/emacs/site-lisp/elpa/hermes-${version}
              mkdir -p "$lispdir"
              install -m444 lisp/*.el lisp/*.elc "$lispdir/"
              runHook postInstall
            '';

            meta = with lib; {
              description = "Emacs frontend for Hermes Agent";
              homepage = "https://git.thanosapollo.org/emacs-hermes";
              license = licenses.gpl3Plus;
              platforms = emacs.meta.platforms;
            };
          };

          devEmacs = emacsPackages.emacsWithPackages (_: [ keymapPopup websocket markdownMode ]);
          emacsWithHermes = emacsPackages.emacsWithPackages (_: [ hermesEl ]);

          mkCheck =
            name: target:
            pkgs.stdenvNoCC.mkDerivation {
              pname = "hermes-el-${name}";
              inherit version;
              src = source;
              nativeBuildInputs = [
                devEmacs
                pkgs.gnumake
              ];
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
                make ${target} HERMES_ENV_WRAPPED=1 EMACS=emacs
                runHook postBuild
              '';

              installPhase = ''
                runHook preInstall
                mkdir -p "$out"
                touch "$out/${name}-passed"
                runHook postInstall
              '';
            };

          mkApp =
            name: target:
            {
              type = "app";
              program = "${
                pkgs.writeShellApplication {
                  name = "hermes-el-${name}";
                  runtimeInputs = [
                    devEmacs
                    pkgs.gnumake
                  ];
                  text = ''
                    export HERMES_ENV_WRAPPED=1
                    exec make ${target} "$@"
                  '';
                }
              }/bin/hermes-el-${name}";
              meta.description = "Run make ${target} for hermes-el";
            };

          check = mkCheck "check" "check";

          packageSmoke =
            pkgs.runCommand "hermes-el-package-smoke"
              {
                nativeBuildInputs = [ emacsWithHermes ];
              }
              ''
                set -eu
                lispdir="${hermesEl}/share/emacs/site-lisp/elpa/hermes-${version}"
                for f in ${elispFileArgs}; do
                  base=$(basename "$f" .el)
                  test -f "$lispdir/$base.elc"
                done
                test -f "$lispdir/hermes-autoloads.el"
                test -f "$lispdir/hermes-pkg.el"
                export HOME="$TMPDIR/home"
                mkdir -p "$HOME"
                emacs -Q --batch --eval '(progn
                  (package-initialize)
                  (unless (and (commandp (quote hermes))
                               (autoloadp (symbol-function (quote hermes))))
                    (error "installed package autoload missing"))
                  (require (quote hermes-session-title))
                  (require (quote hermes))
                  (unless (and (fboundp (quote hermes-session-title-canonicalize))
                               (fboundp (quote hermes)))
                    (error "installed package symbols missing")))'
                touch "$out"
              '';
        in
        {
          inherit
            check
            devEmacs
            emacsWithHermes
            hermesEl
            keymapPopup
            keymapPopupSrc
            packageSmoke
            pkgs
            ;

          apps = {
            default = mkApp "check" "check";
            check = mkApp "check" "check";
            lint = mkApp "lint" "lint";
            test = mkApp "test" "test";
          };
        };
    in
    {
      apps = forAllSystems (system: (mkHermes system).apps);

      checks = forAllSystems (system: {
        default = (mkHermes system).check;
        package = (mkHermes system).hermesEl;
        package-smoke = (mkHermes system).packageSmoke;
      });

      devShells = forAllSystems (system: {
        default =
          let
            hermes = mkHermes system;
          in
          hermes.pkgs.mkShell {
            packages = with hermes.pkgs; [
              hermes.devEmacs
              git
              gnumake
            ];

            EMACS = "emacs";
            EMACS_CMD = "emacs";
            HERMES_ENV_WRAPPED = "1";

            shellHook = ''
              echo "hermes-el dev shell"
              echo "  make check    # compile, lint, and ERT"
              echo "  make test     # ERT only"
            '';
          };
      });

      formatter = forAllSystems (system: (mkHermes system).pkgs.nixfmt);

      overlays.default = final: prev: {
        hermes-el = self.packages.${prev.system}.hermes-el;
        hermes-el-emacs = self.packages.${prev.system}.emacs-with-hermes;
      };

      packages = forAllSystems (system: {
        default = (mkHermes system).hermesEl;
        emacs-with-hermes = (mkHermes system).emacsWithHermes;
        hermes-el = (mkHermes system).hermesEl;
        keymap-popup = (mkHermes system).keymapPopup;
      });
    };
}
