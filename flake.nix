{
  description = "Hermes Agent frontend for Emacs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
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
          version = "0.1.0";

          emacs = pkgs.emacs30-nox or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;
          # Keep in sync with SRCS in the Makefile.
          elispFiles = [
            "lisp/hermes-promise.el"
            "lisp/hermes-transport.el"
            "lisp/hermes-transport-cli.el"
            "lisp/hermes-dashboard-transport.el"
            "lisp/hermes-chat-format.el"
            "lisp/hermes-chat-buffer.el"
            "lisp/hermes-chat-prompts.el"
            "lisp/hermes-chat-dashboard.el"
            "lisp/hermes-chat.el"
            "lisp/hermes-browser.el"
            "lisp/hermes-sessions.el"
            "lisp/hermes-inventory.el"
            "lisp/hermes-rollback.el"
            "lisp/hermes-subagents.el"
            "lisp/hermes-cron.el"
            "lisp/hermes-kanban-log.el"
            "lisp/hermes-kanban-events.el"
            "lisp/hermes-kanban.el"
            "lisp/hermes-mcp.el"
            "lisp/hermes-exec.el"
            "lisp/hermes-onboarding.el"
            "lisp/hermes-capabilities.el"
            "lisp/hermes.el"
          ];
          elispFileArgs = lib.concatStringsSep " " elispFiles;
          ignoredSourceNames = [
            ".direnv"
            ".eca"
            ".emacs-test-cache"
            ".hermes"
            ".test-results"
          ];

          source = lib.cleanSourceWith {
            src = ./.;
            filter =
              path: type:
              let
                name = baseNameOf path;
              in
              (lib.cleanSourceFilter path type)
              && !(lib.elem name ignoredSourceNames || lib.hasSuffix ".elc" name);
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
          websocket = emacsPackages.websocket;
          markdownMode = emacsPackages.markdown-mode;

          hermesEl = emacsPackages.trivialBuild {
            pname = "hermes-el";
            inherit version;
            src = source;
            packageRequires = [ keymapPopup websocket markdownMode ];

            buildPhase = ''
              runHook preBuild
              emacs -l package -f package-initialize -L lisp --batch \
                -f batch-byte-compile ${elispFileArgs}
              runHook postBuild
            '';

            installPhase = ''
              runHook preInstall
              lispdir=$out/share/emacs/site-lisp
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
        in
        {
          inherit
            check
            devEmacs
            emacsWithHermes
            hermesEl
            keymapPopup
            keymapPopupSrc
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
