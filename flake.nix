{
  description = "Codex IDE integration for Emacs";

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

      mkCodex =
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          lib = pkgs.lib;
          version = "0.1.0";

          emacs = pkgs.emacs30-nox or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;
          elispFiles = [
            "lisp/codex-ide-debug.el"
            "lisp/codex-ide-term.el"
            "lisp/codex-ide-context.el"
            "lisp/codex-ide-mcp.el"
            "lisp/codex-ide-appserver.el"
            "lisp/codex-ide.el"
            "lisp/codex-ide-menu.el"
          ];
          elispFileArgs = lib.concatStringsSep " " elispFiles;
          ignoredSourceNames = [
            ".direnv"
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

          codexIde = emacsPackages.trivialBuild {
            pname = "codex-ide";
            inherit version;
            src = source;
            packageRequires = [ keymapPopup ];

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
              description = "Codex IDE integration for Emacs";
              homepage = "https://git.thanosapollo.org/emacs-codex";
              license = licenses.gpl3Plus;
              platforms = emacs.meta.platforms;
            };
          };

          devEmacs = emacsPackages.emacsWithPackages (_: [ keymapPopup ]);
          emacsWithCodex = emacsPackages.emacsWithPackages (_: [ codexIde ]);

          mkCheck =
            name: target:
            pkgs.stdenvNoCC.mkDerivation {
              pname = "codex-ide-${name}";
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
                make ${target} CODEX_ENV_WRAPPED=1 EMACS=emacs
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
                  name = "codex-ide-${name}";
                  runtimeInputs = [
                    devEmacs
                    pkgs.gnumake
                  ];
                  text = ''
                    export CODEX_ENV_WRAPPED=1
                    exec make ${target} "$@"
                  '';
                }
              }/bin/codex-ide-${name}";
              meta.description = "Run make ${target} for codex-ide";
            };

          check = mkCheck "check" "check";
        in
        {
          inherit
            check
            devEmacs
            emacsWithCodex
            codexIde
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
      apps = forAllSystems (system: (mkCodex system).apps);

      checks = forAllSystems (system: {
        default = (mkCodex system).check;
        package = (mkCodex system).codexIde;
      });

      devShells = forAllSystems (system: {
        default =
          let
            codex = mkCodex system;
          in
          codex.pkgs.mkShell {
            packages = with codex.pkgs; [
              codex.devEmacs
              git
              gnumake
            ];

            EMACS = "emacs";
            EMACS_CMD = "emacs";
            CODEX_ENV_WRAPPED = "1";

            shellHook = ''
              echo "codex-ide dev shell"
              echo "  make check    # compile, lint, and ERT"
              echo "  make test     # ERT only"
            '';
          };
      });

      formatter = forAllSystems (system: (mkCodex system).pkgs.nixfmt);

      overlays.default = final: prev: {
        codex-ide = self.packages.${prev.system}.codex-ide;
        codex-ide-emacs = self.packages.${prev.system}.emacs-with-codex;
      };

      packages = forAllSystems (system: {
        default = (mkCodex system).codexIde;
        emacs-with-codex = (mkCodex system).emacsWithCodex;
        codex-ide = (mkCodex system).codexIde;
        keymap-popup = (mkCodex system).keymapPopup;
      });
    };
}
