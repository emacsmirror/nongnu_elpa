{
  description = "Codex IDE integration for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    keymap-popup = {
      url = "git+https://git.thanosapollo.org/emacs-keymap-popup";
      flake = false;
    };
    nixpkgs-emacs29.url = "github:NixOS/nixpkgs/nixos-24.11";
  };

  outputs =
    {
      self,
      nixpkgs,
      nixpkgs-emacs29,
      keymap-popup,
    }:
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
          pkgs29 = import nixpkgs-emacs29 {
            inherit system;
            config.permittedInsecurePackages = [ "emacs-nox-29.4" ];
          };
          lib = pkgs.lib;
          version = "0.1.2";

          emacs = pkgs.emacs30-nox or pkgs.emacs;
          emacsPackages = pkgs.emacsPackagesFor emacs;
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

          keymapPopupVersion =
            let
              versionLine =
                lib.findFirst (line: lib.hasPrefix ";; Version: " line)
                  (throw "keymap-popup.el has no Version header")
                  (lib.splitString "\n" (builtins.readFile "${keymap-popup}/keymap-popup.el"));
            in
            lib.removePrefix ";; Version: " versionLine;
          keymapPopupSrc = keymap-popup;

          keymapPopup = emacsPackages.melpaBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = keymapPopupSrc;
            packageRequires = [ ];
            turnCompilationWarningToError = true;
          };

          eat = emacsPackages.eat;
          vterm = emacsPackages.vterm;
          compat = emacsPackages.compat;

          codexIde = emacsPackages.melpaBuild {
            pname = "codex-ide";
            inherit version;
            src = source;
            files = ''("lisp/*.el")'';
            packageRequires = [
              compat
              keymapPopup
              eat
            ];
            turnCompilationWarningToError = true;

            meta = with lib; {
              description = "Codex IDE integration for Emacs";
              homepage = "https://git.thanosapollo.org/emacs-codex-ide";
              license = licenses.gpl3Plus;
              platforms = emacs.meta.platforms;
            };
          };

          emacs29Packages = pkgs29.emacsPackagesFor pkgs29.emacs29-nox;
          keymapPopup29 = emacs29Packages.melpaBuild {
            pname = "keymap-popup";
            version = keymapPopupVersion;
            src = keymapPopupSrc;
            packageRequires = [ ];
            # The compatibility gate owns warnings from codex-ide, not from
            # its separately maintained dependency.
            turnCompilationWarningToError = false;
          };
          codexIde29 = emacs29Packages.melpaBuild {
            pname = "codex-ide";
            inherit version;
            src = source;
            files = ''("lisp/*.el")'';
            packageRequires = [
              emacs29Packages.compat
              keymapPopup29
              emacs29Packages.eat
            ];
            turnCompilationWarningToError = true;
          };

          devEmacs = emacsPackages.emacsWithPackages (_: [
            compat
            keymapPopup
            eat
            vterm
            emacsPackages.package-lint
          ]);
          emacsWithCodex = emacsPackages.emacsWithPackages (_: [
            codexIde
            compat
            eat
          ]);
          autoloadCheck =
            pkgs.runCommand "codex-ide-autoload-check"
              {
                nativeBuildInputs = [ emacsWithCodex ];
              }
              ''
                export HOME="$TMPDIR/home"
                mkdir -p "$HOME"
                emacs -Q --batch -f package-activate-all \
                  --eval "(dolist (command '(codex-ide-context-mode codex-ide-context-start codex-ide-context-stop codex-ide-context-status codex-ide-send-selection codex-ide-show-debug codex-ide-clear-debug codex-ide-diff-preview codex-ide-mcp-start codex-ide-mcp-stop codex-ide-mcp-status codex-ide-mcp-install-codex-config codex-ide-menu codex-ide codex-ide-new-session codex-ide-resume-last codex-ide-resume codex-ide-stop codex-ide-toggle codex-ide-switch-to-buffer codex-ide-list-project-sessions codex-ide-list-sessions codex-ide-send-prompt codex-ide-send-escape codex-ide-return-live codex-ide-insert-newline codex-ide-check-status)) (unless (commandp command) (error \"Missing command autoload: %s\" command)))" \
                  --eval "(when (featurep 'vterm) (error \"Autoloads eagerly loaded vterm\"))"
                touch "$out"
              '';

          mkCheck =
            name: target:
            pkgs.stdenvNoCC.mkDerivation {
              pname = "codex-ide-${name}";
              inherit version;
              src = source;
              nativeBuildInputs = [
                devEmacs
                pkgs.git
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
                export CODEX_IDE_SKIP_PTY_TESTS=1
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

          mkApp = name: target: {
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
            autoloadCheck
            devEmacs
            emacsWithCodex
            codexIde
            codexIde29
            keymapPopup
            keymapPopupSrc
            pkgs
            eat
            ;

          apps = {
            default = mkApp "check" "check";
            check = mkApp "check" "check";
            lint = mkApp "lint" "lint";
            pty-test = mkApp "pty-test" "pty-test";
            test = mkApp "test" "test";
          };
        };
    in
    {
      apps = forAllSystems (system: (mkCodex system).apps);

      checks = forAllSystems (system: {
        autoload = (mkCodex system).autoloadCheck;
        default = (mkCodex system).check;
        emacs29-package = (mkCodex system).codexIde29;
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
        eat = (mkCodex system).eat;
      });
    };
}
