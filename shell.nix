{ pkgs ? import <nixpkgs> {} }:

let
  emacsForModule =
    if pkgs ? emacs30 then pkgs.emacs30
    else if pkgs ? emacs29 then pkgs.emacs29
    else pkgs.emacs;
in
pkgs.mkShell {
  packages = with pkgs; [
    cacert
    emacsForModule
    gcc
    git
    gnumake
    mbedtls
    pkg-config
  ];
}
