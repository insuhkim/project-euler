{ pkgs, ... }:
{
  packages = with pkgs.ocamlPackages; [ zarith ];

  languages.ocaml.enable = true;
}
