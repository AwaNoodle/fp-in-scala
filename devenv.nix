{ pkgs, ... }:

{
  # https://devenv.sh/packages/
  packages = [ pkgs.mill ];

  # https://devenv.sh/languages/
  languages = {
    scala.enable = true;
    java.jdk.package = pkgs.jdk17;
  };

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks = {
    # Github action linting
    actionlint.enable = true;
    # Markdown 
    markdownlint.enable = true;
    # Nix formatter
    nixfmt.enable = true;
  };

  scripts."local-test".exec = ''
    mill _.compile
    mill _.test
  '';
}
