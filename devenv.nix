{ pkgs, ... }:

{
  # https://devenv.sh/packages/
  packages = [
    pkgs.mill
  ];

  # https://devenv.sh/languages/
  languages = {
    scala.enable = true;
    java.jdk.package = pkgs.jdk17;
  };

  # https://devenv.sh/pre-commit-hooks/
  pre-commit.hooks = {
    mdsh.enable = true;
  };
}
