rec {
  mergeEnvironments = aPartial: bPartial:
    let
      a = unpartialEnvironment aPartial;
      b = unpartialEnvironment bPartial;
    in { packages = a.packages ++ b.packages;
         imports = a.imports ++ b.imports;
         emacsExtraPackages = epkgs: (a.emacsExtraPackages epkgs) ++ (b.emacsExtraPackages epkgs);
         emacsExtraConfig = a.emacsExtraConfig + b.emacsExtraConfig;
       };

  emptyEnvironment =
    { packages = []; imports = []; emacsExtraPackages = {...}: []; emacsExtraConfig = ""; };

  unpartialEnvironment = partialEnvironment:
    emptyEnvironment // partialEnvironment;

  concatEnvironments =
    builtins.foldl' mergeEnvironments emptyEnvironment;

  simpleEnvironment = { packages, imports }:
    { inherit packages imports; emacsExtraPackages = {...}: []; emacsExtraConfig = ""; };

  packagesEnvironment = packages: simpleEnvironment { inherit packages; imports = []; };
  packageEnvironment = package: simpleEnvironment { imports = []; packages = [package]; };
  importsEnvironment = imports: simpleEnvironment { inherit imports; packages = []; };
  importOnlyEnvironment = importVal: simpleEnvironment { imports = [ importVal ]; packages = []; };
  configOnlyEnvironment = configValue: importOnlyEnvironment (any: configValue);
}
