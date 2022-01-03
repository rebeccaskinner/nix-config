let
  consOptional = shouldCons: val: lst:
    if shouldCons then [val] ++ lst else lst;

  consOptionalAttrs = {shouldInclude, optionalPackage}: lst:
    consOptional shouldInclude optionalPackage lst;

  flip = f: x: y: f y x;

in
{
  addPackageCollection = collectionPath: args:
    let
      result = import collectionPath args;
      collectionPackages =
        if builtins.isList result
        then result
        else [result];
    in { home.packages = collectionPackages; };

  includeOptionalPackage = {shouldInclude, optionalPackage}: pkgList:
    consOptional shouldInclude optionalPackage pkgList;

  includeOptionalPackages = optionalPackages: pkgList:
    builtins.foldl' (flip consOptionalAttrs) pkgList optionalPackages;

  flip = flip;
  cons = x: lst: [x] ++ lst;

  makeCollection = pkgList:
    [{home.packages = pkgList;}];
}
