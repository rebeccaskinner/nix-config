# Defines some utilities to manage related collections of packages
# within a given package set.

rec {
  # Given a list of packages, create a new package collection that
  # includes all of the packages in the environment.
  newCollection = collectionPackages:
    {home.packages = collectionPackages;};

  # Merges a list of records. In particular, it should be used to
  # merge a list of collections created by calling 'newCollection'
 concatCollections = collections:
   let
     allPackages = builtins.foldl' (a: b: a ++ b.home.packages) [] collections;
   in newCollection allPackages;

 # Import an expression that takes some parameters and returns a package list.
 callCollection = collectionExpr: args:
   newCollection (import collectionExpr args);

 # Like callCollection but takes a list of expressions
 callCollections = collectionExprs: args:
   let
     pkgs = builtins.concatMap (e: import e args) collectionExprs;
   in newCollection pkgs;

}
