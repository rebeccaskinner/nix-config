{ pkgs
, utils
, includeLatex ? true
, includeGraphicalTools ? true
, ...}:

let

  aspellPkgs =
    pkgs.aspellWithDicts(dicts:
      with dicts; [ en en-computers en-science ]);

  defaultPkgs = with pkgs; [
    pandoc
    ispell
    aspellPkgs
    texlive.combined.scheme-full
    pythonPackages.pygments
  ];

  optionalLatex =
    { shouldInclude = includeLatex;
      optionalPackage = pkgs.texlive.combined.scheme-full;
    };

  optionalViewers =
    let
      includeGraphicalToolF = pkg:
        { shouldInclude = includeGraphicalTools;
          optionalPackage = pkg;
        };
      graphicalTools = with pkgs; [ evince okular ];
    in builtins.map includeGraphicalToolF graphicalTools;

  optionalPkgs = utils.cons optionalLatex optionalViewers;
  collection = utils.includeOptionalPackages optionalPkgs defaultPkgs;

in utils.makeCollection collection
