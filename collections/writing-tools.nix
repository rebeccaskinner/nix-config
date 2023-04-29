{ pkgs
, utils
, includeLatex ? true
, includeGraphicalTools ? true
, ...}:

let
  defaultPkgs = utils.env.packagesEnvironment (
    let
      aspellPkgs =
        pkgs.aspellWithDicts(dicts:
          with dicts; [ en en-computers en-science ]);
    in
      with pkgs;
      [ pandoc
        ispell
        aspellPkgs
        texlive.combined.scheme-full
        python3Packages.pygments
      ]
  );

  latexTools =
    if includeLatex
    then utils.env.packageEnvironment pkgs.texlive.combined.scheme-full
    else utils.env.emptyEnvironment;

  graphicalTools =
    if includeGraphicalTools
    then utils.env.packagesEnvironment [ pkgs.evince pkgs.okular ]
    else utils.env.emptyEnvironment;


in utils.env.concatEnvironments [ defaultPkgs latexTools graphicalTools ]
