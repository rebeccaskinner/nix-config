{ emacs
, lib
}:
emacs.pkgs.trivialBuild {
    name   = "pml-mode";
    pname   = "pml-mode";
    version = "0.0.1";
    src     = ./pml-mode.el;
    meta = {
      description = "A minor mode for editing PML files";
      longDescription = "A minor mode for editing PML files";
    };
  license = lib.licenses.gpl3Plus.spdxId;
}
