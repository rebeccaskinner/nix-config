{ emacs
, lib
}:
emacs.pkgs.trivialBuild {
    name   = "persistent-mode";
    pname   = "persistent-mode";
    version = "0.0.1";
    src     = ./persistent-mode.el;
    packageRequires = (with emacs.pkgs; [
      markdown-mode
      cl-lib
    ]);
    meta = {
      description = "A major mode for editing Persistent Models files";
      longDescription = "A major mode for editing Persistent Models files";
    };
  license = lib.licenses.gpl3Plus.spdxId;
}
