{pkgs}:
{inputFile, fileSha, shaAlgo ? "sha256"}:
pkgs.runCommand "unpackDungeonDraft" {
  name = "DungeonDraft.zip";
  outputHashMode = "flat";
  outputHashAlgo = shaAlgo;
  outputHash = fileSha;
  nativeBuildInputs = [pkgs.unzip];
} ''
cp ${inputFile} $out
''
