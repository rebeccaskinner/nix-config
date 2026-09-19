# Writing: technical documentation, blog posts, fiction, and presentations.
# Document conversion, diagramming, spell and prose checking,
# and a presenter console for PDF slides.
# LaTeX itself, including beamer, lives in profiles/latex.
{ pkgs, primaryUser, ... }:
let
  aspell = pkgs.aspellWithDicts (dicts: with dicts; [
    en
    en-computers
    en-science
  ]);
in
{
  users.users.${primaryUser}.packages = [ aspell ] ++ (with pkgs; [
    drawio
    graphviz
    pandoc
    pdfpc
    vale
  ]);
}
