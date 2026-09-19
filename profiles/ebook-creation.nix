# Turning scanned or printed material into ebooks: scan cleanup, OCR, PDF
# manipulation, and epub editing and library management.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    calibre
    ghostscript
    img2pdf
    ocrmypdf
    pdfarranger
    pdftk
    poppler-utils
    scantailor-advanced
    sigil
    tesseract
  ];
}
