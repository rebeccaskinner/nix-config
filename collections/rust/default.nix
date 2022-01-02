{ pkgs
, ...}:

with pkgs; [
  rustc
  rustfmt
  rust-analyzer
  cargo
  cargo-edit
]
