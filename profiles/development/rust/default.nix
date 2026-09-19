# Basic Rust toolchain for hacking on small projects outside of a
# project-specific nix environment.
{ pkgs, primaryUser, ... }:
{
  users.users.${primaryUser}.packages = with pkgs; [
    cargo
    cargo-edit
    clippy
    rust-analyzer
    rustc
    rustfmt
  ];
}
