#!/bin/sh

echo "installing SpaceVim....."
curl -sLf https://spacevim.org/install.sh | sh
echo "Done."

echo "updating Rust toolchain..."
rustup toolchain install nightly

echo "adding rustfmt..."
rustup component add rustfmt
rustup component add rust-src
rustup component add rust-docs
rustup component add clippy-preview
rustup update

cargo +nightly install racer
