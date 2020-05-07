#!/bin/sh

echo "installing SpaceVim....."
curl -sLf https://spacevim.org/install.sh | sh
echo "Done."

echo "updating Rust toolchain..."
rustup toolchain install nightly

echo "adding rustfmt..."
rustup component add rustfmt
