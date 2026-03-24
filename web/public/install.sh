#!/bin/sh
# Loon installer — https://loonlang.com
# Usage: curl -fsSL https://loonlang.com/install.sh | sh
set -eu

REPO="ecto/loon"
INSTALL_DIR="$HOME/.loon/bin"

# ── Detect platform ──────────────────────────────────────────────────────────

OS="$(uname -s)"
ARCH="$(uname -m)"

case "$OS" in
  Darwin) os="apple-darwin" ;;
  Linux)  os="unknown-linux-gnu" ;;
  *)
    echo "error: unsupported OS: $OS" >&2
    echo "  Loon supports macOS and Linux. For Windows, use WSL." >&2
    exit 1
    ;;
esac

case "$ARCH" in
  x86_64|amd64)  arch="x86_64" ;;
  arm64|aarch64) arch="aarch64" ;;
  *)
    echo "error: unsupported architecture: $ARCH" >&2
    exit 1
    ;;
esac

TARGET="${arch}-${os}"

# ── Find latest release ──────────────────────────────────────────────────────

echo "  detecting platform... ${TARGET}"

LATEST=$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" \
  | grep '"tag_name"' | head -1 | sed 's/.*"tag_name": *"//;s/".*//')

if [ -z "$LATEST" ]; then
  echo "error: could not find latest release" >&2
  echo "  check https://github.com/${REPO}/releases" >&2
  exit 1
fi

echo "  latest release... ${LATEST}"

# ── Download and install ─────────────────────────────────────────────────────

TARBALL="loon-${LATEST}-${TARGET}.tar.gz"
URL="https://github.com/${REPO}/releases/download/${LATEST}/${TARBALL}"

echo "  downloading... ${TARBALL}"

TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

if ! curl -fsSL "$URL" -o "$TMP/$TARBALL"; then
  echo "error: download failed" >&2
  echo "  url: $URL" >&2
  echo "  your platform may not have a prebuilt binary yet." >&2
  echo "  try building from source: cargo install --git https://github.com/${REPO} loon-cli" >&2
  exit 1
fi

tar xzf "$TMP/$TARBALL" -C "$TMP"

mkdir -p "$INSTALL_DIR"
cp "$TMP/loon" "$INSTALL_DIR/loon"
chmod +x "$INSTALL_DIR/loon"

echo "  installed to... ${INSTALL_DIR}/loon"

# ── PATH check ───────────────────────────────────────────────────────────────

VERSION=$("$INSTALL_DIR/loon" --version 2>/dev/null || echo "unknown")
echo ""
echo "  loon ${VERSION} installed successfully!"
echo ""

case ":$PATH:" in
  *":$INSTALL_DIR:"*) ;;
  *)
    echo "  add to your PATH:"
    echo ""
    echo "    export PATH=\"\$HOME/.loon/bin:\$PATH\""
    echo ""
    # Detect shell config file
    SHELL_NAME=$(basename "$SHELL" 2>/dev/null || echo "sh")
    case "$SHELL_NAME" in
      zsh)  RC="$HOME/.zshrc" ;;
      bash) RC="$HOME/.bashrc" ;;
      fish) RC="$HOME/.config/fish/config.fish" ;;
      *)    RC="" ;;
    esac
    if [ -n "$RC" ]; then
      echo "  or run:"
      echo ""
      echo "    echo 'export PATH=\"\$HOME/.loon/bin:\$PATH\"' >> $RC"
      echo ""
    fi
    ;;
esac
