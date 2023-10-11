#!/usr/bin/env bash

set -euo pipefail

ROOT="$(realpath "$(dirname "$0")/..")"
sed -i "s/false;/true;/" "$ROOT/src/debug.zig"
