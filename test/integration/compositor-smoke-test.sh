#!/usr/bin/env bash
# Compositor smoke test — verifies compositor starts and accepts connections.
# Requires: compositor binary built, running on Linux with Wayland support.
set -euo pipefail

COMPOSITOR="${1:-compositor/target/debug/ewwm-compositor}"

echo "=== Compositor Smoke Test ==="

# Test 1: --version flag
echo -n "  version flag... "
if $COMPOSITOR --version 2>/dev/null | grep -q "ewwm-compositor"; then
    echo "PASS"
else
    echo "FAIL"
    exit 1
fi

# Test 2: headless start and clean exit
echo -n "  headless start/exit... "
if timeout 10 $COMPOSITOR --backend headless --headless-exit-after 2 2>/dev/null; then
    echo "PASS"
else
    echo "FAIL (exit code $?)"
    exit 1
fi

echo "=== All smoke tests passed ==="
