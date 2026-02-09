# Native Module Pipeline

Emacs dynamic modules via `emacs-module.h` for performance-critical operations.

## Status

Week 2 validation: confirms the native module compilation pipeline works.
Week 4 IPC will likely use pure Elisp `make-network-process` instead of native modules.
Native modules reserved for potential future use:
- Performance-critical IPC encoding
- GPU texture handle passing
- ZeroMQ bindings (alternative to emacs-zmq)

## Build

```bash
# Requires Emacs headers (provided by Nix devShell)
gcc -shared -fPIC -o hello-module.so \
    -I${EMACS_INCLUDE} \
    hello-module.c
```

## References

- [Emacs Dynamic Modules](https://www.gnu.org/software/emacs/manual/html_node/elisp/Dynamic-Modules.html)
- [emacs-module-rs](https://github.com/ubolonton/emacs-module-rs) - Rust FFI for Emacs modules
