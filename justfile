# EXWM-VR task runner
# https://just.systems

set dotenv-load

project_root := justfile_directory()
core_el := `find lisp/core -name '*.el' -not -name '*-pkg.el' -not -name '*-autoloads.el' 2>/dev/null | sort`
vr_el := `find lisp/vr -name '*.el' 2>/dev/null | sort`
ext_el := `find lisp/ext -name '*.el' 2>/dev/null | sort`
all_el := core_el + " " + vr_el + " " + ext_el
load_flags := "-L " + project_root + "/lisp/core -L " + project_root + "/lisp/vr -L " + project_root + "/lisp/ext"

# ── build ──────────────────────────────────────────────

[group('build')]
build:
    @echo "Byte-compiling Elisp..."
    emacs --batch \
        {{load_flags}} \
        --eval '(setq byte-compile-error-on-warn nil)' \
        -f batch-byte-compile {{all_el}}
    @echo "Done."

[group('build')]
build-compositor:
    @echo "Building compositor (Rust)..."
    cargo build --manifest-path "{{project_root}}/compositor/Cargo.toml"

[group('build')]
build-all: build build-compositor

# ── test ───────────────────────────────────────────────

[group('test')]
test:
    @echo "Running ERT tests..."
    emacs --batch \
        {{load_flags}} \
        -l "{{project_root}}/test/run-tests.el"

[group('test')]
test-compositor:
    @echo "Running compositor tests..."
    cargo test --manifest-path "{{project_root}}/compositor/Cargo.toml"

[group('test')]
test-integration:
    @echo "Running integration tests..."
    emacs --batch \
        {{load_flags}} \
        -l "{{project_root}}/test/run-tests.el" \
        --eval '(ert-run-tests-batch-and-exit "week.*-integration")'

[group('test')]
test-all: test test-compositor test-integration

# ── lint ───────────────────────────────────────────────

[group('lint')]
lint-elisp:
    @echo "Linting Elisp..."
    emacs --batch \
        {{load_flags}} \
        --eval '(setq byte-compile-error-on-warn t)' \
        -f batch-byte-compile {{all_el}}

[group('lint')]
lint-rust:
    @echo "Linting Rust..."
    cargo clippy --manifest-path "{{project_root}}/compositor/Cargo.toml" -- -D warnings

[group('lint')]
lint-all: lint-elisp lint-rust

# ── vr ─────────────────────────────────────────────────

[group('vr')]
vr-mock:
    @echo "Launching compositor with Monado headless..."
    XRT_COMPOSITOR_FORCE_HEADLESS=1 \
        cargo run --manifest-path "{{project_root}}/compositor/Cargo.toml"

[group('vr')]
vr-test:
    @echo "Running VR integration tests..."
    XRT_COMPOSITOR_FORCE_HEADLESS=1 \
        cargo test --manifest-path "{{project_root}}/compositor/Cargo.toml" \
        -- --test-threads=1 vr_

# ── dev ────────────────────────────────────────────────

[group('dev')]
dev:
    @echo "Launching Emacs with EXWM-VR load path..."
    emacs {{load_flags}} --eval '(require (quote exwm))'

[group('dev')]
clean:
    @echo "Cleaning build artifacts..."
    rm -f "{{project_root}}"/lisp/core/*.elc
    rm -f "{{project_root}}"/lisp/vr/*.elc
    rm -f "{{project_root}}"/lisp/ext/*.elc
    rm -rf "{{project_root}}/compositor/target"
    @echo "Done."

[group('dev')]
changelog:
    git-cliff --output "{{project_root}}/CHANGELOG.md"

[group('dev')]
changelog-unreleased:
    git-cliff --unreleased

# ── ci ─────────────────────────────────────────────────

[group('ci')]
ci: lint-elisp build test
    @echo "CI passed."
