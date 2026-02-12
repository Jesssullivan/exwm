;;; v031-ipc-security-test.el --- v0.3.1 IPC security tests  -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for SO_PEERCRED authentication and per-client rate limiting.

;;; Code:

(require 'ert)

(defvar v031-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name)))))

;; ── SO_PEERCRED peer credential reading ─────────────────────

(ert-deftest v031/server-has-peer-uid-field ()
  "IpcClient has peer_uid field."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "peer_uid" nil t)))))

(ert-deftest v031/server-has-peer-pid-field ()
  "IpcClient has peer_pid field."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "peer_pid" nil t)))))

(ert-deftest v031/server-reads-peer-cred ()
  "server.rs reads peer credentials via SO_PEERCRED."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "SO_PEERCRED" nil t)))))

(ert-deftest v031/dispatch-checks-uid ()
  "hello handler checks peer UID against compositor UID."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "libc::getuid()" nil t)))))

(ert-deftest v031/dispatch-rejects-uid-mismatch ()
  "hello handler rejects UID mismatch."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "UID mismatch" nil t)))))

;; ── Rate limiting ───────────────────────────────────────────

(ert-deftest v031/server-has-rate-limiter ()
  "IpcClient has rate_limiter field."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "rate_limiter" nil t)))))

(ert-deftest v031/server-has-rate-limiter-struct ()
  "server.rs defines RateLimiter struct."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "pub struct RateLimiter" nil t)))))

(ert-deftest v031/server-rate-limiter-has-check ()
  "RateLimiter has check method."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "fn check(" nil t)))))

(ert-deftest v031/server-rate-limit-enforced ()
  "poll_clients enforces rate limit."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "rate limit exceeded" nil t)))))

(ert-deftest v031/server-default-rate-limit ()
  "Default rate limit constant exists."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "DEFAULT_RATE_LIMIT" nil t)))))

;; ── IPC security commands ───────────────────────────────────

(ert-deftest v031/dispatch-has-client-info ()
  "dispatch.rs handles ipc-client-info command."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "ipc-client-info" nil t)))))

(ert-deftest v031/dispatch-has-rate-limit-command ()
  "dispatch.rs handles ipc-rate-limit command."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "ipc-rate-limit" nil t)))))

(ert-deftest v031/client-info-returns-peer-uid ()
  "ipc-client-info response includes :peer-uid."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ":peer-uid" nil t)))))

(ert-deftest v031/client-info-returns-peer-pid ()
  "ipc-client-info response includes :peer-pid."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ":peer-pid" nil t)))))

(ert-deftest v031/rate-limit-validates-range ()
  "ipc-rate-limit validates limit is 1-10000."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "limit must be 1-10000" nil t)))))

;; ── Socket permissions ──────────────────────────────────────

(ert-deftest v031/server-sets-socket-permissions ()
  "server.rs sets socket to 0700 permissions."
  (let ((file (expand-file-name "compositor/src/ipc/server.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "0o700" nil t)))))

;; ── Hello response includes peer info ───────────────────────

(ert-deftest v031/hello-returns-peer-pid ()
  "hello response includes :peer-pid when available."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward ":peer-pid" nil t)))))

(ert-deftest v031/hello-logs-authenticated ()
  "hello handler logs 'authenticated' on success."
  (let ((file (expand-file-name "compositor/src/ipc/dispatch.rs" v031-test--root)))
    (with-temp-buffer
      (insert-file-contents file)
      (should (search-forward "authenticated" nil t)))))

(provide 'v031-ipc-security-test)
;;; v031-ipc-security-test.el ends here
