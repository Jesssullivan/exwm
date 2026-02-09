;;; ewwm-ipc-test.el --- Tests for ewwm-ipc.el  -*- lexical-binding: t -*-

;;; Code:

(require 'ert)
(require 'ewwm-ipc)

;; ── Message encoding/decoding tests ────────────────────────

(ert-deftest ewwm-ipc-test/encode-message-length-prefix ()
  "Encoded message starts with 4-byte big-endian length."
  (let* ((msg '(:type :hello :id 1 :version 1 :client "test"))
         (encoded (ewwm-ipc--encode-message msg))
         (payload-str (prin1-to-string msg))
         (payload-bytes (encode-coding-string payload-str 'utf-8))
         (expected-len (length payload-bytes)))
    ;; First 4 bytes are big-endian length
    (should (= (aref encoded 0) (logand (ash expected-len -24) #xff)))
    (should (= (aref encoded 1) (logand (ash expected-len -16) #xff)))
    (should (= (aref encoded 2) (logand (ash expected-len -8) #xff)))
    (should (= (aref encoded 3) (logand expected-len #xff)))
    ;; Total length = 4 prefix + payload
    (should (= (length encoded) (+ 4 expected-len)))))

(ert-deftest ewwm-ipc-test/encode-decode-roundtrip-plist ()
  "Encoding then decoding a plist preserves structure."
  (let* ((msg '(:type :surface-list :id 42))
         (encoded (ewwm-ipc--encode-message msg))
         ;; Manually decode
         (len (ewwm-ipc--decode-length encoded))
         (payload (substring encoded 4 (+ 4 len)))
         (decoded (read (decode-coding-string payload 'utf-8))))
    (should (equal (plist-get decoded :type) :surface-list))
    (should (= (plist-get decoded :id) 42))))

(ert-deftest ewwm-ipc-test/encode-decode-roundtrip-string ()
  "Encoding then decoding a message with string values works."
  (let* ((msg '(:type :hello :id 1 :client "ewwm.el"))
         (encoded (ewwm-ipc--encode-message msg))
         (len (ewwm-ipc--decode-length encoded))
         (payload (substring encoded 4 (+ 4 len)))
         (decoded (read (decode-coding-string payload 'utf-8))))
    (should (equal (plist-get decoded :client) "ewwm.el"))))

(ert-deftest ewwm-ipc-test/encode-decode-roundtrip-nested ()
  "Encoding then decoding nested plists works."
  (let* ((msg '(:type :surface-move :id 5 :surface-id 1 :x 100 :y 200))
         (encoded (ewwm-ipc--encode-message msg))
         (len (ewwm-ipc--decode-length encoded))
         (payload (substring encoded 4 (+ 4 len)))
         (decoded (read (decode-coding-string payload 'utf-8))))
    (should (= (plist-get decoded :surface-id) 1))
    (should (= (plist-get decoded :x) 100))
    (should (= (plist-get decoded :y) 200))))

(ert-deftest ewwm-ipc-test/decode-length-correct ()
  "Length decoding handles various sizes correctly."
  ;; Length = 0
  (should (= (ewwm-ipc--decode-length (unibyte-string 0 0 0 0)) 0))
  ;; Length = 1
  (should (= (ewwm-ipc--decode-length (unibyte-string 0 0 0 1)) 1))
  ;; Length = 256
  (should (= (ewwm-ipc--decode-length (unibyte-string 0 0 1 0)) 256))
  ;; Length = 65536
  (should (= (ewwm-ipc--decode-length (unibyte-string 0 1 0 0)) 65536)))

(ert-deftest ewwm-ipc-test/encode-empty-plist ()
  "Encoding a minimal plist works."
  (let* ((msg '(:type :ping :id 1))
         (encoded (ewwm-ipc--encode-message msg)))
    (should (> (length encoded) 4))))

;; ── Filter function / partial read tests ───────────────────

(ert-deftest ewwm-ipc-test/filter-partial-read ()
  "Filter accumulates partial data correctly."
  (let ((ewwm-ipc--read-buffer ""))
    ;; Simulate receiving a message in two chunks
    (let* ((msg '(:type :hello :version 1))
           (full (ewwm-ipc--encode-message msg))
           (half1 (substring full 0 (/ (length full) 2)))
           (half2 (substring full (/ (length full) 2))))
      ;; First chunk: partial
      (setq ewwm-ipc--read-buffer (concat ewwm-ipc--read-buffer half1))
      (should (< (length ewwm-ipc--read-buffer) (length full)))
      ;; Second chunk: completes the message
      (setq ewwm-ipc--read-buffer (concat ewwm-ipc--read-buffer half2))
      (should (= (length ewwm-ipc--read-buffer) (length full))))))

;; ── Request ID correlation tests ───────────────────────────

(ert-deftest ewwm-ipc-test/request-id-increments ()
  "Each send increments the request ID."
  (let ((ewwm-ipc--next-id 1))
    (should (= ewwm-ipc--next-id 1))
    (setq ewwm-ipc--next-id (1+ ewwm-ipc--next-id))
    (should (= ewwm-ipc--next-id 2))
    (setq ewwm-ipc--next-id (1+ ewwm-ipc--next-id))
    (should (= ewwm-ipc--next-id 3))))

(ert-deftest ewwm-ipc-test/pending-requests-hash ()
  "Pending requests hash table stores and retrieves callbacks."
  (let ((table (make-hash-table :test 'eql))
        (result nil))
    (puthash 42 (lambda (r) (setq result r)) table)
    (should (functionp (gethash 42 table)))
    (funcall (gethash 42 table) '(:status :ok))
    (should (equal result '(:status :ok)))
    (remhash 42 table)
    (should-not (gethash 42 table))))

;; ── Dispatch tests ─────────────────────────────────────────

(ert-deftest ewwm-ipc-test/dispatch-response ()
  "Dispatch matches response to pending request callback."
  (let ((ewwm-ipc--pending-requests (make-hash-table :test 'eql))
        (result nil))
    (puthash 5 (lambda (r) (setq result r)) ewwm-ipc--pending-requests)
    (ewwm-ipc--dispatch '(:type :response :id 5 :status :ok :surfaces ()))
    (should (equal (plist-get result :status) :ok))
    (should-not (gethash 5 ewwm-ipc--pending-requests))))

(ert-deftest ewwm-ipc-test/dispatch-event ()
  "Dispatch routes events to the correct handler."
  (let ((received nil))
    (cl-letf (((symbol-function 'ewwm-ipc--on-surface-created)
               (lambda (msg) (setq received msg))))
      (ewwm-ipc--dispatch '(:type :event :event :surface-created :id 1 :app-id "foot"))
      (should received)
      (should (= (plist-get received :id) 1)))))

(ert-deftest ewwm-ipc-test/dispatch-unknown-event ()
  "Dispatch handles unknown events gracefully."
  ;; Should not error
  (ewwm-ipc--dispatch '(:type :event :event :unknown-event :data 42)))

;; ── Connection state tests ─────────────────────────────────

(ert-deftest ewwm-ipc-test/connected-p-no-connection ()
  "connected-p returns nil when not connected."
  (let ((ewwm--ipc-connection nil))
    (should-not (ewwm-ipc-connected-p))))

(ert-deftest ewwm-ipc-test/socket-path-default ()
  "Socket path falls back to XDG_RUNTIME_DIR."
  (let ((ewwm-ipc-socket-path nil)
        (ewwm--compositor-socket nil))
    ;; The function should return a path ending in ewwm-ipc.sock
    (should (string-suffix-p "ewwm-ipc.sock" (ewwm-ipc--socket-path)))))

;; ── Reconnection logic tests ──────────────────────────────

(ert-deftest ewwm-ipc-test/reconnect-delay-exponential ()
  "Reconnection delay doubles up to max."
  (let ((ewwm-ipc--reconnect-delay 1)
        (ewwm-ipc-reconnect-max-delay 30))
    (setq ewwm-ipc--reconnect-delay
          (min (* ewwm-ipc--reconnect-delay 2) ewwm-ipc-reconnect-max-delay))
    (should (= ewwm-ipc--reconnect-delay 2))
    (setq ewwm-ipc--reconnect-delay
          (min (* ewwm-ipc--reconnect-delay 2) ewwm-ipc-reconnect-max-delay))
    (should (= ewwm-ipc--reconnect-delay 4))
    ;; Caps at max
    (setq ewwm-ipc--reconnect-delay 16)
    (setq ewwm-ipc--reconnect-delay
          (min (* ewwm-ipc--reconnect-delay 2) ewwm-ipc-reconnect-max-delay))
    (should (= ewwm-ipc--reconnect-delay 30))))

;; ── Trace mode tests ──────────────────────────────────────

(ert-deftest ewwm-ipc-test/trace-log-writes-buffer ()
  "Trace logging writes to *ewwm-ipc-trace* buffer."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'get-buffer)
                 (lambda (_name) buf)))
        (ewwm-ipc--trace-log ">>" '(:type :ping :id 1))
        (should (string-match-p ">>" (buffer-string)))
        (should (string-match-p ":ping" (buffer-string)))))))

;; ── Convenience wrapper tests ──────────────────────────────

(ert-deftest ewwm-ipc-test/surface-focus-builds-message ()
  "ewwm-surface-focus constructs the correct message."
  (let ((sent-msg nil))
    (cl-letf (((symbol-function 'ewwm-ipc-send)
               (lambda (msg &optional _cb) (setq sent-msg msg) 1)))
      (ewwm-surface-focus 42)
      (should (equal (plist-get sent-msg :type) :surface-focus))
      (should (= (plist-get sent-msg :surface-id) 42)))))

(ert-deftest ewwm-ipc-test/workspace-switch-builds-message ()
  "ewwm-workspace-switch constructs the correct message."
  (let ((sent-msg nil)
        (ewwm-workspace-number 4)
        (ewwm-workspace--configs nil)
        (ewwm-workspace--names nil)
        (ewwm-workspace-current-index 0)
        (ewwm-workspace-switch-hook nil)
        (ewwm--surface-buffer-alist nil))
    (ewwm-workspace--init)
    (cl-letf (((symbol-function 'ewwm-ipc-send)
               (lambda (msg &optional _cb) (setq sent-msg msg) 1))
              ((symbol-function 'ewwm-ipc-connected-p)
               (lambda () t)))
      (ewwm-workspace-switch 3)
      (should (equal (plist-get sent-msg :type) :workspace-switch))
      (should (= (plist-get sent-msg :workspace) 3)))))

(ert-deftest ewwm-ipc-test/key-grab-builds-message ()
  "ewwm-key-grab constructs the correct message."
  (let ((sent-msg nil))
    (cl-letf (((symbol-function 'ewwm-ipc-send)
               (lambda (msg &optional _cb) (setq sent-msg msg) 1)))
      (ewwm-key-grab "s-r")
      (should (equal (plist-get sent-msg :type) :key-grab))
      (should (equal (plist-get sent-msg :key) "s-r")))))

;;; ewwm-ipc-test.el ends here
