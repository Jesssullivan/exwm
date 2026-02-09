;;; ewwm-layout.el --- Layout management for EWWM  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;;; Commentary:
;; Tiling layout engine for ewwm. Layouts use Emacs window tree to tile
;; surfaces. Each layout algorithm determines the window split structure,
;; then assigns one ewwm buffer per window and sends geometry updates
;; to the compositor.

;;; Code:

(require 'cl-lib)
(require 'ewwm-core)

;; ── Customization ────────────────────────────────────────────

(defgroup ewwm-layout nil
  "EWWM layout management."
  :group 'ewwm)

(defcustom ewwm-layout-default 'tiling
  "Default layout mode."
  :type '(choice (const tiling) (const monocle) (const grid) (const floating))
  :group 'ewwm-layout)

(defcustom ewwm-layout-master-ratio 0.55
  "Ratio of master window width to total frame width."
  :type 'float
  :group 'ewwm-layout)

;; ── Hooks ────────────────────────────────────────────────────

(defvar ewwm-layout-change-hook nil
  "Hook run after layout changes. Called with the new layout symbol.")

;; ── Internal state ───────────────────────────────────────────

(defvar ewwm-layout--current 'tiling
  "Current layout mode.")

(defvar ewwm-layout--cycle-list '(tiling monocle grid)
  "List of layouts to cycle through.")

(defvar ewwm-layout--usable-area nil
  "Usable output area plist (:x N :y N :w N :h N).
Reduced by layer-shell exclusive zones (e.g., waybar).")

;; ── Layout application ───────────────────────────────────────

(defun ewwm-layout--apply-current (buffers)
  "Apply the current layout to BUFFERS.
BUFFERS is a list of ewwm-mode buffers for the current workspace.
Floating buffers are excluded from tiling.
Does nothing in batch mode (no display)."
  (when (and buffers (not noninteractive))
    (let ((tiling-bufs (cl-remove-if
                        (lambda (buf)
                          (eq (buffer-local-value 'ewwm-surface-state buf)
                              'floating))
                        buffers)))
      (when tiling-bufs
        (pcase ewwm-layout--current
          ('tiling (ewwm-layout--tiling tiling-bufs))
          ('monocle (ewwm-layout--monocle tiling-bufs))
          ('grid (ewwm-layout--grid tiling-bufs))
          ('floating nil))))))

(defun ewwm-layout--tiling (buffers)
  "Apply master-stack layout to BUFFERS.
Master window takes left portion, remaining stack on right."
  (when buffers
    (delete-other-windows)
    (switch-to-buffer (car buffers))
    (when (cdr buffers)
      ;; Split for master
      (let* ((master-width (round (* (frame-width) ewwm-layout-master-ratio)))
             (stack-bufs (cdr buffers)))
        (split-window-right master-width)
        (other-window 1)
        ;; Stack remaining buffers vertically
        (switch-to-buffer (car stack-bufs))
        (dolist (buf (cdr stack-bufs))
          (split-window-below)
          (other-window 1)
          (switch-to-buffer buf))
        ;; Return to master
        (select-window (frame-first-window))))
    (ewwm-layout--sync-geometry)))

(defun ewwm-layout--monocle (buffers)
  "Apply monocle layout: single buffer fills frame.
BUFFERS list determines which buffer is visible."
  (when buffers
    (delete-other-windows)
    (switch-to-buffer (car buffers))
    (ewwm-layout--sync-geometry)))

(defun ewwm-layout--grid (buffers)
  "Apply grid layout to BUFFERS: equal-sized grid via recursive splitting."
  (when buffers
    (delete-other-windows)
    (let* ((n (length buffers))
           (cols (ceiling (sqrt n)))
           (rows (ceiling (/ (float n) cols))))
      (switch-to-buffer (car buffers))
      ;; Create grid: first split into rows, then each row into columns
      (when (> rows 1)
        (dotimes (_ (1- rows))
          (split-window-below)))
      ;; Now split each row into columns
      (let ((win (frame-first-window))
            (idx 0))
        (dotimes (r rows)
          (let ((cols-this-row (min cols (- n (* r cols)))))
            (select-window win)
            (when (> cols-this-row 1)
              (dotimes (_ (1- cols-this-row))
                (split-window-right)))
            ;; Fill windows in this row
            (dotimes (c cols-this-row)
              (when (< idx n)
                (select-window (if (= c 0) win
                                 (next-window (selected-window) nil nil)))
                (switch-to-buffer (nth idx buffers))
                (setq idx (1+ idx))))
            ;; Move to next row
            (when (< r (1- rows))
              (setq win (next-window win nil nil))
              ;; Skip to next row's first window
              (dotimes (_ (1- cols-this-row))
                (setq win (next-window win nil nil)))))))
      (select-window (frame-first-window))
      (ewwm-layout--sync-geometry))))

;; ── Geometry sync to compositor ──────────────────────────────

(defun ewwm-layout--sync-geometry ()
  "Send geometry updates to compositor for all visible ewwm windows."
  (when (fboundp 'ewwm-ipc-send)
    (walk-windows
     (lambda (win)
       (let ((buf (window-buffer win)))
         (when (ewwm--surface-buffer-p buf)
           (let* ((edges (window-pixel-edges win))
                  (x (nth 0 edges))
                  (y (nth 1 edges))
                  (w (- (nth 2 edges) x))
                  (h (- (nth 3 edges) y))
                  (sid (buffer-local-value 'ewwm-surface-id buf)))
             ;; Update buffer-local geometry
             (with-current-buffer buf
               (setq ewwm-geometry `(:x ,x :y ,y :w ,w :h ,h)))
             ;; Send to compositor
             (funcall 'ewwm-ipc-send
                      `(:type :surface-resize
                              :surface-id ,sid
                              :geometry (:x ,x :y ,y :w ,w :h ,h)))))))
     nil t)))

;; ── Layout switching ─────────────────────────────────────────

(defun ewwm-layout-set (layout)
  "Set the current LAYOUT mode and re-layout."
  (interactive
   (list (intern (completing-read "Layout: "
                                  (mapcar #'symbol-name ewwm-layout--cycle-list)
                                  nil t))))
  (setq ewwm-layout--current layout)
  (ewwm-layout--apply-current
   (ewwm--buffers-on-workspace
    (if (boundp 'ewwm-workspace-current-index)
        ewwm-workspace-current-index
      0)))
  (run-hook-with-args 'ewwm-layout-change-hook layout)
  (message "ewwm: layout %s" layout))

(defun ewwm-layout-cycle ()
  "Cycle through available layouts."
  (interactive)
  (let* ((pos (cl-position ewwm-layout--current ewwm-layout--cycle-list))
         (next-pos (mod (1+ (or pos 0)) (length ewwm-layout--cycle-list)))
         (next (nth next-pos ewwm-layout--cycle-list)))
    (ewwm-layout-set next)))

(defun ewwm-layout-current ()
  "Return the current layout mode."
  ewwm-layout--current)

;; ── Usable area management ───────────────────────────────────

(defun ewwm-layout--set-usable-area (geometry)
  "Set the usable output area to GEOMETRY plist.
Called when layer-shell exclusive zones change."
  (setq ewwm-layout--usable-area geometry))

(provide 'ewwm-layout)
;;; ewwm-layout.el ends here
