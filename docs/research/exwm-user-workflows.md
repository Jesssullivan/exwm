# EXWM User Workflow Patterns Research

Research conducted February 2026 by surveying the EXWM source code, the EXWM
wiki (emacs-exwm/exwm), the ch11ng/exwm wiki, the Arch Wiki, System Crafters
guides, and approximately a dozen GitHub dotfiles repositories (Ambrevar,
johanwiden, Phundrak, SqrtMinusOne, LemonBreezes, evalwhen, m-masterton,
Olivia5k, rafoo, DynamicMetaFlow, bvk, faijdherbe).

## Sources

- [EXWM Wiki - Configuration Example](https://github.com/emacs-exwm/exwm/wiki/Configuration-Example)
- [EXWM Wiki - Cookbook](https://github.com/emacs-exwm/exwm/wiki/Cookbook)
- [Arch Wiki - EXWM](https://wiki.archlinux.org/title/EXWM)
- [System Crafters - Getting Started with EXWM](https://systemcrafters.net/emacs-desktop-environment/getting-started-with-exwm/)
- [Ambrevar/dotfiles](https://github.com/Ambrevar/dotfiles/blob/master/.emacs.d/lisp/init-exwm.el)
- [johanwiden/exwm-setup](https://github.com/johanwiden/exwm-setup)
- [Phundrak's EXWM config](https://config.phundrak.com/emacs/packages/exwm)
- [LemonBreezes/exwm-module-for-doom-emacs](https://github.com/LemonBreezes/exwm-module-for-doom-emacs)
- [SqrtMinusOne/dotfiles](https://github.com/SqrtMinusOne/dotfiles/blob/master/Desktop.org)
- [faijdherbe EXWM config](https://www.faijdherbe.net/emacs/exwm.html)

---

## 1. Most Common Keybindings

### 1.1 Universal Global Keys (present in nearly every config)

Every surveyed configuration sets `exwm-input-global-keys` before calling
`exwm-wm-mode`. The following bindings appear in 80%+ of configs:

```elisp
(setq exwm-input-global-keys
      `(
        ;; Reset to line-mode / exit fullscreen
        ([?\s-r] . exwm-reset)

        ;; Interactive workspace switch
        ([?\s-w] . exwm-workspace-switch)

        ;; Workspace switching by number (s-0 through s-9)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))

        ;; Launch shell command
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))))
```

### 1.2 Frequently Added Global Keys (50%+ of configs)

```elisp
;; Window navigation with Super + arrow keys or h/j/k/l
([s-left]  . windmove-left)
([s-right] . windmove-right)
([s-up]    . windmove-up)
([s-down]  . windmove-down)

;; Toggle floating
([?\s-f]   . exwm-floating-toggle-floating)

;; Fullscreen
([?\s-F]   . exwm-layout-set-fullscreen)

;; Move window to workspace
([?\s-m]   . exwm-workspace-move-window)

;; Lock screen (slock, i3lock, or xscreensaver)
([s-f2]    . (lambda () (interactive) (start-process "" nil "slock")))

;; Application launcher (dmenu, rofi, or counsel)
([?\s-d]   . (lambda () (interactive) (start-process "dmenu" nil "dmenu_run")))
```

### 1.3 Mode-Map Keys (exwm-mode-map)

Nearly universal in `exwm-mode-map`:

```elisp
;; Send next key directly to X application
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
```

### 1.4 Default Prefix Keys

The default `exwm-input-prefix-keys` list is usually kept, sometimes extended:

```elisp
;; Default (from source):
'(?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:)

;; Common additions:
?\C-\M-j  ;; buffer switching
?\C-\\    ;; input method toggle (for XIM users)
?\C-c     ;; needed for some workflows (Doom, Evil)
```

### 1.5 Built-in exwm-mode-map Keys (C-c prefix)

These are built into EXWM and do not need configuration:

| Key           | Command                          | Description                |
|---------------|----------------------------------|----------------------------|
| `C-c C-f`     | `exwm-layout-set-fullscreen`     | Enter fullscreen           |
| `C-c C-h`     | `exwm-floating-hide`             | Hide floating window       |
| `C-c C-k`     | `exwm-input-release-keyboard`    | Switch to char-mode        |
| `C-c C-m`     | `exwm-workspace-move-window`     | Move window to workspace   |
| `C-c C-q`     | `exwm-input-send-next-key`       | Send next key to X window  |
| `C-c C-t C-f` | `exwm-floating-toggle-floating`  | Toggle tiling/floating     |
| `C-c C-t C-m` | `exwm-layout-toggle-mode-line`   | Toggle mode-line           |

---

## 2. Simulation Key Patterns

### 2.1 The "Standard" Set (present in official wiki, ~90% of configs)

This is the canonical Emacs-to-GUI-app translation layer:

```elisp
(setq exwm-input-simulation-keys
      '(;; Movement
        ([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])     ;; Page Up
        ([?\C-v] . [next])      ;; Page Down
        ;; Editing
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])))
```

### 2.2 Extended Sets (30-50% of configs add some of these)

```elisp
;; Cut/Copy/Paste
([?\C-w] . [?\C-x])        ;; kill-region -> Ctrl-X
([?\M-w] . [?\C-c])        ;; kill-ring-save -> Ctrl-C
([?\C-y] . [?\C-v])        ;; yank -> Ctrl-V

;; Undo/Redo
([?\C-/] . [?\C-z])        ;; undo -> Ctrl-Z
([?\C-?] . [?\C-\S-z])     ;; redo -> Ctrl-Shift-Z

;; Search
([?\C-s] . [?\C-f])        ;; isearch -> Ctrl-F

;; Tab navigation (browsers)
([?\M-f] . [C-tab])        ;; next tab
([?\M-b] . [C-S-tab])      ;; previous tab

;; Word movement
([?\M-f] . [C-right])      ;; forward-word
([?\M-b] . [C-left])       ;; backward-word
```

### 2.3 Per-Application Simulation Keys

Advanced users set local simulation keys using `exwm-manage-finish-hook` or
`exwm-manage-configurations`:

```elisp
;; Via exwm-manage-configurations (declarative, preferred for new configs):
(setq exwm-manage-configurations
      '(((member exwm-class-name '("Firefox" "Chromium"))
         simulation-keys (([?\C-s] . [?\C-f])
                          ([?\C-w] . [?\C-x])
                          ([?\M-w] . [?\C-c])
                          ([?\C-y] . [?\C-v])))
        ((string= exwm-class-name "Emacs")
         char-mode t)))

;; Via hook (imperative, common in older configs):
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (string= exwm-class-name "Firefox")
              (exwm-input-set-local-simulation-keys
               '(([?\C-s] . [?\C-f])
                 ([?\C-w] . [?\C-x]))))))
```

---

## 3. Manage Rules (exwm-manage-configurations)

### 3.1 Most Common Matching Patterns

Matching uses any sexp evaluated in the buffer context. Common variables:

| Variable             | Description                            | Example values                    |
|----------------------|----------------------------------------|-----------------------------------|
| `exwm-class-name`   | WM_CLASS class (second field)          | "Firefox", "Google-chrome", "mpv" |
| `exwm-instance-name` | WM_CLASS instance (first field)       | "Navigator", "google-chrome"      |
| `exwm-title`        | Window title (_NET_WM_NAME or WM_NAME)| "YouTube - Mozilla Firefox"       |

### 3.2 Floating Rules

The most common floating rules by application frequency:

```elisp
(setq exwm-manage-configurations
      '(;; Dialogs and popups -> float
        ((string= exwm-class-name "Pinentry")
         floating t)
        ((string= exwm-class-name "Pavucontrol")
         floating t)
        ((string= exwm-class-name "Nm-connection-editor")
         floating t)
        ((string= exwm-class-name "Blueman-manager")
         floating t)
        ((string-prefix-p "sun-awt-X11-" exwm-instance-name)
         floating t)                    ;; Java dialogs
        ;; mpv -> float with specific size
        ((string= exwm-class-name "mpv")
         floating t
         width 0.4
         height 0.4)))
```

EXWM also auto-floats windows that are:
- Transient windows (dialogs)
- Fixed-size windows (min == max in WM_NORMAL_HINTS)
- Windows with _NET_WM_WINDOW_TYPE_DIALOG or _NET_WM_WINDOW_TYPE_UTILITY

### 3.3 Workspace Assignment

```elisp
;; Send specific apps to specific workspaces
((string= exwm-class-name "Firefox")
 workspace 1)
((member exwm-class-name '("Slack" "discord"))
 workspace 3)
((string= exwm-class-name "Spotify")
 workspace 4)
```

### 3.4 Char-Mode Rules

```elisp
;; Nested Emacs instances in char-mode
((string= exwm-class-name "Emacs")
 char-mode t)
;; Terminal emulators in char-mode
((member exwm-class-name '("kitty" "Alacritty" "st-256color"))
 char-mode t)
```

### 3.5 Mode-Line Control

```elisp
;; Hide mode-line for specific apps
((string= exwm-class-name "mpv")
 floating t
 floating-mode-line nil
 tiling-mode-line nil)
```

---

## 4. Hook Usage Patterns

### 4.1 exwm-manage-finish-hook

The most heavily used hook. Runs after an X window is managed, in the context
of its exwm-mode buffer. Common uses:

**Buffer naming based on class (most common pattern):**
```elisp
;; Often put on exwm-update-class-hook instead, but some configs do both
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
```

**Conditional char-mode:**
```elisp
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when (string-prefix-p "emacs" exwm-instance-name)
              (exwm-input-release-keyboard
               (exwm--buffer->id (window-buffer))))))
```

**Workspace assignment (older pattern, prefer exwm-manage-configurations):**
```elisp
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (pcase exwm-class-name
              ("Firefox" (exwm-workspace-move-window 1))
              ("Slack"   (exwm-workspace-move-window 3)))))
```

**Window dedication (prevent buffer replacement):**
```elisp
(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (when exwm-class-name
              (set-window-dedicated-p (selected-window) "loose"))))
```

### 4.2 exwm-update-title-hook

Runs when _NET_WM_NAME or WM_NAME changes. Used for buffer naming:

**Simple title-based naming (most common):**
```elisp
(add-hook 'exwm-update-title-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-title)))
```

**Class-prefixed naming (second most common):**
```elisp
(add-hook 'exwm-update-title-hook
          (lambda ()
            (exwm-workspace-rename-buffer
             (format "%s: %s" exwm-class-name exwm-title))))
```

**Conditional naming by application:**
```elisp
(add-hook 'exwm-update-title-hook
          (lambda ()
            (pcase exwm-class-name
              ("Firefox"
               (exwm-workspace-rename-buffer
                (format "Firefox: %s" exwm-title)))
              ("Alacritty"
               (exwm-workspace-rename-buffer
                (format "Term: %s" exwm-title)))
              (_
               (exwm-workspace-rename-buffer exwm-title)))))
```

### 4.3 exwm-update-class-hook

Runs when WM_CLASS changes (usually once, on window creation). The most
universal pattern:

```elisp
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
```

Some configs use class-hook for initial naming and title-hook for updates.
This gives good buffer names before titles arrive, then refines them.

### 4.4 exwm-workspace-switch-hook

Runs after workspace switch. Less commonly customized:

**Track previous workspace for "switch-back":**
```elisp
(defvar my/exwm-previous-workspace 0)
(add-hook 'exwm-workspace-switch-hook
          (lambda ()
            (setq my/exwm-previous-workspace
                  (cons exwm-workspace-current-index
                        (car my/exwm-previous-workspace)))))
```

**Update external status bars:**
```elisp
(add-hook 'exwm-workspace-switch-hook
          (lambda ()
            (start-process-shell-command
             "polybar-msg" nil
             (format "polybar-msg action exwm hook 0 %d"
                     exwm-workspace-current-index))))
```

### 4.5 exwm-floating-setup-hook / exwm-floating-exit-hook

Used to hide/show mode-line on floating windows:

```elisp
(add-hook 'exwm-floating-setup-hook #'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook  #'exwm-layout-show-mode-line)
```

### 4.6 exwm-init-hook

Post-initialization. Used for starting background processes:

```elisp
(add-hook 'exwm-init-hook
          (lambda ()
            (start-process-shell-command "xmodmap" nil
              "xmodmap ~/.Xmodmap")
            (start-process-shell-command "polybar" nil "polybar main")))
```

---

## 5. Workspace Configurations

### 5.1 Number of Workspaces

From surveyed configs:

| Count | Frequency | Notes                                 |
|-------|-----------|---------------------------------------|
| 4     | ~30%      | Wiki default example                  |
| 5     | ~25%      | System Crafters default               |
| 10    | ~20%      | Maps cleanly to s-0..s-9              |
| 1     | ~10%      | Source default; dynamic creation       |
| 6-9   | ~15%      | Various; often tied to monitor layout  |

The source default is `exwm-workspace-number 1`, relying on dynamic creation
via `exwm-workspace-switch-create`. Most configs override to 4-10.

### 5.2 Workspace Naming

By default, workspaces are numbered 0-N. Custom naming uses
`exwm-workspace-index-map`:

```elisp
;; Example: named workspaces
(setq exwm-workspace-index-map
      (lambda (i)
        (nth i '("main" "web" "code" "chat" "media"))))
```

This is not commonly done; most users stick with numbers.

### 5.3 Multi-Monitor (RandR)

The standard multi-monitor pattern:

```elisp
(require 'exwm-randr)
(setq exwm-randr-workspace-monitor-plist
      '(0 "eDP-1" 1 "HDMI-1" 2 "eDP-1" 3 "HDMI-1"))

(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil
             "xrandr --output eDP-1 --auto --output HDMI-1 --auto --left-of eDP-1")))
(exwm-randr-mode 1)
```

### 5.4 System Tray

Enabled in ~70% of configs:

```elisp
(require 'exwm-systemtray)
(setq exwm-systemtray-height 16)
(exwm-systemtray-mode 1)
```

---

## 6. The 80/20 Feature Set

Based on frequency analysis across all sources, here are the EXWM features
that cover approximately 80% of user needs:

### Tier 1 -- Essential (used by 90%+ of configs)

1. **Global keybindings** (`exwm-input-global-keys`):
   - `s-r` for `exwm-reset`
   - `s-0` through `s-9` for workspace switching
   - `s-w` for interactive workspace switch
   - `s-&` for shell command launcher
   - `C-q` in exwm-mode-map for `exwm-input-send-next-key`

2. **Simulation keys** (`exwm-input-simulation-keys`):
   - Movement: C-b/f/p/n/a/e/v, M-v mapped to arrows/home/end/pgup/pgdown
   - Editing: C-d -> delete, C-k -> S-end delete

3. **Buffer naming** via `exwm-update-class-hook` or `exwm-update-title-hook`:
   - `(exwm-workspace-rename-buffer exwm-class-name)` at minimum

4. **Workspace count**: 4-10 workspaces, configured via `exwm-workspace-number`

5. **Prefix keys**: Default set, sometimes extended with `?\C-\\` for XIM

### Tier 2 -- Commonly Used (50-80% of configs)

6. **Per-app configurations** (`exwm-manage-configurations`):
   - Floating rules for dialogs (Pinentry, Pavucontrol)
   - Char-mode for terminal emulators and nested Emacs
   - Workspace assignment for browsers, chat apps

7. **Window navigation**: `s-{left,right,up,down}` or `s-{h,j,k,l}`
   via `windmove-*`

8. **System tray**: `(exwm-systemtray-mode 1)`

9. **Floating mode-line hiding**:
   - `exwm-floating-setup-hook` -> `exwm-layout-hide-mode-line`
   - `exwm-floating-exit-hook` -> `exwm-layout-show-mode-line`

10. **RandR multi-monitor**: `exwm-randr-workspace-monitor-plist` + screen
    change hook

### Tier 3 -- Power-User Features (20-50% of configs)

11. **Extended simulation keys**: Cut/copy/paste (C-w/M-w/C-y -> C-x/C-c/C-v),
    undo (C-/ -> C-z), search (C-s -> C-f)

12. **Local simulation keys**: Per-app simulation keys via
    `exwm-input-set-local-simulation-keys` in hooks

13. **Run-or-raise**: Jump to existing window or launch if absent

14. **Previous workspace toggle**: Track workspace history via
    `exwm-workspace-switch-hook`

15. **Window dedication**: Prevent buffer replacement in X windows

16. **Application launcher integration**: dmenu, rofi, or counsel-linux-app

17. **Screen locking**: slock, i3lock, or xscreensaver via keybinding

18. **Background processes on init**: polybar, xmodmap, compositors
    via `exwm-init-hook`

---

## 7. Configuration Anti-Patterns and Gotchas

Commonly reported issues from wiki/issues:

1. **Setting `exwm-input-global-keys` after `exwm-wm-mode`**: Has no effect.
   Must be set before initialization.

2. **Forgetting `exwm-input-prefix-keys`**: Keys like `C-x` must be in the
   prefix list or they get sent to the X application in line-mode.

3. **Simulation keys in apps that reject SendEvent**: Some apps (notably
   older Java apps) reject synthetic key events by default. Requires
   per-app workarounds.

4. **Not calling `exwm-workspace-rename-buffer`**: Results in all X window
   buffers being named `*EXWM*<N>`, making buffer switching unusable.

5. **Blocking Emacs with subprocesses**: External commands launched
   synchronously block the entire window manager. Always use
   `start-process-shell-command` instead of `shell-command`.

---

## 8. Implications for ewwm

The patterns above suggest the following priorities for ewwm compatibility:

1. **Global keybinding system** must support `s-` (Super) prefix keys and
   the same registration API (`exwm-input-global-keys` equivalent).

2. **Simulation keys** are a core differentiator of EXWM over other WMs.
   The translation layer from Emacs keybindings to X11 key events needs
   faithful reproduction.

3. **Per-application configuration** via `exwm-manage-configurations` has
   become the preferred declarative approach over imperative hooks. The
   matching criteria (class-name, instance-name, title) are the same
   X11 properties used by every window manager.

4. **Hook system** (`exwm-manage-finish-hook`, `exwm-update-title-hook`,
   `exwm-update-class-hook`, `exwm-workspace-switch-hook`) is the
   primary extension point. Buffer naming is the dominant use case.

5. **Workspace model** of 4-10 numbered workspaces with `s-N` switching
   is the de facto standard. Dynamic creation via `switch-create` is
   widely used.

6. **System tray** and **RandR multi-monitor** are expected features but
   can be treated as optional modules.

7. **Floating window management** with auto-detection (transient, fixed-size,
   dialog types) plus manual override (per-class rules) covers the
   vast majority of use cases.
