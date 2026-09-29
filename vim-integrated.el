;;; vim-integrated.el --- Vim integration with other modes -*- lexical-binding: t; -*-

;;; Commentary:

;; Override maps of vim maps for major modes like `dired-mode'.

;;; Code:

(require 'vim)

(defun vim-integrated-merge (major modes map keys)
  "Set KEYS in the MODES override maps of major mode MAJOR as bound in MAP.
Each element of KEYS is either KEY or (FROM . KEY), where FROM is the key
in MAP; KEY alone is (KEY . KEY).  MODES is as in `vim-major-mode-map-set'."
  (let (bindings)
    (dolist (key keys)
      (let* ((key (if (consp key) key (cons key key)))
             (definition (keymap-lookup map (car key))))
        (when (and definition (not (numberp definition)))
          (setq bindings (nconc bindings (list (cdr key) definition))))))
    (apply #'vim-major-mode-map-set major modes bindings)))

;;; dired

(defvar dired-mode-map)

(defvar vim-integrated-dired-keys
  '(("n" . "j") ("p" . "k")
    "c" "C" "d" "D" "m" "o" "O" "r" "R" "s" "S" "t" "T" "u" "U" "x" "X" "%" "=" "~")
  "Keys merged from `dired-mode-map'.")

(vim-define-major-mode-map dired-mode)

(with-eval-after-load 'dired
  (vim-integrated-merge 'dired-mode '(normal visual) dired-mode-map vim-integrated-dired-keys))

;;; ibuffer

(defvar ibuffer-mode-map)

(defvar vim-integrated-ibuffer-keys
  '(("n" . "j") ("p" . "k")
    "d" "D" "m" "o" "O" "s" "S" "t" "u" "U" "x" "%")
  "Keys merged from `ibuffer-mode-map'.")

(vim-define-major-mode-map ibuffer-mode)

(with-eval-after-load 'ibuffer
  (vim-integrated-merge 'ibuffer-mode '(normal visual) ibuffer-mode-map vim-integrated-ibuffer-keys))

;;; archive

(defvar archive-mode-map)

(defvar vim-integrated-archive-keys
  '(("n" . "j") ("p" . "k")
    "C" "m" "o" "u")
  "Keys merged from `archive-mode-map'.")

(vim-define-major-mode-map archive-mode)

(with-eval-after-load 'arc-mode
  (vim-integrated-merge 'archive-mode '(normal visual) archive-mode-map vim-integrated-archive-keys))

;;; image

(defvar image-mode-map)
(declare-function image-next-line "image-mode")
(declare-function image-previous-line "image-mode")
(declare-function image-backward-hscroll "image-mode")
(declare-function image-forward-hscroll "image-mode")
(declare-function image-bob "image-mode")
(declare-function image-eob "image-mode")

(defvar vim-integrated-image-keys
  '("m" "u")
  "Keys merged from `image-mode-map'.")

(vim-define-major-mode-map image-mode)

(with-eval-after-load 'image-mode
  (vim-integrated-merge 'image-mode '(normal visual) image-mode-map vim-integrated-image-keys)
  (keymap-set image-mode-map "<remap> <vim-h>" #'image-backward-hscroll)
  (keymap-set image-mode-map "<remap> <vim-j>" #'image-next-line)
  (keymap-set image-mode-map "<remap> <vim-k>" #'image-previous-line)
  (keymap-set image-mode-map "<remap> <vim-l>" #'image-forward-hscroll)
  (keymap-set image-mode-map "<remap> <vim-gg>" #'image-bob)
  (keymap-set image-mode-map "<remap> <vim-G>" #'image-eob))

(provide 'vim-integrated)
;;; vim-integrated.el ends here
