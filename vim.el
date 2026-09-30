;;; vim.el --- Vim emulation -*- lexical-binding: t; -*-

;; Author: vhqr0 <zq_cmd@163.com>
;; URL: https://github.com/vhqr0/vim.el
;; Package-Requires: ((emacs "31.1"))
;; Version: 0.1.0
;; Keywords: emulations

;;; Commentary:

;; A small vim emulation built on Emacs commands.
;; Enable it with `vim-global-mode'.

;;; Code:

(require 'cl-lib)

(defgroup vim nil
  "Vim emulation."
  :group 'emulations)

(defvar vim-last nil
  "Last command for `vim-exec-last'.")
(defvar vim-pending-op-fn nil
  "Op function waiting for a move.")
(defvar vim-in-exec-last nil
  "Non-nil while repeating the last command.")

(defun vim-this-command-keep-pending-op-p ()
  "Return non-nil if `this-command' keeps the pending op."
  (and (symbolp this-command)
       (get this-command 'vim-keep-pending-op)))

(defun vim-pending-op-post-command ()
  "Clear the pending op after commands that do not keep it."
  (unless (vim-this-command-keep-pending-op-p)
    (setq vim-pending-op-fn nil)))

(dolist (cmd '(digit-argument negative-argument universal-argument universal-argument-more))
  (put cmd 'vim-keep-pending-op t))

(defvar-keymap vim-normal-mode-map)

(defvar-keymap vim-inner-tobj-map)

(defvar-keymap vim-outer-tobj-map)

(defvar-keymap vim-visual-mode-map
  :parent vim-normal-mode-map
  "i" vim-inner-tobj-map
  "a" vim-outer-tobj-map)

(defvar-keymap vim-insert-mode-map)

(defvar-keymap vim-op-base-map
  "i" vim-inner-tobj-map
  "a" vim-outer-tobj-map)

(defvar-local vim-normal-mode nil
  "Non-nil if vim normal mode is enabled.")

(defvar-local vim-visual-mode nil
  "Non-nil if vim visual mode is enabled.")

(defvar-local vim-insert-mode nil
  "Non-nil if vim insert mode is enabled.")

(add-to-list 'minor-mode-alist '(vim-normal-mode " <N>"))
(add-to-list 'minor-mode-alist '(vim-visual-mode " <V>"))
(add-to-list 'minor-mode-alist '(vim-insert-mode " <I>"))

(defvar-local vim-emulation-map-alist
  (list (cons 'vim-normal-mode vim-normal-mode-map)
        (cons 'vim-visual-mode vim-visual-mode-map)
        (cons 'vim-insert-mode vim-insert-mode-map))
  "Alist of vim maps in `emulation-mode-map-alists'.
It is set locally to use the override maps of the major mode.")

(defun vim-change-mode (&optional new-mode)
  "Change vim mode to NEW-MODE.
NEW-MODE is normal, visual, insert or nil for Emacs."
  (interactive)
  (setq-local vim-normal-mode (eq new-mode 'normal)
              vim-visual-mode (eq new-mode 'visual)
              vim-insert-mode (eq new-mode 'insert))
  (force-mode-line-update))

(defalias 'vim-change-mode-to-emacs 'vim-change-mode
  "Change to Emacs mode.")

(defun vim-change-mode-to-normal ()
  "Change to vim normal mode."
  (interactive)
  (vim-change-mode 'normal))

(defun vim-change-mode-to-visual ()
  "Change to vim visual mode."
  (interactive)
  (vim-change-mode 'visual))

(defun vim-change-mode-to-insert ()
  "Change to vim insert mode."
  (interactive)
  (vim-change-mode 'insert))

(defun vim-visual-post-command ()
  "Sync vim visual mode with the region."
  (if (and (region-active-p) (not deactivate-mark))
      (when (or vim-normal-mode vim-insert-mode)
        (vim-change-mode-to-visual))
    (when vim-visual-mode
      (vim-change-mode-to-normal))))

(defvar vim-major-mode-map-alist nil
  "Alist of (MAJOR . MAPS) overriding vim maps in major mode MAJOR.
MAPS is an alist of (MODE . MAP), where MODE is normal, visual or insert.
A major mode uses the MAPS of its nearest ancestor in this alist.")

(defun vim-define-major-mode-map (major)
  "Define normal, visual and insert override maps of major mode MAJOR.
Maps of the parent of MAJOR are defined first, and the maps of MAJOR
inherit them.  A major mode already in `vim-major-mode-map-alist' or a map
variable already bound is kept.  MAJOR must be loaded.  Return the maps."
  (or (alist-get major vim-major-mode-map-alist)
      (let ((parent-maps (if-let* ((parent (get major 'derived-mode-parent)))
                             (vim-define-major-mode-map parent)
                           (list (cons 'normal vim-normal-mode-map)
                                 (cons 'visual vim-visual-mode-map)
                                 (cons 'insert vim-insert-mode-map)))))
        (setf (alist-get major vim-major-mode-map-alist)
              (mapcar (lambda (parent-map)
                        (let* ((mode (car parent-map))
                               (name (intern (format "vim-%s-%s-override-map" major mode))))
                          (unless (boundp name)
                            (set-default name (define-keymap :parent (cdr parent-map)))
                            (put name 'variable-documentation
                                 (format "Vim %s override map of `%s'." mode major)))
                          (cons mode (symbol-value name))))
                      parent-maps)))))

(defun vim-major-mode-map-set (major modes &rest bindings)
  "Set BINDINGS in the MODES override maps of major mode MAJOR.
MODES is normal, visual or insert, or a list of them.
BINDINGS is a list of KEY DEFINITION pairs as in `keymap-set'."
  (let ((maps (alist-get major vim-major-mode-map-alist)))
    (unless maps
      (error "No override maps of %s" major))
    (dolist (mode (ensure-list modes))
      (let ((map (alist-get mode maps)))
        (unless map
          (error "No %s override map of %s" mode major))
        (cl-loop for (key definition) on bindings by #'cddr
                 do (keymap-set map key definition))))))

(defun vim-change-mode-to-default ()
  "Change to the default vim mode of the current buffer."
  (interactive)
  (if (minibufferp)
      (vim-change-mode-to-insert)
    (vim-change-mode-to-normal))
  (when-let* ((maps (seq-some (lambda (major)
                                (alist-get major vim-major-mode-map-alist))
                              (derived-mode-all-parents major-mode))))
    (setq-local vim-emulation-map-alist
                (list (cons 'vim-normal-mode (alist-get 'normal maps))
                      (cons 'vim-visual-mode (alist-get 'visual maps))
                      (cons 'vim-insert-mode (alist-get 'insert maps))))))

(defvar-keymap vim-global-mode-map)

;;;###autoload
(define-minor-mode vim-global-mode
  "Vim global mode."
  :global t
  (setq vim-pending-op-fn nil)
  (if vim-global-mode
      (progn
        (add-to-list 'emulation-mode-map-alists 'vim-emulation-map-alist)
        (add-hook 'post-command-hook #'vim-pending-op-post-command)
        (add-hook 'after-change-major-mode-hook #'vim-change-mode-to-default)
        (add-hook 'post-command-hook #'vim-visual-post-command))
    (setq emulation-mode-map-alists (delq 'vim-emulation-map-alist emulation-mode-map-alists))
    (remove-hook 'post-command-hook #'vim-pending-op-post-command)
    (remove-hook 'after-change-major-mode-hook #'vim-change-mode-to-default)
    (remove-hook 'post-command-hook #'vim-visual-post-command))
  (dolist (buffer (buffer-list))
    (unless (string-prefix-p " " (buffer-name buffer))
      (with-current-buffer buffer
        (if vim-global-mode
            (vim-change-mode-to-default)
          (vim-change-mode-to-emacs))))))

(defun vim-exit-visual ()
  "Exit vim visual mode."
  (interactive)
  (deactivate-mark))

(defun vim-exit-insert ()
  "Exit vim insert mode."
  (interactive)
  (vim-change-mode-to-normal)
  (unless (bolp)
    (backward-char 1)))

(keymap-set vim-normal-mode-map "<escape>" #'abort-recursive-edit)

(keymap-set vim-visual-mode-map "<escape>" #'vim-exit-visual)
(keymap-set vim-visual-mode-map "v" #'vim-exit-visual)

(keymap-set vim-insert-mode-map "<escape>" #'vim-exit-insert)

;;; exec

(defun vim-cmd (cmd-fn)
  "Call CMD-FN with the prefix count and record it for repeat."
  (let ((n (prefix-numeric-value current-prefix-arg)))
    (funcall cmd-fn n)
    (setq vim-last (list 'cmd n cmd-fn))))

(defun vim-op-on-region (op-fn)
  "Call OP-FN on the region."
  (let ((beg (region-beginning))
        (end (region-end)))
    (deactivate-mark)
    (funcall op-fn beg end)))

(defun vim-op (op-fn &optional transient-map)
  "Call OP-FN on the region, or make it pending.
When pending, activate TRANSIENT-MAP if non-nil."
  (if (region-active-p)
      (progn
        (vim-op-on-region op-fn)
        (setq vim-last (list 'region-op op-fn)))
    (setq vim-pending-op-fn op-fn
          prefix-arg current-prefix-arg)
    (when transient-map
      (set-transient-map transient-map #'vim-this-command-keep-pending-op-p))))

(defun vim-move-mark-region (n move-fn move-type)
  "Mark the region of MOVE-FN by N according to MOVE-TYPE."
  (set-mark (point))
  (funcall move-fn n)
  (unless (<= (mark) (point))
    (exchange-point-and-mark))
  (pcase move-type
    ;; j/k
    ('line
     (goto-char (line-end-position))
     (unless (eobp)
       (forward-char 1))
     (exchange-point-and-mark)
     (goto-char (line-beginning-position))
     (exchange-point-and-mark))
    ;; e
    ('inclusive
     (unless (eolp)
       (forward-char 1)))
    ;; h/l/w/b
    ('exclusive)
    ('tobj)))

(defun vim-op-on-move (n op-fn move-fn move-type)
  "Call OP-FN on the region of MOVE-FN by N according to MOVE-TYPE."
  (save-excursion
    (unwind-protect
        (progn
          (vim-move-mark-region n move-fn move-type)
          (vim-op-on-region op-fn))
      (deactivate-mark))))

(defun vim-move (move-fn move-type)
  "Call MOVE-FN, or apply the pending op on it with MOVE-TYPE."
  (let ((n (prefix-numeric-value current-prefix-arg)))
    (if (or (region-active-p) (null vim-pending-op-fn))
        (funcall move-fn n)
      (vim-op-on-move n vim-pending-op-fn move-fn move-type)
      (setq vim-last (list 'op n vim-pending-op-fn move-fn move-type)))))

(defun vim-exec-last (&optional n)
  "Repeat the last command N times."
  (interactive "p")
  (let ((vim-in-exec-last t))
    (dotimes (_ n)
      (when vim-last
        (pcase (car-safe vim-last)
          ('cmd
           (cl-destructuring-bind (n cmd-fn) (cdr-safe vim-last)
             (funcall cmd-fn n)))
          ('region-op
           (cl-destructuring-bind (op-fn) (cdr-safe vim-last)
             (when (region-active-p)
               (vim-op-on-region op-fn))))
          ('op
           (cl-destructuring-bind (n op-fn move-fn move-type) (cdr-safe vim-last)
             (if (region-active-p)
                 (vim-op-on-region op-fn)
               (vim-op-on-move n op-fn move-fn move-type)))))))))

(keymap-set vim-normal-mode-map "." #'vim-exec-last)

;;; move

(defmacro vim-define-move (key type move-fn)
  "Define a vim move command on KEY with TYPE and MOVE-FN."
  (let ((name (intern (concat "vim-" key))))
    `(progn
       (defun ,name ()
         ,(format "Vim move %s." key)
         (interactive)
         (vim-move ,move-fn ',type))
       (keymap-set vim-normal-mode-map ,(key-description key) #',name))))

(defun vim-next-line (n)
  "Move N lines forward."
  (line-move n t))

(defun vim-previous-line (n)
  "Move N lines backward."
  (vim-next-line (- n)))

(defun vim-forward-word-begin (n)
  "Move to the beginning of the next word N times."
  (if (< n 0)
      (backward-word (- n))
    (dotimes (_ n)
      (skip-syntax-forward "w")
      (skip-syntax-forward "^w"))))

(defun vim-backward-word-begin (n)
  "Move to the beginning of the previous word N times."
  (vim-forward-word-begin (- n)))

(defun vim-forward-word-end (n)
  "Move to the end of the next word N times."
  (if (< n 0)
      (dotimes (_ (- n))
        (when (looking-at "\\sw")
          (skip-syntax-backward "w"))
        (skip-syntax-backward "^w")
        (unless (bobp)
          (backward-char 1)))
    (dotimes (_ n)
      (forward-char 1)
      (skip-syntax-forward "^w")
      (skip-syntax-forward "w")
      (backward-char 1))))

(defun vim-backward-word-end (n)
  "Move to the end of the previous word N times."
  (vim-forward-word-end (- n)))

(defun vim-goto-next-sexp-begin ()
  "Move to the beginning of the next sexp."
  (let ((pt (point)))
    (condition-case nil
        (progn
          (forward-sexp 1)
          (backward-sexp 1)
          (when (<= (point) pt)
            (forward-sexp 2)
            (backward-sexp 1)
            (when (<= (point) pt)
              (goto-char (point-max)))))
      (scan-error
       (goto-char pt)
       (up-list 1)
       (vim-goto-next-sexp-begin)))))

(defun vim-goto-previous-sexp-end ()
  "Move to the end of the previous sexp."
  (let ((pt (point)))
    (condition-case nil
        (progn
          (backward-sexp 1)
          (forward-sexp 1)
          (when (> (point) pt)
            (backward-sexp 2)
            (forward-sexp 1)))
      (scan-error
       (goto-char pt)
       (backward-up-list 1)
       (vim-goto-previous-sexp-end)))))

(defun vim-forward-sexp-begin (n)
  "Move to the beginning of the next sexp N times."
  (goto-char
   (save-excursion
     (if (< n 0)
         (dotimes (_ (- n))
           (condition-case nil
               (backward-sexp 1)
             (scan-error
              (backward-up-list 1))))
       (dotimes (_ n)
         (vim-goto-next-sexp-begin)))
     (point))))

(defun vim-backward-sexp-begin (n)
  "Move to the beginning of the previous sexp N times."
  (vim-forward-sexp-begin (- n)))

(defun vim-forward-sexp-end (n)
  "Move to the end of the next sexp N times."
  (goto-char
   (save-excursion
     (if (< n 0)
         (dotimes (_ (- n))
           (vim-goto-previous-sexp-end)
           (backward-char 1))
       (dotimes (_ n)
         (unless (looking-at "\\s(")
           (forward-char 1))
         (condition-case nil
             (forward-sexp 1)
           (scan-error
            (up-list 1)))
         (backward-char 1)))
     (point))))

(defun vim-backward-sexp-end (n)
  "Move to the end of the previous sexp N times."
  (vim-forward-sexp-end (- n)))

(defun vim-jump-item (n)
  "Jump to the matching paren on the line.
N is ignored."
  (ignore n)
  (goto-char
   (save-excursion
     (skip-syntax-forward "^()" (line-end-position))
     (cond
      ((looking-at "\\s(")
       (vim-forward-sexp-end 1))
      ((looking-at "\\s)")
       (forward-char 1)
       (vim-backward-sexp-begin 1))
      (t
       (user-error "No paren on line")))
     (point))))

(defun vim-beginning-of-line (n)
  "Move to the beginning of line.
N is ignored."
  (ignore n)
  (beginning-of-line))

(defun vim-end-of-line (n)
  "Move to the last char of the Nth line."
  (end-of-line (if (< n 0) (+ n 2) n))
  (unless (bolp)
    (backward-char 1)))

(defun vim-back-to-indentation (n)
  "Move to the indentation.
N is ignored."
  (ignore n)
  (back-to-indentation))

(defun vim-goto-line (n)
  "Go to line N, counting from the end if negative."
  (if (< n 0)
      (progn
        (goto-char (point-max))
        (forward-line (1+ n)))
    (goto-char (point-min))
    (forward-line (1- n))))

(defun vim-goto-line-or-end (n)
  "Go to line N with a prefix arg, or to the end of buffer."
  (if current-prefix-arg
      (vim-goto-line n)
    (goto-char (point-max))))

(vim-define-move "h" exclusive #'backward-char)
(vim-define-move "l" exclusive #'forward-char)
(vim-define-move "j" line #'vim-next-line)
(vim-define-move "k" line #'vim-previous-line)
(vim-define-move "w" exclusive #'vim-forward-word-begin)
(vim-define-move "e" inclusive #'vim-forward-word-end)
(vim-define-move "b" exclusive #'vim-backward-word-begin)
(vim-define-move "ge" inclusive #'vim-backward-word-end)
(vim-define-move "W" exclusive #'vim-forward-sexp-begin)
(vim-define-move "E" inclusive #'vim-forward-sexp-end)
(vim-define-move "B" exclusive #'vim-backward-sexp-begin)
(vim-define-move "gE" inclusive #'vim-backward-sexp-end)
(vim-define-move "%" inclusive #'vim-jump-item)
(vim-define-move "0" exclusive #'vim-beginning-of-line)
(vim-define-move "^" exclusive #'vim-back-to-indentation)
(vim-define-move "$" inclusive #'vim-end-of-line)
(vim-define-move "gg" line #'vim-goto-line)
(vim-define-move "G" line #'vim-goto-line-or-end)

;;;; find

(defvar vim-find-last nil
  "Last find as (FIND-TYPE CHAR).")

(defun vim-read-find-char (find-type)
  "Read a find char and record it with FIND-TYPE."
  (if vim-in-exec-last
      (nth 1 vim-find-last)
    (let ((char (read-char "Find char: ")))
      (setq vim-find-last (list find-type char))
      char)))

(defun vim-find-forward (n char to)
  "Find CHAR forward N times on the line.
If TO is non-nil, stop before CHAR."
  (let ((pt (point))
        (string (char-to-string char)))
    (unless (if (< n 0)
                (progn
                  (goto-char (max (- pt (if (and to vim-in-exec-last) 1 0)) (line-beginning-position)))
                  (when (search-backward string (line-beginning-position) t (- n))
                    (when to
                      (forward-char 1))
                    t))
              (goto-char (min (+ pt (if (and to vim-in-exec-last) 2 1)) (line-end-position)))
              (when (search-forward string (line-end-position) t n)
                (backward-char (if to 2 1))
                t))
      (goto-char pt)
      (user-error "Can't find %c" char))))

(defun vim-find-backward (n char to)
  "Find CHAR backward N times on the line.
If TO is non-nil, stop after CHAR."
  (vim-find-forward (- n) char to))

(defun vim-find-char-forward (n)
  "Find a char forward N times."
  (vim-find-forward n (vim-read-find-char 'find-forward) nil))

(defun vim-find-char-backward (n)
  "Find a char backward N times."
  (vim-find-backward n (vim-read-find-char 'find-backward) nil))

(defun vim-till-char-forward (n)
  "Find till a char forward N times."
  (vim-find-forward n (vim-read-find-char 'till-forward) t))

(defun vim-till-char-backward (n)
  "Find till a char backward N times."
  (vim-find-backward n (vim-read-find-char 'till-backward) t))

(vim-define-move "f" inclusive #'vim-find-char-forward)
(vim-define-move "F" exclusive #'vim-find-char-backward)
(vim-define-move "t" inclusive #'vim-till-char-forward)
(vim-define-move "T" exclusive #'vim-till-char-backward)

(defun vim-find-moves (find-type)
  "Return the moves and reverse moves of FIND-TYPE."
  (pcase find-type
    ('find-forward '(vim-find-char-forward inclusive vim-find-char-backward exclusive))
    ('find-backward '(vim-find-char-backward exclusive vim-find-char-forward inclusive))
    ('till-forward '(vim-till-char-forward inclusive vim-till-char-backward exclusive))
    ('till-backward '(vim-till-char-backward exclusive vim-till-char-forward inclusive))))

(defun vim-\; ()
  "Repeat the last find."
  (interactive)
  (unless vim-find-last
    (user-error "No previous find"))
  (cl-destructuring-bind (move-fn move-type _rev-move-fn _rev-move-type) (vim-find-moves (car vim-find-last))
    (let ((vim-in-exec-last t))
      (vim-move move-fn move-type))))

(defun vim-\, ()
  "Repeat the last find in the reverse direction."
  (interactive)
  (unless vim-find-last
    (user-error "No previous find"))
  (cl-destructuring-bind (_move-fn _move-type rev-move-fn rev-move-type) (vim-find-moves (car vim-find-last))
    (let ((vim-in-exec-last t))
      (vim-move rev-move-fn rev-move-type))))

(keymap-set vim-normal-mode-map ";" #'vim-\;)
(keymap-set vim-normal-mode-map "," #'vim-\,)

;;;; search

(defvar vim-search-last nil
  "Last search as (REGEXP FORWARD).")

(defun vim-search-move (n regexp forward)
  "Search REGEXP N times.
Search forward if FORWARD is non-nil."
  (when (< n 0)
    (setq n (- n)
          forward (not forward)))
  (let ((pt (point)))
    (if forward
        (progn
          (unless (eobp)
            (forward-char 1))
          (if (re-search-forward regexp nil t n)
              (goto-char (match-beginning 0))
            (goto-char pt)
            (user-error "Search failed: %s" regexp)))
      (unless (re-search-backward regexp nil t n)
        (goto-char pt)
        (user-error "Search failed: %s" regexp)))))

(defun vim-search-next (n)
  "Repeat the last search N times."
  (unless vim-search-last
    (user-error "No previous search"))
  (cl-destructuring-bind (regexp forward) vim-search-last
    (vim-search-move n regexp forward)))

(defun vim-search-previous (n)
  "Repeat the last search N times in the reverse direction."
  (vim-search-next (- n)))

(vim-define-move "n" exclusive #'vim-search-next)
(vim-define-move "N" exclusive #'vim-search-previous)

(defun vim-search (forward)
  "Search with isearch, forward if FORWARD is non-nil."
  (let ((op-fn vim-pending-op-fn)
        (count current-prefix-arg)
        (pt (point)))
    (isearch-mode forward t nil t)
    (when (> (length isearch-string) 0)
      (setq vim-search-last
            (list (if isearch-regexp isearch-string (regexp-quote isearch-string))
                  forward)))
    (goto-char pt)
    (let ((vim-pending-op-fn op-fn)
          (current-prefix-arg count))
      (vim-move #'vim-search-next 'exclusive))))

(defun vim-/ ()
  "Search forward."
  (interactive)
  (vim-search t))

(defun vim-? ()
  "Search backward."
  (interactive)
  (vim-search nil))

(keymap-set vim-normal-mode-map "/" #'vim-/)
(keymap-set vim-normal-mode-map "?" #'vim-?)

;;; tobj

(defmacro vim-define-tobj (key kind move-fn)
  "Define a vim text object command on KEY of KIND with MOVE-FN.
KIND is inner or outer."
  (let ((name (intern (concat "vim-" (if (eq kind 'inner) "i" "a") key)))
        (map (if (eq kind 'inner) 'vim-inner-tobj-map 'vim-outer-tobj-map)))
    `(progn
       (defun ,name ()
         ,(format "Vim %s text object %s." kind key)
         (interactive)
         (vim-move ,move-fn 'tobj))
       (keymap-set ,map ,(key-description key) #',name))))

(defun vim-mark-thing (thing n outer)
  "Mark N THINGs at point.
If OUTER is non-nil, include surrounding spaces."
  (let ((bounds (bounds-of-thing-at-point thing))
        beg end)
    (unless bounds
      (user-error "No %s at point" thing))
    (save-excursion
      (setq beg (car bounds))
      (goto-char (cdr bounds))
      (when (> n 1)
        (forward-thing thing (1- n)))
      (setq end (point))
      (when outer
        (skip-chars-forward " \t")
        (if (> (point) end)
            (setq end (point))
          (goto-char beg)
          (skip-chars-backward " \t")
          (setq beg (point)))))
    (set-mark beg)
    (goto-char end)))

(defun vim-mark-inner-word (n)
  "Mark N inner words."
  (vim-mark-thing 'word n nil))

(defun vim-mark-outer-word (n)
  "Mark N outer words."
  (vim-mark-thing 'word n t))

(defun vim-mark-inner-sexp (n)
  "Mark N inner sexps."
  (vim-mark-thing 'sexp n nil))

(defun vim-mark-outer-sexp (n)
  "Mark N outer sexps."
  (vim-mark-thing 'sexp n t))

(defun vim-mark-inner-file (n)
  "Mark N inner file names."
  (vim-mark-thing 'filename n nil))

(defun vim-mark-outer-file (n)
  "Mark N outer file names."
  (vim-mark-thing 'filename n t))

(defun vim-mark-inner-line (n)
  "Mark N inner lines."
  (back-to-indentation)
  (set-mark (point))
  (end-of-line n))

(defun vim-mark-outer-line (n)
  "Mark N outer lines."
  (beginning-of-line)
  (set-mark (point))
  (forward-line n))

(defun vim-mark-inner-paragraph (n)
  "Mark N inner paragraphs."
  (mark-paragraph n)
  (skip-chars-forward " \t\n")
  (beginning-of-line))

(defun vim-mark-inner-defun (n)
  "Mark N inner defuns."
  (end-of-defun n)
  (set-mark (point))
  (beginning-of-defun n))

(defun vim-mark-whole-buffer (n)
  "Mark the whole buffer.
N is ignored."
  (ignore n)
  (set-mark (point-max))
  (goto-char (point-min)))

(defun vim-mark-pair (open close n outer)
  "Mark the Nth enclosing pair of OPEN and CLOSE.
If OUTER is non-nil, include the pair."
  (let ((regexp (regexp-opt (list (char-to-string open) (char-to-string close))))
        (depth 0)
        beg end)
    (save-excursion
      (when (eq (char-after) open)
        (forward-char 1))
      (while (< 0 n)
        (unless (re-search-backward regexp nil t)
          (user-error "No %c%c at point" open close))
        (if (eq (char-after) close)
            (setq depth (1+ depth))
          (if (< 0 depth)
              (setq depth (1- depth))
            (setq n (1- n)))))
      (setq beg (point))
      (forward-char 1)
      (while (<= 0 depth)
        (unless (re-search-forward regexp nil t)
          (user-error "No %c%c at point" open close))
        (if (eq (char-before) open)
            (setq depth (1+ depth))
          (setq depth (1- depth))))
      (setq end (point)))
    (set-mark (if outer beg (1+ beg)))
    (goto-char (if outer end (1- end)))))

(defun vim-mark-quote (char outer)
  "Mark the string quoted by CHAR at point.
If OUTER is non-nil, include the quotes."
  (let* ((ppss (syntax-ppss))
         (beg (cond
               ((nth 3 ppss) (nth 8 ppss))
               ((looking-at "\\s\"") (point))))
         end)
    (unless (and beg (eq (char-after beg) char))
      (user-error "No %c string at point" char))
    (save-excursion
      (goto-char beg)
      (forward-sexp 1)
      (setq end (point)))
    (set-mark (if outer beg (1+ beg)))
    (goto-char (if outer end (1- end)))))

(defun vim-mark-inner-dquote (n)
  "Mark the inner double quoted string.
N is ignored."
  (ignore n)
  (vim-mark-quote ?\" nil))

(defun vim-mark-outer-dquote (n)
  "Mark the outer double quoted string.
N is ignored."
  (ignore n)
  (vim-mark-quote ?\" t))

(defun vim-mark-inner-squote (n)
  "Mark the inner single quoted string.
N is ignored."
  (ignore n)
  (vim-mark-quote ?' nil))

(defun vim-mark-outer-squote (n)
  "Mark the outer single quoted string.
N is ignored."
  (ignore n)
  (vim-mark-quote ?' t))

(defun vim-mark-inner-paren (n)
  "Mark the Nth inner paren."
  (vim-mark-pair ?\( ?\) n nil))

(defun vim-mark-outer-paren (n)
  "Mark the Nth outer paren."
  (vim-mark-pair ?\( ?\) n t))

(defun vim-mark-inner-brace (n)
  "Mark the Nth inner brace."
  (vim-mark-pair ?{ ?} n nil))

(defun vim-mark-outer-brace (n)
  "Mark the Nth outer brace."
  (vim-mark-pair ?{ ?} n t))

(defun vim-mark-inner-bracket (n)
  "Mark the Nth inner bracket."
  (vim-mark-pair ?\[ ?\] n nil))

(defun vim-mark-outer-bracket (n)
  "Mark the Nth outer bracket."
  (vim-mark-pair ?\[ ?\] n t))

(defun vim-mark-inner-angle (n)
  "Mark the Nth inner angle."
  (vim-mark-pair ?< ?> n nil))

(defun vim-mark-outer-angle (n)
  "Mark the Nth outer angle."
  (vim-mark-pair ?< ?> n t))

(vim-define-tobj "w" inner #'vim-mark-inner-word)
(vim-define-tobj "w" outer #'vim-mark-outer-word)
(vim-define-tobj "W" inner #'vim-mark-inner-sexp)
(vim-define-tobj "W" outer #'vim-mark-outer-sexp)
(vim-define-tobj "f" inner #'vim-mark-inner-file)
(vim-define-tobj "f" outer #'vim-mark-outer-file)
(vim-define-tobj "l" inner #'vim-mark-inner-line)
(vim-define-tobj "l" outer #'vim-mark-outer-line)
(vim-define-tobj "p" inner #'vim-mark-inner-paragraph)
(vim-define-tobj "p" outer #'mark-paragraph)
(vim-define-tobj "d" inner #'vim-mark-inner-defun)
(vim-define-tobj "d" outer #'mark-defun)
(vim-define-tobj "h" inner #'vim-mark-whole-buffer)
(vim-define-tobj "h" outer #'vim-mark-whole-buffer)
(vim-define-tobj "b" inner #'vim-mark-inner-paren)
(vim-define-tobj "b" outer #'vim-mark-outer-paren)
(vim-define-tobj "B" inner #'vim-mark-inner-brace)
(vim-define-tobj "B" outer #'vim-mark-outer-brace)
(vim-define-tobj "r" inner #'vim-mark-inner-bracket)
(vim-define-tobj "r" outer #'vim-mark-outer-bracket)
(vim-define-tobj "a" inner #'vim-mark-inner-angle)
(vim-define-tobj "a" outer #'vim-mark-outer-angle)
(vim-define-tobj "q" inner #'vim-mark-inner-squote)
(vim-define-tobj "q" outer #'vim-mark-outer-squote)
(vim-define-tobj "Q" inner #'vim-mark-inner-dquote)
(vim-define-tobj "Q" outer #'vim-mark-outer-dquote)

(keymap-set vim-inner-tobj-map "(" #'vim-ib)
(keymap-set vim-outer-tobj-map "(" #'vim-ab)
(keymap-set vim-inner-tobj-map "{" #'vim-iB)
(keymap-set vim-outer-tobj-map "{" #'vim-aB)
(keymap-set vim-inner-tobj-map "[" #'vim-ir)
(keymap-set vim-outer-tobj-map "[" #'vim-ar)
(keymap-set vim-inner-tobj-map "<" #'vim-ia)
(keymap-set vim-outer-tobj-map "<" #'vim-aa)
(keymap-set vim-inner-tobj-map "'" #'vim-iq)
(keymap-set vim-outer-tobj-map "'" #'vim-aq)
(keymap-set vim-inner-tobj-map "\"" #'vim-iQ)
(keymap-set vim-outer-tobj-map "\"" #'vim-aQ)

;;; op

(keymap-set vim-op-base-map "j" #'vim-j)
(keymap-set vim-op-base-map "k" #'vim-k)
(keymap-set vim-op-base-map "p" #'vim-ip)
(keymap-set vim-op-base-map "o" #'vim-iW)
(keymap-set vim-op-base-map "m" #'vim-%)
(keymap-set vim-visual-mode-map "m" #'vim-%)

(defmacro vim-define-op (key op-fn)
  "Define a vim op command on KEY with OP-FN."
  (let ((name (intern (concat "vim-" key)))
        (map (intern (concat "vim-" key "-map"))))
    `(progn
       (defvar-keymap ,map
         :parent vim-op-base-map
         :doc ,(format "Vim op %s map." key)
         ,(key-description (substring key -1)) #'vim-al)
       (defun ,name ()
         ,(format "Vim op %s." key)
         (interactive)
         (vim-op ,op-fn ,map))
       (put ',name 'vim-keep-pending-op t)
       (keymap-set vim-normal-mode-map ,(key-description key) #',name))))

(defun vim-trim-region (beg end)
  "Return the bounds of BEG and END without surrounding blanks.
A trailing newline is also excluded."
  (save-excursion
    (goto-char beg)
    (skip-chars-forward " \t" end)
    (setq beg (point))
    (goto-char end)
    (when (and (bolp) (< beg end))
      (backward-char 1))
    (skip-chars-backward " \t" beg)
    (cons beg (max beg (point)))))

(defun vim-change-region (beg end)
  "Kill the trimmed region between BEG and END and insert."
  (let ((bounds (vim-trim-region beg end)))
    (goto-char (car bounds))
    (kill-region (car bounds) (cdr bounds)))
  (vim-change-mode-to-insert))

(defun vim-invert-case-region (beg end)
  "Invert the case of the region between BEG and END."
  (let* ((text (buffer-substring-no-properties beg end))
         (chars (mapcar (lambda (char)
                          (if (eq char (upcase char))
                              (downcase char)
                            (upcase char)))
                        text)))
    (replace-region-contents beg end (apply #'string chars))))

(defvar vim-replace-char nil
  "Last replace char.")

(defun vim-read-replace-char ()
  "Read a replace char."
  (if vim-in-exec-last
      vim-replace-char
    (setq vim-replace-char (read-char "Replace char: "))))

(defun vim-replace-region (beg end)
  "Replace chars between BEG and END with a read char."
  (let ((char (vim-read-replace-char)))
    (delete-region beg end)
    (goto-char beg)
    (save-excursion
      (insert-char char (- end beg)))))

(defun vim-put-region (beg end)
  "Replace the region between BEG and END with the last kill."
  (delete-region beg end)
  (goto-char beg)
  (yank))

(defun vim-join-region (beg end)
  "Join the lines of the trimmed region between BEG and END."
  (let ((bounds (vim-trim-region beg end)))
    (delete-indentation nil (car bounds) (cdr bounds))))

(defvar vim-eval-function-alist
  '((emacs-lisp-mode . eval-region))
  "Alist of (MAJOR . FUNCTION) to eval a region in major mode MAJOR.
FUNCTION is called with the beginning and end of the region.  A major
mode uses the FUNCTION of its nearest ancestor in this alist.")

(defun vim-eval-region (beg end)
  "Eval the region between BEG and END by `vim-eval-function-alist'."
  (if-let* ((eval-function (seq-some (lambda (major)
                                       (alist-get major vim-eval-function-alist))
                                     (derived-mode-all-parents major-mode))))
      (funcall eval-function beg end)
    (user-error "No eval function for %s" major-mode)))

(vim-define-op "d" #'kill-region)
(vim-define-op "c" #'vim-change-region)
(vim-define-op "y" #'copy-region-as-kill)
(vim-define-op "=" #'indent-region)
(vim-define-op "gc" #'comment-or-uncomment-region)
(vim-define-op "gu" #'downcase-region)
(vim-define-op "gU" #'upcase-region)
(vim-define-op "g~" #'vim-invert-case-region)
(vim-define-op "<" #'indent-rigidly-left-to-tab-stop)
(vim-define-op ">" #'indent-rigidly-right-to-tab-stop)
(vim-define-op "gJ" #'vim-join-region)
(vim-define-op "g-" #'narrow-to-region)
(vim-define-op "gy" #'vim-eval-region)

(keymap-set vim-visual-mode-map "u" #'vim-gu)
(keymap-set vim-visual-mode-map "U" #'vim-gU)

;;; cmd

(defmacro vim-define-cmd (key cmd-fn)
  "Define a vim cmd command on KEY with CMD-FN."
  (let ((name (intern (concat "vim-" key))))
    `(progn
       (defun ,name ()
         ,(format "Vim cmd %s." key)
         (interactive)
         (vim-cmd ,cmd-fn))
       (keymap-set vim-normal-mode-map ,(key-description key) #',name))))

(defun vim-kill-to-eol (n)
  "Kill to the end of the Nth line."
  (unless (region-active-p)
    (set-mark (line-end-position n)))
  (vim-op-on-region #'kill-region))

(defun vim-change-to-eol (n)
  "Change to the end of the Nth line."
  (unless (region-active-p)
    (set-mark (line-end-position n)))
  (vim-op-on-region #'vim-change-region))

(defun vim-copy-to-eol (n)
  "Copy to the end of the Nth line."
  (unless (region-active-p)
    (set-mark (line-end-position n)))
  (vim-op-on-region #'copy-region-as-kill))

(defun vim-delete-char (n)
  "Delete N chars forward."
  (unless (region-active-p)
    (set-mark (min (+ (point) n) (line-end-position))))
  (vim-op-on-region #'kill-region))

(defun vim-delete-backward-char (n)
  "Delete N chars backward."
  (unless (region-active-p)
    (set-mark (max (- (point) n) (line-beginning-position))))
  (vim-op-on-region #'kill-region))

(defun vim-substitute (n)
  "Change N chars."
  (set-mark (min (+ (point) n) (line-end-position)))
  (vim-op-on-region #'vim-change-region))

(defun vim-substitute-line (n)
  "Change N lines."
  (vim-mark-outer-line n)
  (vim-op-on-region #'vim-change-region))

(defun vim-replace (n)
  "Replace N chars with a read char."
  (unless (region-active-p)
    (set-mark (min (+ (point) n) (line-end-position))))
  (vim-op-on-region #'vim-replace-region))

(defun vim-invert-case (n)
  "Invert the case of N chars."
  (unless (region-active-p)
    (set-mark (point))
    (goto-char (min (+ (point) n) (line-end-position))))
  (vim-op-on-region #'vim-invert-case-region))

(defun vim-join-line (n)
  "Join N lines."
  (unless (region-active-p)
    (vim-mark-outer-line (1+ n)))
  (vim-op-on-region #'vim-join-region))

(vim-define-cmd "D" #'vim-kill-to-eol)
(vim-define-cmd "C" #'vim-change-to-eol)
(vim-define-cmd "Y" #'vim-copy-to-eol)
(vim-define-cmd "x" #'vim-delete-char)
(vim-define-cmd "X" #'vim-delete-backward-char)
(vim-define-cmd "s" #'vim-substitute)
(vim-define-cmd "S" #'vim-substitute-line)
(vim-define-cmd "r" #'vim-replace)
(vim-define-cmd "~" #'vim-invert-case)
(vim-define-cmd "J" #'vim-join-line)

;;;; insert

(defun vim-insert (n)
  "Insert before point.
N is ignored."
  (ignore n)
  (vim-change-mode-to-insert))

(defun vim-insert-at-indentation (n)
  "Insert at the indentation.
N is ignored."
  (ignore n)
  (back-to-indentation)
  (vim-change-mode-to-insert))

(defun vim-append (n)
  "Append after point.
N is ignored."
  (ignore n)
  (unless (eolp)
    (forward-char 1))
  (vim-change-mode-to-insert))

(defun vim-append-at-eol (n)
  "Append at the end of line.
N is ignored."
  (ignore n)
  (end-of-line)
  (vim-change-mode-to-insert))

(defun vim-open-line-below (n)
  "Open a line below and insert.
N is ignored."
  (ignore n)
  (end-of-line)
  (newline)
  (indent-according-to-mode)
  (vim-change-mode-to-insert))

(defun vim-open-line-above (n)
  "Open a line above and insert.
N is ignored."
  (ignore n)
  (beginning-of-line)
  (open-line 1)
  (indent-according-to-mode)
  (vim-change-mode-to-insert))

(vim-define-cmd "i" #'vim-insert)
(vim-define-cmd "I" #'vim-insert-at-indentation)
(vim-define-cmd "a" #'vim-append)
(vim-define-cmd "A" #'vim-append-at-eol)
(vim-define-cmd "o" #'vim-open-line-below)
(vim-define-cmd "O" #'vim-open-line-above)

;;;; put

(defun vim-end-with-newline-p (string)
  "Return non-nil if STRING ends with a newline."
  (string-suffix-p "\n" string))

(defun vim-put-after (n)
  "Put the last kill after point N times."
  (if (region-active-p)
      (vim-op-on-region #'vim-put-region)
    (save-excursion
      (if (vim-end-with-newline-p (current-kill 0))
          (progn
            (end-of-line)
            (if (eobp)
                (newline)
              (forward-char 1)))
        (unless (eolp)
          (forward-char 1)))
      (dotimes (_ n)
        (yank)))))

(defun vim-put-before (n)
  "Put the last kill before point N times."
  (if (region-active-p)
      (vim-op-on-region #'vim-put-region)
    (save-excursion
      (when (vim-end-with-newline-p (current-kill 0))
        (beginning-of-line))
      (dotimes (_ n)
        (yank)))))

(vim-define-cmd "p" #'vim-put-after)
(vim-define-cmd "P" #'vim-put-before)

;;; surround

(defvar vim-surround-alist
  '((?b ?\( ?\)) (?\( ?\( ?\)) (?\) ?\( ?\))
    (?B ?{ ?}) (?{ ?{ ?}) (?} ?{ ?})
    (?r ?\[ ?\]) (?\[ ?\[ ?\]) (?\] ?\[ ?\])
    (?a ?< ?>) (?< ?< ?>) (?> ?< ?>)
    (?q ?' ?') (?' ?' ?')
    (?Q ?\" ?\") (?\" ?\" ?\"))
  "Alist of surround chars to (OPEN CLOSE).")

(defvar vim-surround-from nil
  "Last surround char to replace.")
(defvar vim-surround-to nil
  "Last surround char to insert.")

(defun vim-read-surround-from ()
  "Read a surround char to replace."
  (if vim-in-exec-last
      vim-surround-from
    (setq vim-surround-from (read-char "Surround from: "))))

(defun vim-read-surround-to ()
  "Read a surround char to insert."
  (if vim-in-exec-last
      vim-surround-to
    (setq vim-surround-to (read-char "Surround to: "))))

(defun vim-surround-pair (char)
  "Return the surround pair of CHAR."
  (or (alist-get char vim-surround-alist)
      (list char char)))

(defun vim-surround-bounds (char n)
  "Return the bounds of the Nth surround of CHAR."
  (cl-destructuring-bind (open close) (vim-surround-pair char)
    (save-mark-and-excursion
      (if (eq open close)
          (vim-mark-quote open t)
        (vim-mark-pair open close n t))
      (cons (mark) (point)))))

(defun vim-delete-surround (n)
  "Delete the Nth surround."
  (let ((bounds (vim-surround-bounds (vim-read-surround-from) n)))
    (save-excursion
      (goto-char (cdr bounds))
      (delete-char -1)
      (goto-char (car bounds))
      (delete-char 1))))

(defun vim-change-surround (n)
  "Change the Nth surround."
  (let ((bounds (vim-surround-bounds (vim-read-surround-from) n)))
    (cl-destructuring-bind (open close) (vim-surround-pair (vim-read-surround-to))
      (save-excursion
        (goto-char (cdr bounds))
        (delete-char -1)
        (insert-char close)
        (goto-char (car bounds))
        (delete-char 1)
        (insert-char open)))))

(defun vim-surround-region (beg end)
  "Surround the trimmed region between BEG and END."
  (let ((bounds (vim-trim-region beg end)))
    (cl-destructuring-bind (open close) (vim-surround-pair (vim-read-surround-to))
      (save-excursion
        (goto-char (cdr bounds))
        (insert-char close)
        (goto-char (car bounds))
        (insert-char open)))))

(defun vim-ds ()
  "Delete surround."
  (interactive)
  (vim-cmd #'vim-delete-surround))

(defun vim-cs ()
  "Change surround."
  (interactive)
  (vim-cmd #'vim-change-surround))

(defvar-keymap vim-ys-map
  :parent vim-op-base-map
  "s" #'vim-il)

(defun vim-ys ()
  "Surround op."
  (interactive)
  (vim-op #'vim-surround-region vim-ys-map))

(put 'vim-ys 'vim-keep-pending-op t)

(keymap-set vim-d-map "s" #'vim-ds)
(keymap-set vim-c-map "s" #'vim-cs)
(keymap-set vim-y-map "s" #'vim-ys)

(keymap-set vim-visual-mode-map "s" #'vim-ys)
(keymap-set vim-visual-mode-map "S" #'vim-ys)

;;; misc

(defun vim-execute-in-emacs ()
  "Execute a key sequence without vim bindings."
  (interactive)
  (let* ((vim-normal-mode nil)
         (vim-visual-mode nil)
         (keys (read-key-sequence "Emacs: "))
         (cmd (key-binding keys)))
    (unless cmd
      (user-error "%s is undefined" (key-description keys)))
    (setq this-command cmd
          real-this-command cmd
          last-command-event (aref keys (1- (length keys))))
    (if (commandp cmd t)
        (call-interactively cmd)
      (execute-kbd-macro cmd))))

(defun vim-goto-last-change ()
  "Go to the position of the last change."
  (interactive)
  (when (eq buffer-undo-list t)
    (user-error "No undo information in this buffer"))
  (let ((pos (seq-some (lambda (entry)
                         (pcase entry
                           (`(,(and beg (pred integerp)) . ,(pred integerp)) beg)
                           (`(,(pred stringp) . ,pos) (abs pos))))
                       buffer-undo-list)))
    (unless pos
      (user-error "No change in this buffer"))
    (goto-char pos)))

(defun vim-select-line ()
  "Select lines."
  (interactive)
  (unless (region-active-p)
    (set-mark-command nil))
  (unless (<= (point) (mark))
    (exchange-point-and-mark))
  (goto-char (line-beginning-position))
  (exchange-point-and-mark)
  (goto-char (line-end-position))
  (unless (eobp)
    (forward-char 1)))

(defun vim-overwrite ()
  "Overwrite until escape or return."
  (interactive)
  (let ((overwrite-mode 'overwrite-mode-textual)
        (inhibit-quit t)
        event)
    (force-mode-line-update)
    (while (not (memq (setq event (read-event)) '(escape return ?\e ?\r ?\n ?\C-g)))
      (cond
       ((memq event '(?\d backspace))
        (unless (bolp)
          (backward-char 1)))
       ((characterp event)
        (let ((last-command-event event))
          (self-insert-command 1)))))
    (force-mode-line-update)))

(keymap-set vim-normal-mode-map "1" #'digit-argument)
(keymap-set vim-normal-mode-map "2" #'digit-argument)
(keymap-set vim-normal-mode-map "3" #'digit-argument)
(keymap-set vim-normal-mode-map "4" #'digit-argument)
(keymap-set vim-normal-mode-map "5" #'digit-argument)
(keymap-set vim-normal-mode-map "6" #'digit-argument)
(keymap-set vim-normal-mode-map "7" #'digit-argument)
(keymap-set vim-normal-mode-map "8" #'digit-argument)
(keymap-set vim-normal-mode-map "9" #'digit-argument)
(keymap-set vim-normal-mode-map "-" #'negative-argument)
(keymap-set vim-normal-mode-map "u" #'undo)
(keymap-set vim-normal-mode-map "U" #'undo-redo)
(keymap-set vim-normal-mode-map "\\" #'vim-execute-in-emacs)
(keymap-set vim-normal-mode-map ":" #'execute-extended-command)
(keymap-set vim-normal-mode-map "v" #'set-mark-command)
(keymap-set vim-normal-mode-map "V" #'vim-select-line)
(keymap-set vim-normal-mode-map "q" #'quit-window)
(keymap-set vim-normal-mode-map "m" #'point-to-register)
(keymap-set vim-normal-mode-map "'" #'jump-to-register)
(keymap-set vim-normal-mode-map "R" #'vim-overwrite)
(keymap-set vim-normal-mode-map "g f" #'find-file-at-point)
(keymap-set vim-normal-mode-map "g ;" #'vim-goto-last-change)
(keymap-set vim-normal-mode-map "g o" #'pop-global-mark)
(keymap-set vim-normal-mode-map "g d" #'xref-find-definitions)
(keymap-set vim-normal-mode-map "g r" #'revert-buffer-quick)
(keymap-set vim-normal-mode-map "g R" #'revert-buffer)
(keymap-set vim-normal-mode-map "z z" #'recenter-top-bottom)
(keymap-set vim-visual-mode-map "o" #'exchange-point-and-mark)

(provide 'vim)
;;; vim.el ends here
