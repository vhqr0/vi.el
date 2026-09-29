;;; vim-tests.el --- Tests for vim.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Run with `make test'.  TEXT uses "|" to mark point.

;;; Code:

(require 'ert)
(require 'vim)
(require 'vim-integrated)
(require 'dired)

(defvar python-indent-guess-indent-offset)

(defun vim-test--mark (text pos)
  "Insert \"|\" into TEXT before 1-based POS."
  (concat (substring text 0 (1- pos)) "|" (substring text (1- pos))))

(defun vim-test--setup (text mode)
  (transient-mark-mode 1)
  (unless vim-global-mode
    (vim-global-mode 1))
  (let ((buffer (get-buffer-create "*vim-test*"))
        (python-indent-guess-indent-offset nil))
    (switch-to-buffer buffer)
    (deactivate-mark)
    (erase-buffer)
    (funcall mode)
    (insert (string-replace "|" "" text))
    (goto-char (1+ (string-search "|" text)))
    (vim-change-mode-to-normal)
    (setq vim-pending-op-fn nil
          vim-last nil
          vim-find-last nil
          vim-search-last nil
          vim-surround-from nil
          vim-surround-to nil
          vim-replace-char nil)
    buffer))

(defun vim-test--result ()
  (concat (buffer-substring-no-properties (point-min) (point))
          "|"
          (buffer-substring-no-properties (point) (point-max))))

(defun vim-test--execute (keys)
  (let ((inhibit-message t))
    (execute-kbd-macro (kbd keys))))

(cl-defun vim-test (text keys &key (mode #'text-mode) kill)
  "Execute KEYS on TEXT, return the result text with point marked."
  (setq kill-ring (and kill (list kill))
        kill-ring-yank-pointer kill-ring)
  (with-current-buffer (vim-test--setup text mode)
    (vim-test--execute keys)
    (vim-test--result)))

(cl-defun vim-test-state (text keys &key (mode #'text-mode) kill)
  "Execute KEYS on TEXT, return (STATE REGION KILL)."
  (setq kill-ring (and kill (list kill))
        kill-ring-yank-pointer kill-ring)
  (with-current-buffer (vim-test--setup text mode)
    (vim-test--execute keys)
    (list (cond (vim-visual-mode 'visual)
                (vim-insert-mode 'insert)
                (vim-normal-mode 'normal)
                (t 'emacs))
          (and (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end)))
          (car kill-ring))))

(defmacro vim-test-error (text keys &rest args)
  "Execute KEYS on TEXT, expecting an error.
Return (RESULT REGION-ACTIVE) after the error."
  `(let (result)
     (should-error (vim-test ,text ,keys ,@args))
     (with-current-buffer "*vim-test*"
       (setq result (list (vim-test--result) (region-active-p))))
     result))

;;; move

(ert-deftest vim-test-move-char-line ()
  (should (equal (vim-test "ab|cd" "h") "a|bcd"))
  (should (equal (vim-test "ab|cd" "l") "abc|d"))
  (should (equal (vim-test "a|bcd" "2 l") "abc|d"))
  (should (equal (vim-test "a|b\ncd\nef" "j") "ab\nc|d\nef"))
  (should (equal (vim-test "ab\ncd\ne|f" "2 k") "a|b\ncd\nef")))

(ert-deftest vim-test-move-word ()
  (should (equal (vim-test "|foo bar baz" "w") "foo |bar baz"))
  (should (equal (vim-test "|foo bar baz" "2 w") "foo bar |baz"))
  (should (equal (vim-test "|foo bar baz" "e") "fo|o bar baz"))
  (should (equal (vim-test "fo|o bar baz" "e") "foo ba|r baz"))
  (should (equal (vim-test "foo bar |baz" "b") "foo |bar baz"))
  (should (equal (vim-test "foo ba|r baz" "g e") "fo|o bar baz")))

(ert-deftest vim-test-move-sexp ()
  (let ((text "(foo (bar baz) qux) (a b)"))
    (should (equal (vim-test (vim-test--mark text 2) "W" :mode #'emacs-lisp-mode)
                   (vim-test--mark text 6)))
    (should (equal (vim-test (vim-test--mark text 6) "E" :mode #'emacs-lisp-mode)
                   (vim-test--mark text 14)))
    (should (equal (vim-test (vim-test--mark text 7) "B" :mode #'emacs-lisp-mode)
                   (vim-test--mark text 6)))
    (should (equal (vim-test (vim-test--mark text 16) "g E" :mode #'emacs-lisp-mode)
                   (vim-test--mark text 14)))))

(ert-deftest vim-test-move-line ()
  (should (equal (vim-test "  foo b|ar" "0") "|  foo bar"))
  (should (equal (vim-test "  foo b|ar" "^") "  |foo bar"))
  (should (equal (vim-test "  f|oo bar\nx" "$") "  foo ba|r\nx"))
  (should (equal (vim-test "a\nb\n|c" "g g") "|a\nb\nc"))
  (should (equal (vim-test "|a\nb\nc" "2 G") "a\n|b\nc"))
  (should (equal (vim-test "|a\nb\nc" "G") "a\nb\nc|")))

(ert-deftest vim-test-move-find ()
  (should (equal (vim-test "|a-b-c-d" "f -") "a|-b-c-d"))
  (should (equal (vim-test "|a-b-c-d" "f - ;") "a-b|-c-d"))
  (should (equal (vim-test "|a-b-c-d" "f - ; ,") "a|-b-c-d"))
  (should (equal (vim-test "|a-b-c-d" "t - ;") "a-|b-c-d"))
  (should (equal (vim-test "a-b-c-|d" "F -") "a-b-c|-d"))
  (should (equal (vim-test "a-b-c-|d" "T - ;") "a-b-|c-d")))

(ert-deftest vim-test-move-jump-item ()
  (let ((text "((a (b (c))) (d ((e))))"))
    (dolist (pair '((1 . 23) (2 . 12) (11 . 5) (23 . 1) (17 . 21)))
      (should (equal (vim-test (vim-test--mark text (car pair)) "%" :mode #'emacs-lisp-mode)
                     (vim-test--mark text (cdr pair)))))))

(ert-deftest vim-test-move-search ()
  (should (equal (vim-test "|foo bar foo baz foo" "/ f o o RET") "foo bar |foo baz foo"))
  (should (equal (vim-test "|foo bar foo baz foo" "/ f o o RET n") "foo bar foo baz |foo"))
  (should (equal (vim-test "foo bar foo baz |foo" "? f o o RET") "foo bar |foo baz foo")))

(ert-deftest vim-test-move-negative ()
  (should (equal (vim-test "foo bar |baz" "- w") "foo |bar baz"))
  (should (equal (vim-test "foo |bar baz" "- b") "foo bar |baz"))
  (should (equal (vim-test "a-b-c-|d" "- f -") "a-b-c|-d")))

;;; tobj

(ert-deftest vim-test-tobj ()
  (should (equal (vim-test "foo b|ar baz" "d i w") "foo | baz"))
  (should (equal (vim-test "foo b|ar baz" "d a w") "foo |baz"))
  (should (equal (vim-test "f(a, (b| c), d)" "d i b") "f(a, (|), d)"))
  (should (equal (vim-test "f(a, (b| c), d)" "d a b") "f(a, |, d)"))
  (should (equal (vim-test "f(a, (b| c), d)" "2 d i (") "f(|)"))
  (should (equal (vim-test "x [a| b] y" "d i r") "x [|] y"))
  (should (equal (vim-test "x <a| b> y" "d a a") "x | y"))
  (should (equal (vim-test "  a|b  \nc" "d i l") "  |\nc"))
  (should (equal (vim-test "a\nb|b\nc" "d a l") "a\n|c"))
  (should (equal (vim-test "(f \"a|b\" 'x)" "d i Q" :mode #'emacs-lisp-mode) "(f \"|\" 'x)"))
  (should (equal (vim-test "x = 'a|b'" "d a q" :mode #'python-mode) "x = |")))

(ert-deftest vim-test-tobj-error-keeps-state ()
  (should (equal (vim-test-error "fo|o bar" "d i b") '("fo|o bar" nil)))
  (should (equal (vim-test-error "foo (bar [b|az) qux" "v l i r") '("foo (bar [ba|z) qux" t))))

;;; op

(ert-deftest vim-test-op ()
  (should (equal (vim-test "|foo bar baz" "d w") "|bar baz"))
  (should (equal (vim-test "|foo bar baz" "d e") "| bar baz"))
  (should (equal (vim-test "foo |bar baz" "d b") "|bar baz"))
  (should (equal (vim-test "|foo bar baz" "d 2 w") "|baz"))
  (should (equal (vim-test "|foo bar baz" "2 d w") "|baz"))
  (should (equal (vim-test "a|b\ncd\nef" "d j") "|ef"))
  (should (equal (vim-test "ab\nc|d\nef" "d d") "ab\n|ef"))
  (should (equal (vim-test "|ab\ncd\nef" "2 d d") "|ef"))
  (should (equal (vim-test "|foo bar" "y w") "|foo bar"))
  (should (equal (vim-test-state "|foo bar" "y w") '(normal nil "foo ")))
  (should (equal (vim-test "|hello world" "g U w") "|HELLO world"))
  (should (equal (vim-test "|Hello" "g ~ ~") "|hELLO"))
  (should (equal (vim-test "|a\nb" "g c c" :mode #'emacs-lisp-mode) "|;; a\nb"))
  (should (equal (vim-test "|a\n  b\nc" "g J j") "|a b\nc")))

(ert-deftest vim-test-op-change ()
  (should (equal (vim-test "  a|b cd\nx" "c c") "  |\nx"))
  (should (equal (vim-test "|ab\ncd\nef" "c j") "|\nef"))
  (should (equal (vim-test "|foo bar" "c w") "| bar"))
  (should (equal (car (vim-test-state "|foo bar" "c w")) 'insert)))

(ert-deftest vim-test-op-count ()
  (should (equal (vim-test "f(a, (b| c), d)" "d 2 i b") "f(|)"))
  (should (equal (vim-test "f(a, (b| c), d)" "d 2 i (") "f(|)"))
  (should (equal (vim-test "|foo bar baz qux" "c 2 a w x <escape>") "|x baz qux"))
  (should (equal (vim-test "|a\nb\nc\nd" "d 3 d") "|d"))
  (should (equal (vim-test "|a\nb\nc\nd" "y 2 y") "|a\nb\nc\nd"))
  (should (equal (vim-test "|foo bar baz" "d 2 w") "|baz")))

(ert-deftest vim-test-op-pending-clear ()
  (with-current-buffer (vim-test--setup "|foo bar" #'text-mode)
    (vim-test--execute "d")
    (should vim-pending-op-fn)
    (let ((this-command 'digit-argument))
      (vim-pending-op-post-command))
    (should vim-pending-op-fn)
    (let ((this-command 'abort-recursive-edit))
      (vim-pending-op-post-command))
    (should-not vim-pending-op-fn)
    (vim-test--execute "w")
    (should (equal (vim-test--result) "foo |bar"))))

(ert-deftest vim-test-op-pending ()
  (should (equal (vim-test "|foo bar baz" "d x w") "oo |bar baz"))
  (should (equal (vim-test "|aa bb cc" "d c w") "| bb cc")))

;;; cmd

(ert-deftest vim-test-cmd ()
  (should (equal (vim-test "a|bcd" "x") "a|cd"))
  (should (equal (vim-test "a|bcd" "9 x") "a|"))
  (should (equal (vim-test "ab|cd" "X") "a|cd"))
  (should (equal (vim-test "a|bc\nd" "D") "a|\nd"))
  (should (equal (vim-test "a|bc\nd" "2 D") "a|"))
  (should (equal (vim-test-state "a|bc" "Y") '(normal nil "bc")))
  (should (equal (vim-test "a|bc" "r z") "a|zc"))
  (should (equal (vim-test "a|bcd" "3 r z") "a|zzz"))
  (should (equal (vim-test "|abc" "~") "A|bc"))
  (should (equal (vim-test "|a\n  b\n  c" "J") "a| b\n  c"))
  (should (equal (vim-test "|a\n  b\n  c" "2 J") "a| b c")))

(ert-deftest vim-test-cmd-insert ()
  (should (equal (vim-test "a|bc" "i x <escape>") "a|xbc"))
  (should (equal (vim-test "a|bc" "a x <escape>") "ab|xc"))
  (should (equal (vim-test "  a|bc" "I x <escape>") "  |xabc"))
  (should (equal (vim-test "a|bc" "A x <escape>") "abc|x"))
  (should (equal (vim-test "a|b\nc" "o x <escape>") "ab\n|x\nc"))
  (should (equal (vim-test "a\nb|c" "O x <escape>") "a\n|x\nbc"))
  (should (equal (vim-test "a|bc" "s x <escape>") "a|xc"))
  (should (equal (vim-test "  a|b  \nc" "S x <escape>") "  |x  \nc"))
  (should (equal (vim-test "a|bc" "C x <escape>") "a|x")))

(ert-deftest vim-test-cmd-put ()
  (should (equal (vim-test "a|bc" "p" :kill "XY") "a|bXYc"))
  (should (equal (vim-test "a|bc" "P" :kill "XY") "a|XYbc"))
  (should (equal (vim-test "a|b\nc" "p" :kill "L\n") "a|b\nL\nc"))
  (should (equal (vim-test "a\nb|c" "P" :kill "L\n") "a\nL\nb|c")))

;;; visual

(ert-deftest vim-test-visual-state ()
  (should (equal (vim-test-state "|foo bar" "v e") '(visual "fo" nil)))
  (should (equal (vim-test-state "|foo bar" "v e <escape>") '(normal nil nil)))
  (should (equal (vim-test-state "|foo bar" "v e v") '(normal nil nil)))
  (should (equal (vim-test-state "a\nb|b\nc" "V") '(visual "bb\n" nil)))
  (should (equal (vim-test-state "a\nb|b\nc" "V j") '(visual "bb\nc" nil)))
  (should (equal (vim-test "|foo bar" "v e o") "|foo bar"))
  (should (equal (vim-test-state "|foo bar" "v e o") '(visual "fo" nil))))

(ert-deftest vim-test-visual-op ()
  (should (equal (vim-test "|foo bar" "v e d") "|o bar"))
  (should (equal (vim-test "|foo bar" "v e x") "|o bar"))
  (should (equal (vim-test "|foo bar" "v e D") "|o bar"))
  (should (equal (vim-test-state "|foo bar" "v e y") '(normal nil "fo")))
  (should (equal (vim-test "|foo bar" "v e c x <escape>") "|xo bar"))
  (should (equal (vim-test "|foo bar" "v e U") "FO|o bar"))
  (should (equal (vim-test "|foo bar" "v e r z") "|zzo bar"))
  (should (equal (vim-test "|foo bar" "v e p" :kill "XY") "XY|o bar"))
  (should (equal (vim-test "a\nb|b\nc" "V d") "a\n|c"))
  (should (equal (vim-test "  a\n  b|b  \nc" "V c x <escape>") "  a\n  |x  \nc"))
  (should (equal (vim-test "|a\n  b\nc" "V j J") "a| b\nc")))

(ert-deftest vim-test-visual-tobj ()
  (should (equal (vim-test-state "f (a b|c) d" "v i b") '(visual "a bc" nil)))
  (should (equal (vim-test-state "f (a b|c) d" "v a b") '(visual "(a bc)" nil)))
  (should (equal (vim-test "f (a b|c) d" "v i b d") "f (|) d")))

;;; surround

(ert-deftest vim-test-surround ()
  (should (equal (vim-test "f(a, [b| c])" "d s r") "f(a, b| c)"))
  (should (equal (vim-test "f(a, [b| c])" "d s (") "fa, [b| c]"))
  (should (equal (vim-test "f(a, [b| c])" "c s r B") "f(a, {b| c})"))
  (should (equal (vim-test "  f|oo bar" "y s i w )") "  (f|oo) bar"))
  (should (equal (vim-test "  f|oo bar  " "y s s b") "  (f|oo bar)  "))
  (should (equal (vim-test "  f|oo bar" "y s i w Q") "  \"f|oo\" bar"))
  (should (equal (vim-test "a\n  b|b  \nc" "V s )") "a\n  (bb)  \n|c")))

;;; repeat

(ert-deftest vim-test-repeat ()
  (should (equal (vim-test "|a b c d" "d w .") "|c d"))
  (should (equal (vim-test "|abcd" "x .") "|cd"))
  (should (equal (vim-test "|a-b-c-d-e" "d f - .") "|c-d-e"))
  (should (equal (vim-test "|a b c" "r z w .") "z |z c"))
  (should (equal (vim-test "(((|a)))" "d s b ." :mode #'emacs-lisp-mode) "(|a)"))
  (should (equal (vim-test "|abc def ghi" "x w v e .") "bc |f ghi")))

;;; misc

(ert-deftest vim-test-execute-in-emacs ()
  (should (equal (vim-test "|abc" "\\ w") "w|abc"))
  (should (equal (vim-test "|abc" "3 \\ C-f") "abc|")))

(ert-deftest vim-test-overwrite ()
  (should (equal (vim-test "|abc" "R x y <escape>") "xy|c"))
  (should (equal (vim-test "|ab" "R x y z <escape>") "xyz|")))

;;; more coverage

(ert-deftest vim-test-move-more ()
  (should (equal (vim-test "|foo bar foo baz foo" "/ f o o RET n N") "foo bar |foo baz foo"))
  (should (equal (vim-test "|a\nb\nc" "- g g") "a\nb\n|c"))
  (should (equal (vim-test "a\nb|b\nc" "- 2 $") "|a\nbb\nc"))
  (should (equal (vim-test-error "x (a \"(b\" c) y|" "%" :mode #'emacs-lisp-mode)
                 '("x (a \"(b\" c) y|" nil))))

(ert-deftest vim-test-tobj-more ()
  (should (equal (vim-test "(a b|c-d e)" "d i W" :mode #'emacs-lisp-mode) "(a | e)"))
  (should (equal (vim-test "(a b|c-d e)" "d a W" :mode #'emacs-lisp-mode) "(a |e)"))
  (should (equal (vim-test "see ~/a/b|c.el ok" "d i f") "see | ok"))
  (should (equal (vim-test "see ~/a/b|c.el ok" "d a f") "see |ok"))
  (should (equal (vim-test "p1\np|1\n\np2" "d i p") "|\np2"))
  (should (equal (vim-test "(defun a () 1)\n\n(defun b ()\n  |2)\n" "d i d" :mode #'emacs-lisp-mode)
                 "(defun a () 1)\n\n|"))
  (should (equal (vim-test "a\nb|b\nc" "d i h") "|"))
  (should (equal (vim-test "x {a| b} y" "d i {") "x {|} y"))
  (should (equal (vim-test "x [a| b] y" "d a [") "x | y"))
  (should (equal (vim-test "x <a| b> y" "d i <") "x <|> y"))
  (should (equal (vim-test "x = 'a|b'" "d i '" :mode #'python-mode) "x = '|'"))
  (should (equal (vim-test "x = \"a|b\"" "d a \"" :mode #'python-mode) "x = |")))

(ert-deftest vim-test-op-more ()
  (should (equal (vim-test "(a\n|b)" "= =" :mode #'emacs-lisp-mode) "(a\n| b)"))
  (should (equal (vim-test "|    a" "< <") "|a"))
  (should (equal (vim-test "|a" "> >" :mode #'fundamental-mode) "|\ta"))
  (should (equal (vim-test "|HELLO World" "g u w") "|hello World"))
  (should (equal (vim-test-state "a|b\ncd" "y y") '(normal nil "ab\n")))
  (should (equal (vim-test "|a\nb\nc" "y 2 j") "|a\nb\nc")))

(ert-deftest vim-test-visual-more ()
  (should (equal (vim-test "|FOO bar" "v e u") "fo|O bar"))
  (should (equal (vim-test "|foo bar" "v e C x <escape>") "|xo bar"))
  (should (equal (vim-test-state "|foo bar" "v e Y") '(normal nil "fo")))
  (should (equal (vim-test "|a\n  b\nc" "V j J") "a| b\nc"))
  (should (equal (vim-test "|abc def ghi" "v e d w v e .") "c |f ghi"))
  (should (equal (vim-test-state "|foo bar" "C-SPC C-f") '(visual "f" nil)))
  (should (equal (vim-test-state "|foo bar" "i S-<right>") '(visual "f" nil)))
  (should (equal (vim-test-state "|foo bar" "C-SPC C-f C-w") '(normal nil "f"))))

(ert-deftest vim-test-repeat-count ()
  (should (equal (vim-test "|abcdef" "x 3 .") "|ef")))

(ert-deftest vim-test-insert-more ()
  (should (equal (vim-test "(a\n |b\n c)" "o x <escape>" :mode #'emacs-lisp-mode) "(a\n b\n |x\n c)"))
  (should (equal (vim-test "(a\n |b)" "O x <escape>" :mode #'emacs-lisp-mode) "(a\n |x\n b)")))

(ert-deftest vim-test-overwrite-more ()
  (should (equal (vim-test "|abc" "R x DEL y <escape>") "y|bc"))
  (should (equal (vim-test "|abc" "R x RET y <escape>") "x\ny|c")))

(ert-deftest vim-test-global-mode-off ()
  (unwind-protect
      (with-current-buffer (vim-test--setup "|foo" #'text-mode)
        (vim-global-mode -1)
        (should-not (or vim-normal-mode vim-visual-mode vim-insert-mode))
        (should-not (memq #'vim-visual-post-command (default-value 'post-command-hook)))
        (should-not (memq #'vim-pending-op-post-command (default-value 'post-command-hook))))
    (vim-global-mode 1)))

(ert-deftest vim-test-major-mode-map ()
  (with-current-buffer (vim-test--setup "|foo" #'text-mode)
    (should (eq (keymap-lookup nil "m") #'point-to-register)))
  (let ((buffer (dired-noselect default-directory)))
    (unwind-protect
        (with-current-buffer buffer
          (should vim-normal-mode)
          (should (eq (keymap-lookup nil "m") #'dired-mark))
          (should (eq (keymap-lookup nil "d") #'dired-flag-file-deletion))
          (should (eq (keymap-lookup nil "j") #'dired-next-line))
          (should (eq (keymap-lookup nil "w") #'vim-w))
          (should (eq (keymap-lookup nil "g g") #'vim-gg))
          (vim-change-mode-to-visual)
          (should (eq (keymap-lookup nil "m") #'dired-mark))
          (should (eq (keymap-lookup nil "i w") #'vim-iw))
          (should (eq (keymap-lookup nil "<escape>") #'vim-exit-visual))
          (vim-change-mode-to-normal)
          (fundamental-mode)
          (should (eq (keymap-lookup nil "m") #'point-to-register)))
      (kill-buffer buffer))))

(ert-deftest vim-test-major-mode-map-set ()
  (vim-define-major-mode-map 'vim-test-major-mode)
  (vim-major-mode-map-set 'vim-test-major-mode 'insert "<f11>" #'ignore)
  (vim-major-mode-map-set 'vim-test-major-mode '(normal) "<f12>" #'ignore)
  (should-not (keymap-lookup vim-vim-test-major-mode-normal-override-map "<f11>"))
  (should (eq (keymap-lookup vim-vim-test-major-mode-insert-override-map "<f11>") #'ignore))
  (should (eq (keymap-lookup vim-vim-test-major-mode-normal-override-map "<f12>") #'ignore))
  (should-not (keymap-lookup vim-vim-test-major-mode-visual-override-map "<f12>"))
  (should (eq (keymap-lookup vim-vim-test-major-mode-insert-override-map "<escape>") #'vim-exit-insert))
  (should-error (vim-major-mode-map-set 'vim-test-no-mode 'normal "<f11>" #'ignore))
  (should-error (vim-major-mode-map-set 'vim-test-major-mode 'emacs "<f11>" #'ignore)))

(define-derived-mode vim-test-parent-mode special-mode "Parent")
(define-derived-mode vim-test-child-mode vim-test-parent-mode "Child")
(define-derived-mode vim-test-grandchild-mode vim-test-child-mode "Grandchild")

(ert-deftest vim-test-major-mode-map-derived ()
  (vim-define-major-mode-map 'vim-test-child-mode)
  (should (eq (keymap-parent vim-special-mode-normal-override-map) vim-normal-mode-map))
  (should (eq (keymap-parent vim-vim-test-parent-mode-normal-override-map)
              vim-special-mode-normal-override-map))
  (should (eq (keymap-parent vim-vim-test-child-mode-normal-override-map)
              vim-vim-test-parent-mode-normal-override-map))
  (should (eq (keymap-parent vim-vim-test-child-mode-visual-override-map)
              vim-vim-test-parent-mode-visual-override-map))
  (let ((child-maps (alist-get 'vim-test-child-mode vim-major-mode-map-alist)))
    (vim-define-major-mode-map 'vim-test-child-mode)
    (should (eq (alist-get 'vim-test-child-mode vim-major-mode-map-alist) child-maps)))
  (defvar vim-vim-test-grandchild-mode-insert-override-map (make-sparse-keymap))
  (let ((insert-map vim-vim-test-grandchild-mode-insert-override-map))
    (vim-define-major-mode-map 'vim-test-grandchild-mode)
    (should (eq vim-vim-test-grandchild-mode-insert-override-map insert-map))
    (should (eq (alist-get 'insert (alist-get 'vim-test-grandchild-mode vim-major-mode-map-alist))
                insert-map))
    (should (eq (keymap-parent vim-vim-test-grandchild-mode-normal-override-map)
                vim-vim-test-child-mode-normal-override-map)))
  (vim-major-mode-map-set 'vim-test-parent-mode 'normal "<f11>" #'ignore)
  (vim-major-mode-map-set 'vim-test-child-mode 'normal "<f12>" #'ignore)
  (should (eq (keymap-lookup vim-vim-test-child-mode-normal-override-map "<f11>") #'ignore))
  (should-not (keymap-lookup vim-vim-test-parent-mode-normal-override-map "<f12>"))
  (with-temp-buffer
    (vim-test-grandchild-mode)
    (vim-change-mode-to-default)
    (should (eq (alist-get 'vim-normal-mode minor-mode-overriding-map-alist)
                vim-vim-test-grandchild-mode-normal-override-map))
    (should (eq (keymap-lookup nil "<f11>") #'ignore)))
  (with-temp-buffer
    (text-mode)
    (vim-change-mode-to-default)
    (should-not (alist-get 'vim-normal-mode minor-mode-overriding-map-alist))))

(define-derived-mode vim-test-rebind-mode nil "Rebind")

(ert-deftest vim-test-op-rebind-jk ()
  (vim-define-major-mode-map 'vim-test-rebind-mode)
  (vim-major-mode-map-set 'vim-test-rebind-mode 'normal "j" #'ignore "k" #'ignore)
  (should (equal (vim-test "|a\nb\nc" "j" :mode #'vim-test-rebind-mode) "|a\nb\nc"))
  (should (equal (vim-test "|a\nb\nc" "d j" :mode #'vim-test-rebind-mode) "|c"))
  (should (equal (vim-test-state "a\n|b\nc" "y k" :mode #'vim-test-rebind-mode)
                 '(normal nil "a\nb\n"))))

(defvar vim-test--eval nil)

(ert-deftest vim-test-op-narrow-eval ()
  (should (equal (vim-test "|a\nb\nc" "g - j") "|a\nb\n"))
  (setq vim-test--eval nil)
  (vim-test "|(setq vim-test--eval 1)" "g y y" :mode #'emacs-lisp-mode)
  (should (equal vim-test--eval 1))
  (vim-test "|(setq vim-test--eval 2)" "g y y" :mode #'lisp-interaction-mode)
  (should (equal vim-test--eval 2))
  (should (equal (vim-test-error "|a" "g y y") '("|a" nil))))

(ert-deftest vim-test-visual-jump-item ()
  (should (equal (vim-test-state "|(a b) c" "v m") '(visual "(a b" nil))))

(provide 'vim-tests)
;;; vim-tests.el ends here
