;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 출처 : http://ergoemacs.org/emacs/elisp_examples.html
;; 커서 위치에 p 태그 삽입
(defun insert-p-tag ()
  "Insert <p></p> at cursor point."
  (interactive)
  (insert "<p></p>")
  (backward-char 4))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 출처 : http://ergoemacs.org/emacs/elisp_examples.html
;; 커서가 위치한 단어를 선택
;; turn on highlight selection
(transient-mark-mode 1)
(defun select-current-word ()
  "Select the word under cursor.
'word' here is considered any alphanumeric sequence with '_' or '-'."
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (let ((variable value)
;;      (variable value)
;;      …)
;;  body…)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1
(let ((a 1)
      (b 2))
  (* a b))

;;ex1-1
(let (a)
  (setq a 3)
  (* a 2))

;; ex2
(let ((zebra 'stripes)
      (tiger 'fierce))
  (message "One kind of animal has %s and another is %s."
           zebra tiger))

;; uninitialized variables in a let statement
(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
   "Here are %d variables with %s, %s, and %s value."
   birch pine fir oak))

;; 
(if (> 5 4)                             ; if-part
    (message "5 is greater than 4!")
  (message "else"))   ; then-part


(defun type-of-animal (characteristic)
  "Print message in echo area depending on CHARACTERISTIC.
If the CHARACTERISTIC is the symbol `fierce',
then warn of a tiger."
  (if (equal characteristic 'fierce)
      (message "It's a tiger!")
    (message "It's not a tiger!")))

(type-of-animal 'fierce)
(type-of-animal 'stripes)
(type-of-animal 'zebra)


(transient-mark-mode 1)
(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

;; Find/Replace Text Region
(defun replace-greek-region (start end)
  "Replace 'alpha' to 'α' and other greek letters in current region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "alpha" nil t)
      (replace-match "α" nil t))
    (goto-char (point-min))
    (while (search-forward "beta" nil t)
      (replace-match "β" nil t))
    (goto-char (point-min))
    (while (search-forward "gamma" nil t)
      (replace-match "γ" nil t))))

;; dkdkdkd alpha dkdkdkd
;; dkdkdkd beta dkdkdkd
;; dkdkdkd gamma dkdkdkd

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emulation of the vi % command
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; bind C-% to vi's % command  
(global-set-key (kbd "C-%") 'forward-or-backward-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete Enclosed Text
(defun delete-enclosed-text ()
  "Delete texts between any pair of delimiters."
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^([{<")
      (setq p1 (point))
      (skip-chars-forward "^)]}>")
      (setq p2 (point))
      (delete-region p1 p2))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;{dadfadfadf}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delete Linebreaks
(defun remove-line-breaks ()
  "Remove line endings in current paragraph."
  (interactive)
  (let ((fill-column)
        (point-max)))
  (fill-paragraph nil))

