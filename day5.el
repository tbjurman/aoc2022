;; Initial stacks
;;
;;                 [M]     [V]     [L]
;; [G]             [V] [C] [G]     [D]
;; [J]             [Q] [W] [Z] [C] [J]
;; [W]         [W] [G] [V] [D] [G] [C]
;; [R]     [G] [N] [B] [D] [C] [M] [W]
;; [F] [M] [H] [C] [S] [T] [N] [N] [N]
;; [T] [W] [N] [R] [F] [R] [B] [J] [P]
;; [Z] [G] [J] [J] [W] [S] [H] [S] [G]
;;  1   2   3   4   5   6   7   8   9

(defun tb-mk-stacks-arr ()
  (copy-sequence
   [nil
    ("G" "J" "W" "E" "F" "T" "Z")
    ("M" "W" "G")
    ("G" "H" "N" "J")
    ("W" "N" "C" "R" "J")
    ("M" "V" "Q" "G" "B" "S" "F" "W")
    ("C" "W" "V" "D" "T" "R" "S")
    ("V" "G" "Z" "D" "C" "N" "B" "H")
    ("C" "G" "M" "N" "J" "S")
    ("L" "D" "J" "C" "W" "N" "P" "G")
    ]))

(defun tb-read-contents ()
  (with-temp-buffer
    (insert-file-contents-literally "day5-input.txt")
    (seq-filter
     #'(lambda (x) x)
     (mapcar #'tb-split (split-string (buffer-string) "\n" t)))))

;; split function using regexp
(defun tb-split (line)
  (let ((re "^move \\([0-9]+\\) from \\([0-9]+\\) to \\([0-9]+\\)"))
    (when (string-match re line)
      (list
       (string-to-number (match-string 1 line))
       (string-to-number (match-string 2 line))
       (string-to-number (match-string 3 line))))))

(defun tb-do-move (stacks move)
  (let ((cnt (nth 0 move))
        (from (nth 1 move))
        (to (nth 2 move)))
    (while (> cnt 0)
      (let ((val (pop (aref stacks from))))
        (push val (aref stacks to)))
      (setq cnt (- cnt 1)))))

(defun tb-do-move-2 (stacks move)
  (let ((cnt (nth 0 move))
        (from (nth 1 move))
        (to (nth 2 move))
        (tmp nil))
    (while (> cnt 0)
      (push (pop (aref stacks from)) tmp)
      (setq cnt (- cnt 1)))
    (while-let ((item (car tmp)))
      (push item (aref stacks to))
      (setq tmp (cdr tmp)))))

(defun tb-mk-msg (stacks)
  (let ((idx 1)
        (res ""))
    (while (< idx 10)
      (setq res (concat res (pop (aref stacks idx))))
      (setq idx (+ idx 1)))
    res))

(defun tb-do-day5 (func)
  (let ((moves (tb-read-contents))
        (stacks (tb-mk-stacks-arr)))
    (while-let ((move (car moves)))
      (funcall func stacks move)
      (setq moves (cdr moves)))
    (tb-mk-msg stacks)))

(defun tb-day5-1 ()
  (tb-do-day5 #'tb-do-move))

(defun tb-day5-2 ()
  (tb-do-day5 #'tb-do-move-2))

(cons (tb-day5-1) (tb-day5-2))
