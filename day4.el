(defun tb-read-contents ()
  (with-temp-buffer
    (insert-file-contents-literally "day4-input.txt")
    (mapcar #'tb-split (split-string (buffer-string) "\n" t))))

;; split function using regexp
(defun tb-split (line)
  (let ((re "^\\([0-9]+\\)-\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)"))
    (when (string-match re line)
      (list
       (list (string-to-number (match-string 1 line))
             (string-to-number (match-string 2 line)))
       (list (string-to-number (match-string 3 line))
             (string-to-number (match-string 4 line)))))))

;; split function using split-string
;; (defun tb-split (line)
;;   (let* ((strs (split-string line "," t))
;;          (strs0 (split-string (nth 0 strs) "-" t))
;;          (num00 (string-to-number (nth 0 strs0)))
;;          (num01 (string-to-number (nth 1 strs0)))
;;          (strs1 (split-string (nth 1 strs) "-" t))
;;          (num10 (string-to-number (nth 0 strs1)))
;;          (num11 (string-to-number (nth 1 strs1))))
;;     (list (list num00 num01) (list num10 num11))))


(defun tb-is-fully-contained (pair)
  (or (tb-is-contained pair) (tb-is-contained (reverse pair))))

(defun tb-is-contained (pair)
  (let ((a0 (nth 0 (nth 0 pair)))
        (a1 (nth 1 (nth 0 pair)))
        (b0 (nth 0 (nth 1 pair)))
        (b1 (nth 1 (nth 1 pair))))
    (when (and (>= a0 b0) (<= a1 b1))
      t)))

(defun tb-day4-1 ()
  (let ((contents (tb-read-contents))
        (sum 0))
    (while-let ((pair (car contents)))
      (when (tb-is-fully-contained pair)
        (setq sum (+ sum 1)))
      (setq contents (cdr contents)))
    sum))

(defun tb-overlaps (pair)
  (let* ((sorted-pair (sort pair #'(lambda (x y) (> (nth 0 x)) (nth 0 y))))
         (a0 (nth 0 (nth 0 sorted-pair)))
         (a1 (nth 1 (nth 0 sorted-pair)))
         (b0 (nth 0 (nth 1 sorted-pair)))
         (b1 (nth 1 (nth 1 sorted-pair))))
    (when (and (<= b0 a1) (>= b1 a0)) t)))

(defun tb-day4-2 ()
  (let ((contents (tb-read-contents))
        (sum 0))
    (while-let ((pair (car contents)))
      (when (tb-overlaps pair)
        (setq sum (+ sum 1)))
      (setq contents (cdr contents)))
    sum))

(cons (tb-day4-1) (tb-day4-2))
