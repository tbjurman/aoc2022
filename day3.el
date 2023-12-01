;; constants
(setq tb-a (string-to-char "a"))
(setq tb-z (string-to-char "z"))
(setq tb-A (string-to-char "A"))
(setq tb-Z (string-to-char "Z"))

(defun tb-read-contents (&rest split)
  (with-temp-buffer
    (insert-file-contents-literally "day3-input.txt")
    (if split
        (mapcar #'tb-split (split-string (buffer-string) "\n" t))
      (mapcar #'string-to-list (split-string (buffer-string) "\n" t)))))

(defun tb-split (str)
  (let* ((first (substring str 0 (/ (length str) 2)))
         (second (nth 1 (string-split str first))))
    (list (string-to-list first) (string-to-list second))))

(defun tb-prio (item)
  (cond
   ((if (and (>= item tb-a) (<= item tb-z))
        (- item (- tb-a 1))))
   ((if (and (>= item tb-A) (<= item tb-Z))
        (- item (- tb-A 27))))))

(defun tb-mk-prio-list (list1 list2)
  (let ((res '())
        (visited '()))
    (while-let ((item (car list1)))
      (if (and (member item list2) (not (member item visited)))
          (progn
            (setq res (append res (list (tb-prio item))))
            (setq visited (append (list item) visited))))
      (setq list1 (cdr list1)))
    res))

(defun tb-prio-sum (list1)
    (seq-reduce #'+ list1 0))

(defun tb-day3-1 ()
  (let ((contents (tb-read-contents t))
        (sum 0))
    (while-let ((lists (car contents)))
      (let* ((l1 (nth 0 lists))
             (l2 (nth 1 lists))
             (thissum (tb-prio-sum (tb-mk-prio-list l1 l2))))
        (setq sum (+ sum thissum))
        (setq contents (cdr contents))))
    sum))

(defun tb-day3-2 ()
  (let ((contents (tb-read-contents))
        (sum 0))
    (while-let ((l1 (nth 0 contents))
                (l2 (nth 1 contents))
                (l3 (nth 2 contents)))
      (while-let ((item (car l1)))
        (if (and (member item l2) (member item l3))
            (progn
              (setq sum (+ sum (tb-prio item)))
              (setq l1 '()))
          (setq l1 (cdr l1))))
      (setq contents (cdddr contents))
      (setq visited '()))
    sum))


(cons (tb-day3-1) (tb-day3-2))
