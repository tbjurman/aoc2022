(defun tb-read-bags ()
    (with-temp-buffer
      (progn
        (insert "((")
        (insert-file-contents-literally "day1-input.txt")
        (replace-regexp "^$" ")(")
        (end-of-buffer)
        (insert "))")
        (goto-char 0)
        (read (current-buffer)))))

(defun tb-sum-and-sort-bags (bags)
  (sort (mapcar (lambda (bag)
                  (seq-reduce #'+ bag 0))
                bags)
        #'>))

(defun tb-day1-1 ()
  (let ((bags (tb-read-bags)))
    (car (seq-take (tb-sum-and-sort-bags bags) 1))))

(defun tb-day1-2 ()
  (let* ((bags (tb-read-bags))
         (top-three (seq-take (tb-sum-and-sort-bags bags) 3)))
    (seq-reduce #'+ top-three 0)))

(cons (tb-day1-1) (tb-day1-2))
