(defun tb-read-rps ()
  (with-temp-buffer
    (insert-file-contents-literally "day2-input.txt")
    (mapcar #'split-string (split-string (buffer-string) "\n" t))))

(defun tb-win-score (item)
  (cond
   ((if (equal item '("A" "X")) 3)) ; Draw
   ((if (equal item '("A" "Y")) 6)) ; Win
   ((if (equal item '("A" "Z")) 0)) ; Lose
   ((if (equal item '("B" "X")) 0)) ; Lose
   ((if (equal item '("B" "Y")) 3)) ; Draw
   ((if (equal item '("B" "Z")) 6)) ; Win
   ((if (equal item '("C" "X")) 6)) ; Win
   ((if (equal item '("C" "Y")) 0)) ; Lose
   ((if (equal item '("C" "Z")) 3)) ; Draw
   ))

(defun tb-score (item)
  (let* ((a (car item))
         (b (nth 1 item))
         (score (cond
                ((if (equal b "A") 1)) ; Rock
                ((if (equal b "X") 1)) ; Rock
                ((if (equal b "B") 2)) ; Paper
                ((if (equal b "Y") 2)) ; Paper
                ((if (equal b "C") 3)) ; Scissors
                ((if (equal b "Z") 3)) ; Scissors
                )))
    (+ score (tb-win-score item))))

(defun tb-make-move (item)
  (cond
   ((if (equal item '("A" "X")) '("A" "Z"))) ; Lose
   ((if (equal item '("A" "Y")) '("A" "X"))) ; Draw
   ((if (equal item '("A" "Z")) '("A" "Y"))) ; Win
   ((if (equal item '("B" "X")) '("B" "X"))) ; Lose
   ((if (equal item '("B" "Y")) '("B" "Y"))) ; Draw
   ((if (equal item '("B" "Z")) '("B" "Z"))) ; Win
   ((if (equal item '("C" "X")) '("C" "Y"))) ; Lose
   ((if (equal item '("C" "Y")) '("C" "Z"))) ; Draw
   ((if (equal item '("C" "Z")) '("C" "X"))) ; Win
   ))

(defun tb-day2-1 ()
  (let* ((game (tb-read-rps))
         (scores (mapcar #'tb-score game)))
    (seq-reduce #'+ scores 0)))

(defun tb-day2-2 ()
  (let* ((game (tb-read-rps))
         (moves (mapcar #'tb-make-move game))
         (scores (mapcar #'tb-score moves)))
    (seq-reduce #'+ scores 0)))

(cons (tb-day2-1) (tb-day2-2))
