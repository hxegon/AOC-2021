;; Read file into sequence of lines
(defun read-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defvar day1-input (mapcar #'parse-integer (read-lines "day1-input.txt")))

;; How many times does depths increase?
(defun day1-a (incs depths)
  (if (< (length depths) 2)
      incs
      (day1-a (if (< (first depths) (second depths))
                  (+ 1 incs) incs)
              (cdr depths)))))

(day1-a 0 day1-input)

;; How many times does it increase over a sliding window of 3 depths?
(defun day1-b (incs depths)
  (if (< (length depths) 3)
      incs
    (let ((inced-p (loop for d in depths
                         for i from 1 to 4
                         when (< i 4) sum d into a
                         when (> i 1) sum d into b
                         finally (return (< a b)))))
      (day1-b (if inced-p (+ 1 incs) incs) (cdr depths)))))

(day1-b 0 day1-input)
