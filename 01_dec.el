;;; package --- Summary

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defconst test-string
  "1
2
3

1
3

1
1
1
3
1

1
1
1
3
1")

(defun normalise-input(input-string)
  "INPUT-STRING is normalised into a list of lists."
  (mapcar
   #'(lambda (str)
       (mapcar 'string-to-number (split-string str "\n")))
   (split-string input-string "\n\n")))

(defun total-calories-per-elf(normalised-input)
  "NORMALISED-INPUT: ((1 2) (2 2)) => (3 4)."
  (mapcar
   #'(lambda (x)
       (apply '+ x))
   normalised-input))

(defun find-max-total-calories(calories-per-elf)
  "CALORIES-PER-ELF is a list of numbers. Return the highest number."
  (cl-reduce #'max calories-per-elf))

(defun max-calories-from-input(total-input)
  "TOTAL-INPUT: the input string of the exercise."
  (let ((normalised-input (normalise-input total-input)))
    (find-max-total-calories
     (total-calories-per-elf normalised-input))))

(defun total-calories-ordered(normalised-input)
  "Take NORMALISED-INPUT, add up the enclosed lists and order greatest first."
  (sort (total-calories-per-elf normalised-input) '>))

(defun top-three-calories-added-up(total-input)
  "Take TOTAL-INPUT, calculate total calories, order them, then add up the top 3."
  (let ((normalised-input (normalise-input total-input)))
    (apply '+
           (cl-subseq (total-calories-ordered normalised-input) 0 3))))

;; The answer for star #1
(max-calories-from-input test-string)
;; The answer for star #2
(top-three-calories-added-up test-string)

(provide '01_dec)
;;; 01_dec.el ends here
