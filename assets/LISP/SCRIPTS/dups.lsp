(defun vs ( / e i s )
   (if (setq s (ssget "_x" '((0 . "LWPOLYLINE"))))
       (repeat (setq i (sslength s))
           (foreach x
               (LM:ListDupesFuzz
                   (vl-remove-if-not '(lambda ( x ) (= 10 (car x)))
                       (setq e (entget (ssname s (setq i (1- i)))))
                   )
                   1e-8
               )
               (command "_.zoom" "_Object"
                   (entmakex
                       (list
                          '(0 . "CIRCLE")
                          '(8 . "Duplicate-Vertices") ;; Layer
                           x
                          '(40 . 1.0) ;; Radius
                          '(62 . 1)   ;; Colour
                           (assoc 210 e)
                       )
                   )
                   ""
               )
               (princ "\nPress any key to view next duplicate...")
               (grread)
           )
       )
   )
   (princ)
)

;; List Duplicates with Fuzz  -  Lee Mac
;; Returns a list of items appearing more than once in a supplied list

(defun LM:ListDupesFuzz ( l f / c r x )
   (while l
       (setq x (car l)
             c (length l)
             l (vl-remove-if '(lambda ( y ) (equal x y f)) (cdr l))
       )
       (if (< (length l) (1- c))
           (setq r (cons x r))
       )
   )
   (reverse r)
)
(princ)