(defun c:AutoDPL (/ ss i ent entlist pts allpts minX maxX minY maxY dimGap userGap input)
  (vl-load-com)
  
  ;; Ask user for offset
  (initget 6) ; restrict to positive numbers only
  (setq userGap (getdist "\nEnter offset distance for dimensions <default = 3×DIMSCALE×DIMTXT>: "))

  ;; Use user input or default
  (setq dimGap 
        (if userGap
          userGap
          (* 3.0 (getvar "DIMSCALE") (getvar "DIMTXT"))
        )
  )

  ;; Select all LWPOLYLINEs
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if ss
    (progn
      (setq i 0 allpts '())
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              entlist (entget ent)
        )
        (foreach pair entlist
          (if (= (car pair) 10)
            (setq allpts (cons (cdr pair) allpts))
          )
        )
        (setq i (1+ i))
      )

      ;; Find bounding box
      (setq minX (apply 'min (mapcar 'car allpts)))
      (setq maxX (apply 'max (mapcar 'car allpts)))
      (setq minY (apply 'min (mapcar 'cadr allpts)))
      (setq maxY (apply 'max (mapcar 'cadr allpts)))

      ;; Start UNDO group
      (command "UNDO" "BEGIN")

      ;; Draw dimensions: bottom, right, top, left
      (command "DIMLINEAR" (list minX minY) (list maxX minY) (list (/ (+ minX maxX) 2) (- minY dimGap)) "")
      (command "DIMLINEAR" (list maxX minY) (list maxX maxY) (list (+ maxX dimGap) (/ (+ minY maxY) 2)) "")
      (command "DIMLINEAR" (list maxX maxY) (list minX maxY) (list (/ (+ minX maxX) 2) (+ maxY dimGap)) "")
      (command "DIMLINEAR" (list minX maxY) (list minX minY) (list (- minX dimGap) (/ (+ minY maxY) 2)) "")

      (command "UNDO" "END")
    )
    (prompt "\nNothing selected.")
  )
  (princ)
)
