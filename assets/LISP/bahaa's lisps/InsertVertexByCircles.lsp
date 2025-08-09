(defun c:InsertVertexByCircles (/ circleEnt circleObj baseLayer circleList allCircles i cEnt cObj center radius
                                   ss ent ed coords pt hasVertex inside pt1 pt2 insertIndex j midpoint newCoords)
  (vl-load-com)

  ;; Step 1: Select reference circle
  (setq circleEnt (car (entsel "\nSelect one reference circle: ")))
  (if (null circleEnt)
    (progn (princ "\nNothing selected.") (exit))
  )
  (setq circleObj (vlax-ename->vla-object circleEnt))
  (if (/= (vla-get-objectname circleObj) "AcDbCircle")
    (progn (princ "\nSelected object is not a circle.") (exit))
  )

  ;; Step 2: Get the layer name of the reference circle
  (setq baseLayer (vla-get-layer circleObj))

  ;; Step 3: Get all circles on the same layer
  (setq allCircles (ssget "X" (list '(0 . "CIRCLE") (cons 8 baseLayer))))
  (setq circleList '())

  (if allCircles
    (progn
      (setq i 0)
      (while (< i (sslength allCircles))
        (setq cEnt (ssname allCircles i))
        (setq cObj (vlax-ename->vla-object cEnt))
        (setq center (vlax-get cObj 'Center))
        (setq radius (vlax-get cObj 'Radius))
        (setq circleList (append circleList (list (list center radius))))
        (setq i (1+ i))
      )
    )
    (progn (princ "\nNo other circles found on same layer.") (exit))
  )

  (princ (strcat "\nFound " (itoa (length circleList)) " circle(s) on layer \"" baseLayer "\"."))

  ;; Step 4: Select polylines
  (setq ss (ssget '((0 . "LWPOLYLINE"))))
  (if (null ss)
    (progn (princ "\nNo polylines selected.") (exit))
  )

  ;; Step 5: For each polyline
  (setq i 0)
  (while (< i (sslength ss))
    (setq ent (ssname ss i))
    (setq ed (entget ent))
    (setq coords '())

    ;; Extract polyline vertices
    (foreach d ed
      (if (= (car d) 10)
        (setq coords (append coords (list (cdr d))))
      )
    )

    ;; For each circle in the list
    (foreach circle circleList
      (setq center (car circle))
      (setq radius (cadr circle))

      ;; Check if vertex at center already exists
      (setq hasVertex nil)
      (foreach pt coords
        (if (equal pt center 1e-6)
          (setq hasVertex T)
        )
      )

      ;; If not, check if polyline passes inside circle
      (setq inside nil)
      (setq insertIndex -1)
      (setq j 0)
      (while (and (< j (1- (length coords))) (not inside))
        (setq pt1 (nth j coords))
        (setq pt2 (nth (1+ j) coords))
        ;; midpoint of segment
        (setq midpoint (mapcar '(lambda (a b) (/ (+ a b) 2.0)) pt1 pt2))
        (if (< (distance midpoint center) radius)
          (progn
            (setq inside T)
            (setq insertIndex j)
          )
        )
        (setq j (1+ j))
      )

      ;; Insert vertex at center if needed
      (if (and inside (not hasVertex))
        (progn
          (setq newCoords '())
          (setq j 0)
          (repeat (length coords)
            (setq pt (nth j coords))
            (setq newCoords (append newCoords (list pt)))
            (if (= j insertIndex)
              (setq newCoords (append newCoords (list center)))
            )
            (setq j (1+ j))
          )

          ;; Modify polyline with new vertices
          (setq ed (entget ent))
          (setq ed (subst (cons 90 (length newCoords)) (assoc 90 ed) ed))
          (setq ed (vl-remove-if (function (lambda (x) (= (car x) 10))) ed))
          (foreach pt newCoords
            (setq ed (append ed (list (cons 10 pt))))
          )
          (entmod ed)
          (entupd ent)
        )
      )
    )

    (setq i (1+ i))
  )

  (princ "\nDone adding vertices based on circles.")
  (princ)
)
