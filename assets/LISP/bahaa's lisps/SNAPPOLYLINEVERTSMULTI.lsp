(defun c:SNAPPOLYLINEVERTSMULTI ( / tol pA pB paList pbList get-vertices set-vertex find-nearest snap-count)

  ;; Prompt for tolerance
  (setq tol (getreal "\nEnter snap tolerance (e.g., 0.2): "))
  (if (not tol) (setq tol 0.2))

  ;; Function to get vertices of a polyline
  (defun get-vertices (ent)
    (vlax-get ent 'Coordinates)
  )

  ;; Function to set vertex at index i to new x y
  (defun set-vertex (ent i x y)
    (vlax-put ent 'Coordinates
      (apply 'append
        (mapcar '(lambda (j)
                   (if (= j i)
                     (list x y)
                     (list (nth (* 2 j) (vlax-get ent 'Coordinates))
                           (nth (+ 1 (* 2 j)) (vlax-get ent 'Coordinates)))
                   ))
                (number-sequence 0 (- (/ (length (vlax-get ent 'Coordinates)) 2) 1))
        )
      )
    )
  )

  ;; Find closest point in a list within tolerance
  (defun find-nearest (pt lst tol / minDist nearest)
    (setq minDist nil
          nearest nil)
    (foreach other lst
      (setq dist (distance pt other))
      (if (and (< dist tol) (or (not minDist) (< dist minDist)))
        (progn (setq minDist dist
                     nearest other))
      )
    )
    nearest
  )

  ;; Select primary polylines
  (princ "\nSelect primary polylines (Polyline A): ")
  (setq pA (ssget '((0 . "LWPOLYLINE"))))

  ;; Select secondary polylines
  (princ "\nSelect secondary polylines (Polyline B): ")
  (setq pB (ssget '((0 . "LWPOLYLINE"))))

  ;; Safety check
  (if (or (not pA) (not pB))
    (progn (princ "\nSelection missing.") (exit))
  )

  ;; Collect all vertices from all primary polylines
  (setq paList '())
  (repeat (setq i (sslength pA))
    (setq ent (vlax-ename->vla-object (ssname pA (setq i (1- i)))))
    (setq coords (get-vertices ent))
    (setq j 0)
    (while (< j (length coords))
      (setq pt (list (nth j coords) (nth (+ 1 j) coords)))
      (setq paList (cons pt paList))
      (setq j (+ j 2))
    )
  )

  ;; Snap vertices in secondary polylines
  (setq snap-count 0)
  (repeat (setq i (sslength pB))
    (setq ent (vlax-ename->vla-object (ssname pB (setq i (1- i)))))
    (setq coords (get-vertices ent))
    (setq newcoords '())
    (setq j 0)
    (while (< j (length coords))
      (setq pt (list (nth j coords) (nth (+ 1 j) coords)))
      (setq nearest (find-nearest pt paList tol))
      (if nearest
        (progn
          (setq newcoords (append newcoords nearest))
          (setq snap-count (+ snap-count 1))
        )
        (setq newcoords (append newcoords pt))
      )
      (setq j (+ j 2))
    )
    (vlax-put ent 'Coordinates newcoords)
  )

  (princ (strcat "\nDone. Snapped " (itoa snap-count) " vertex/vertices."))
  (princ)
)
