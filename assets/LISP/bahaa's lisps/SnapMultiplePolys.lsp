(defun c:SnapMultiplePolys (/ tol entA objA ssB i n objB vertsA coordsB newCoordsB ptB minDist closestPt changed changedTotal totalPts)

  (vl-load-com)

  ;; Ask for snap tolerance
  (initget 6)
  (setq tol (getreal "\nEnter snap tolerance (e.g., 0.2): "))
  (if (not tol) (setq tol 0.2)) ; default

  ;; Select reference polyline A
  (prompt "\nSelect the reference polyline (Polyline A): ")
  (setq entA (car (entsel)))
  (if (not entA)
    (progn (princ "\nNo reference polyline selected.") (exit))
  )
  (setq objA (vlax-ename->vla-object entA))

  ;; Get vertices of polyline A
  (defun GetVertices (pline)
    (setq pts '())
    (setq i 0)
    (setq totalPts (fix (vlax-curve-getEndParam pline)))
    (while (<= i totalPts)
      (setq pts (cons (vlax-curve-getPointAtParam pline i) pts))
      (setq i (1+ i))
    )
    (reverse pts)
  )
  (setq vertsA (GetVertices objA))

  ;; Select multiple polylines to snap
  (prompt "\nSelect one or more polylines to snap (Polyline B, C, D ...): ")
  (setq ssB (ssget '((0 . "LWPOLYLINE"))))
  (if (not ssB)
    (progn (princ "\nNo target polylines selected.") (exit))
  )

  (setq changedTotal 0)

  ;; Loop through each selected target polyline
  (setq n 0)
  (while (< n (sslength ssB))
    (setq entB (ssname ssB n))
    (setq objB (vlax-ename->vla-object entB))

    ;; Get target polyline B's coordinates
    (setq coordsB (vlax-get objB 'Coordinates))
    (setq newCoordsB '())
    (setq changed 0)

    ;; coordsB is flat list of coords
    (setq i 0)
    (while (< i (length coordsB))
      (setq ptB (list (nth i coordsB) (nth (1+ i) coordsB)))

      ;; Find closest vertex on reference polyline A
      (setq minDist 1e10)
      (setq closestPt nil)
      (foreach ptA vertsA
        (setq dist (distance ptB ptA))
        (if (< dist minDist)
          (progn
            (setq minDist dist)
            (setq closestPt ptA)
          )
        )
      )

      ;; Snap if within tolerance
      (if (and closestPt (< minDist tol))
        (progn
          (setq newCoordsB (append newCoordsB (list (car closestPt) (cadr closestPt))))
          (setq changed (1+ changed))
        )
        (setq newCoordsB (append newCoordsB (list (car ptB) (cadr ptB))))
      )

      (setq i (+ i 2))
    )

    ;; Update the target polyline if changed
    (if (> changed 0)
      (vlax-put objB 'Coordinates newCoordsB)
    )
    (setq changedTotal (+ changedTotal changed))

    (setq n (1+ n))
  )

  (if (> changedTotal 0)
    (princ (strcat "\nDone. Snapped total " (itoa changedTotal) " vertices in all selected polylines."))
    (princ "\nNo vertices were within tolerance to snap.")
  )

  (princ)
)
