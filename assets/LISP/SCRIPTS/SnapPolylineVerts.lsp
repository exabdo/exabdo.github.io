(defun c:SnapPolylineVerts (/ tol entA entB objA objB vertsA coordsB newCoordsB i ptB minDist closestPt changed totalPts)

  (vl-load-com)

  ;; Ask for snap tolerance
  (initget 6)
  (setq tol (getreal "\nEnter snap tolerance (e.g., 0.2): "))
  (if (not tol) (setq tol 0.2)) ; default if none entered

  ;; Select polylines
  (prompt "\nSelect the reference polyline (Polyline A): ")
  (setq entA (car (entsel)))
  (if (not entA)
    (progn (princ "\nNo reference polyline selected.") (exit))
  )
  (setq objA (vlax-ename->vla-object entA))

  (prompt "\nSelect the polyline to adjust (Polyline B): ")
  (setq entB (car (entsel)))
  (if (not entB)
    (progn (princ "\nNo polyline to adjust selected.") (exit))
  )
  (setq objB (vlax-ename->vla-object entB))

  ;; Get vertices from Polyline A
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

  ;; Get coordinates array from Polyline B
  (setq coordsB (vlax-get objB 'Coordinates))
  (setq newCoordsB '())
  (setq changed 0)

  ;; coordsB is a flat list: (x1 y1 x2 y2 ...), so vertices are pairs of elements
  (setq i 0)
  (while (< i (length coordsB))
    (setq ptB (list (nth i coordsB) (nth (1+ i) coordsB)))

    ;; Find closest vertex in A
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

    ;; If close enough, snap vertex
    (if (and closestPt (< minDist tol))
      (progn
        (setq newCoordsB (append newCoordsB (list (car closestPt) (cadr closestPt))))
        (setq changed (1+ changed))
      )
      ;; else keep original
      (setq newCoordsB (append newCoordsB (list (car ptB) (cadr ptB))))
    )

    (setq i (+ i 2))
  )

  ;; Update polyline B coordinates only if any vertex changed
  (if (> changed 0)
    (progn
      (vlax-put objB 'Coordinates newCoordsB)
      (princ (strcat "\nDone. Snapped " (itoa changed) " vertex/vertices."))
    )
    (princ "\nNo vertices were within tolerance to snap.")
  )

  (princ)
)
