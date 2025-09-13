(defun c:DELVERTRANGE2 ( / ent obj coords pts pt1 pt2 ptmid idx1 idx2 idxmid newpts i dist tmp nearest-index flat )
  (vl-load-com)

  (defun nearest-index (pt lst / i idx mindist d)
    (setq i 0 idx -1 mindist 1e9)
    (foreach v lst
      (setq d (distance pt v))
      (if (< d mindist)
        (setq mindist d idx i))
      (setq i (1+ i)))
    idx
  )

  (princ "\nSelect a LWPOLYLINE...")
  (setq ent (car (entsel)))
  (if (not ent)
    (progn (princ "\nNo selection.") (exit))
  )

  (setq obj (vlax-ename->vla-object ent))

  ;; Get all points as (x y) list
  (setq coords (vlax-get obj 'Coordinates))
  (setq pts '())
  (while coords
    (setq pts (append pts (list (list (car coords) (cadr coords)))))
    (setq coords (cddr coords))
  )

  ;; Get 3 pick points
  (setq pt1 (getpoint "\nPick FIRST vertex (start of deletion): "))
  (setq pt2 (getpoint "\nPick SECOND vertex (end of deletion): "))
  (setq ptmid (getpoint "\nPick MIDDLE vertex (to confirm side): "))

  ;; Find indices
  (setq idx1 (nearest-index pt1 pts))
  (setq idx2 (nearest-index pt2 pts))
  (setq idxmid (nearest-index ptmid pts))

  ;; Ensure correct order
  (if (> idx1 idx2)
    (setq tmp idx1 idx1 idx2 idx2 tmp))

  ;; Check if mid is in range
  (if (or (<= idxmid idx1) (>= idxmid idx2))
    (progn (princ "\nMiddle point is not inside range. Nothing deleted.") (exit))
  )

  ;; Rebuild vertex list without range
  (setq i 0 newpts '())
  (repeat (length pts)
    (if (or (<= i idx1) (>= i idx2))
      (setq newpts (append newpts (list (nth i pts)))))
    (setq i (1+ i))
  )

  ;; Flatten list: ((x y)(x y)) → (x y x y ...)
  (setq flat '())
  (foreach p newpts
    (setq flat (append flat p)))

  ;; Set new coordinates
  (vla-put-Coordinates obj
    (vlax-make-variant
      (vlax-safearray-fill
        (vlax-make-safearray vlax-vbDouble (cons 0 (1- (length flat))))
        flat)))

  (princ (strcat "\nDeleted vertices from index " (itoa (1+ idx1)) " to " (itoa (1- idx2)) "."))
  (princ)
)
