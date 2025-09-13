(defun c:COPYINSIDE (/ *error* old_echo boundary_ent boundary_pts mode base_pt disp_pt ss_inside ss_cross ss_obj n ent_list ent)
  ;; Error handler
  (defun *error* (msg)
    (setvar "CMDECHO" old_echo)
    (princ (strcat "\n[ERROR] " msg "\n"))
    (princ)
  )

  (setq old_echo (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)
  (command "_.UNDO" "BEGIN")
  (princ "\n--- COPYINSIDE (All Types, No Filter) ---")

  ;; Ask for boundary
  (initget "Draw Select")
  (setq mode (getkword "\nDraw a new polyline or select existing one? [Draw/Select] <Draw>: "))
  (if (or (not mode) (= mode "Draw"))
    (progn
      (princ "\nDraw your closed polyline boundary (use Polyline, close it).")
      (command "_.PLINE")
      (while (= (logand (getvar "CMDACTIVE") 1) 1) (command pause))
      (setq boundary_ent (entlast))
    )
    (setq boundary_ent (car (entsel "\nSelect a closed polyline boundary: ")))
  )

  ;; Validate boundary
  (if (not (and boundary_ent
                (= (cdr (assoc 0 (entget boundary_ent))) "LWPOLYLINE")
                (>= (cdr (assoc 90 (entget boundary_ent))) 3)
                (= (logand (cdr (assoc 70 (entget boundary_ent))) 1) 1)))
    (progn
      (princ "\nSelected object is not a closed polyline with at least 3 vertices. Command canceled.")
      (setvar "CMDECHO" old_echo)
      (command "_.UNDO" "END")
      (princ)
      (exit)
    )
  )

  ;; Get boundary points
  (setq boundary_pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget boundary_ent))))
  ;; Get base/displacement pts
  (setq base_pt (getpoint "\nSpecify base point for copy: "))
  (if (not base_pt)
    (progn
      (princ "\nNo base point specified. Command canceled.")
      (setvar "CMDECHO" old_echo) (command "_.UNDO" "END") (princ) (exit)
    )
  )
  (setq disp_pt (getpoint base_pt "\nSpecify second point (displacement): "))
  (if (not disp_pt)
    (progn
      (princ "\nNo displacement specified. Command canceled.")
      (setvar "CMDECHO" old_echo) (command "_.UNDO" "END") (princ) (exit)
    )
  )

  ;; Select all inside objects (fully within)
  (setq ss_inside (ssget "_WP" boundary_pts))
  ;; Select all crossing objects (crossing, not fully within)
  (setq ss_cross (ssget "_CP" boundary_pts))

  (if (and (not ss_inside) (not ss_cross))
    (progn
      (princ "\nNo objects found inside or crossing the boundary.")
      (setvar "CMDECHO" old_echo) (command "_.UNDO" "END") (princ) (exit)
    )
  )

  (setq n 0)
  ;; Copy fully inside objects
  (if ss_inside
    (progn
      (repeat (sslength ss_inside)
        (setq ent (ssname ss_inside 0))
        (if (tblsearch "layer" (cdr (assoc 8 (entget ent))))
          (progn
            (command "_.COPY" ent "" "_non" base_pt "_non" disp_pt)
            (setq n (1+ n))
          )
        )
        (ssdel ent ss_inside)
      )
    )
  )
  ;; Copy crossing objects (as is, user can trim manually after if needed)
  ;; Only copy if not already inside set (avoid double copy)
  (if ss_cross
    (progn
      (repeat (sslength ss_cross)
        (setq ent (ssname ss_cross 0))
        (if (and ent
                 (not (ssget "_WP" boundary_pts (list ent)))
                 (tblsearch "layer" (cdr (assoc 8 (entget ent)))))
          (progn
            (command "_.COPY" ent "" "_non" base_pt "_non" disp_pt)
            (setq n (1+ n))
          )
        )
        (ssdel ent ss_cross)
      )
    )
  )

  (setvar "CMDECHO" old_echo)
  (command "_.UNDO" "END")
  (princ (strcat "\nDone. " (itoa n) " objects copied inside/crossing the boundary.\n"))
  (princ)
)
(princ "\nLISP routine 'COPYINSIDE-ALLTYPES' loaded. Type COPYINSIDE to run.\n")
(princ)