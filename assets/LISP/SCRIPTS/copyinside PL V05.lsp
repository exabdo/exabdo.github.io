(defun c:COPYINSIDE (/ *error* ss base_pt disp_pt old_echo pline_ent pt_list keep_poly 
                       ;;; --- New variables for offset functionality ---
                       offset_dist selection_boundary temporary_boundary offset_pline)

  ;; --- Error handler to restore settings ---
  (defun *error* (msg)
    (setvar "CMDECHO" old_echo)
    (if (and temporary_boundary (entget selection_boundary)) ; Ensure temp boundary is deleted on error
        (entdel selection_boundary)
    )
    (princ)
  )

  ;; --- Setup ---
  (setq old_echo (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;;; --- Updated version information ---
  (princ "\n--- Copy Inside Polygon (v4.0 - Offset & Keep Polygon) ---")
  (princ "\nDraw your polygon using polyline. Use A for arcs, L for lines.")

  ;; --- Let user draw a polyline ---
  (command "_.PLINE")
  (while (= (logand (getvar "CMDACTIVE") 1) 1)
    (command pause)
  )

  ;; Get the last drawn polyline
  (setq pline_ent (entlast))

  ;; Validate the polyline
  (if (and pline_ent
           (= (cdr (assoc 0 (entget pline_ent))) "LWPOLYLINE")
           (>= (cdr (assoc 90 (entget pline_ent))) 3))
    (progn
      
      ;;; --- START: Added Offset Functionality ---

      ;; --- Get offset distance ---
      (setq offset_dist (getdist "\nEnter offset distance for selection boundary <0.0>: "))
      (if (not offset_dist) (setq offset_dist 0.0)) ; Default to 0 if user presses Enter

      ;; --- Determine the selection boundary (original or offset) ---
      (setq selection_boundary pline_ent) ; Default to the polyline that was just drawn
      (setq temporary_boundary nil)      ; A flag to tell us if we created a temporary entity

      ;; If the user entered a valid offset distance, perform the offset
      (if (> offset_dist 1e-6) ; Check for a small but positive distance
        (progn
          (command "_.OFFSET" offset_dist pline_ent)
          (princ "\nSpecify a point on the side to offset for selection.")
          (command PAUSE) ; Let the user click the side
          (command "")    ; Exit the offset command cleanly
          
          (setq offset_pline (entlast)) ; The newly created offset polyline

          ;; Check if the offset was successful and created a new entity
          (if (and offset_pline (not (equal offset_pline pline_ent)))
            (progn
              (setq selection_boundary offset_pline) ; The new selection boundary is the offset polyline
              (setq temporary_boundary T)            ; Mark that we created a temporary entity to be deleted later
            )
            (princ "\nOffset failed. Using original boundary.")
          )
        )
      )
      
      ;; --- Get the selection boundary's vertex list ---
      (setq pt_list nil)
      (foreach pair (entget selection_boundary)
        (if (= (car pair) 10)
          (setq pt_list (append pt_list (list (cdr pair))))
        )
      )
      
      ;; --- Delete the temporary offset polyline if it was created ---
      (if temporary_boundary
        (entdel selection_boundary)
      )

      ;;; --- END: Added Offset Functionality ---


      ;; --- Select objects completely inside the final boundary ---
      (setq ss (ssget "_WP" pt_list))

      ;; Ask if user wants to keep the *originally drawn* polygon
      (initget "Yes No")
      (setq keep_poly (getkword "\nKeep the originally drawn polygon? [Yes/No] <No>: "))
      (if (or (not keep_poly) (= keep_poly "No"))
        (entdel pline_ent)
      )

      ;; --- Proceed if selection found ---
      (if ss
        (progn
          (princ (strcat "\n" (itoa (sslength ss)) " objects selected."))

          ;; Get base and displacement points
          (setq base_pt (getpoint "\nSpecify base point for copy: "))
          (if base_pt
            (progn
              (setq disp_pt (getpoint base_pt "\nSpecify second point (displacement): "))
              (if disp_pt
                (command "_.COPY" ss "" "_non" base_pt "_non" disp_pt)
              )
            )
          )
        )
        (princ "\nNo objects were found completely inside the selection boundary.")
      )
    )
    (princ "\nInvalid polyline. Must contain at least 3 vertices.")
  )

  ;; Cleanup
  (*error* nil)
)

;;; --- Updated load message ---
(princ "\nLISP routine 'COPYINSIDE' (v4.0 - Offset & Keep Polygon) loaded. Type COPYINSIDE to run.")
(princ)