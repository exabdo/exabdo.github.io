(defun c:JOINTRACE (/ ss fuzz refEnt refLayer currLayer before after newents before_ents
                      i ent obj ss_new pt len entStart entEnd nearPt minDist d
                      connecting_lines final_selection master_vertex_list vertex_pair owner_ent vertex_pt
                      del_originals total_ents count final_ss)

  (vl-load-com)

  ;; Get current layer
  (setq currLayer (getvar "CLAYER"))
  (prompt (strcat "\nNew polyline will be created on CURRENT layer: " currLayer))

  ;; Get reference entity and layer
  (setq refEnt (car (entsel "\nClick an object on the REFERENCE layer (to trace): ")))
  (if (null refEnt)
    (progn (prompt "\nNo object selected. Exiting.") (exit))
  )
  (setq refLayer (cdr (assoc 8 (entget refEnt))))
  (prompt (strcat "\nReference layer set to: " refLayer))

  ;; Get fuzz distance
  (initget 6)
  (setq fuzz (getreal "\nEnter fuzz distance for joining [0.01]: "))
  (if (null fuzz) (setq fuzz 0.01))

  ;; User selects objects manually from the reference layer
  (prompt (strcat "\nSelect objects to trace on layer '" refLayer "': "))
  (setq ss (ssget (list (cons 8 refLayer) (cons 0 "LINE,ARC,LWPOLYLINE,POLYLINE"))))

  (if ss
    (progn
      ;; --- ENHANCEMENT: Ask to delete original objects ---
      (initget "Yes No")
      (setq del_originals (getkword "\nDelete original source objects after tracing? [Yes/No] <No>: "))
      (if (null del_originals) (setq del_originals "No"))

      (command "._UNDO" "_Begin")

      ;; Save all polylines before
      (setq before (ssget "_X" '((0 . "LWPOLYLINE"))))
      (setq before_ents '())
      (if before
        (progn
          (setq i 0)
          (while (< i (sslength before))
            (setq before_ents (cons (ssname before i) before_ents))
            (setq i (1+ i))
          )
        )
      )

      ;; Copy selection in place
      (command "._COPY" ss "" "0,0,0" "0,0,0")
      (setq ss_new (ssget "_P"))

      ;; First Pass Join
      (command "._PEDIT" "M")
      (setq i 0)
      (while (< i (sslength ss_new))
        (command (ssname ss_new i))
        (setq i (1+ i))
      )
      (command "" "Y" "J" (rtos fuzz 2 8) "")

      ;; Identify new polylines created
      (setq after (ssget "_X" '((0 . "LWPOLYLINE"))))
      (setq newents '())
      (if after
        (progn
          (setq i 0)
          (while (< i (sslength after))
            (setq ent (ssname after i))
            (if (not (member ent before_ents))
              (setq newents (cons ent newents))
            )
            (setq i (1+ i))
          )
        )
      )
      (setq newents (reverse newents))

      ;; Set color/layer for the new polylines
      (foreach ent newents
        (setq obj (vlax-ename->vla-object ent))
        (vla-put-layer obj currLayer)
        (vla-put-color obj 1) ; Red
      )
      
      (prompt "\nAnalyzing segments for gaps...")

      ;; Robust Gap-Filling Logic
      (setq connecting_lines '())
      (setq master_vertex_list '())
      (foreach ent newents
        (setq obj (vlax-ename->vla-object ent))
        (setq i 0)
        (while (<= i (fix (vlax-curve-getendparam obj)))
          (setq pt (vlax-curve-getpointatparam obj i))
          (setq master_vertex_list (cons (cons pt ent) master_vertex_list))
          (setq i (1+ i))
        )
      )

      ;; --- ENHANCEMENT: Add progress indicator ---
      (setq total_ents (length newents))
      (setq count 0)

      (foreach ent newents
        (setq count (1+ count))
        (princ (strcat "\rProcessing segment " (itoa count) " of " (itoa total_ents) "... "))

        (setq obj (vlax-ename->vla-object ent))
        (setq entStart (vlax-curve-getstartpoint obj))
        (setq entEnd (vlax-curve-getendpoint obj))

        (foreach endpoint (list entStart entEnd)
          (setq nearPt nil)
          (setq minDist nil)
          (foreach vertex_pair master_vertex_list
            (setq owner_ent (cdr vertex_pair))
            (setq vertex_pt (car vertex_pair))
            (if (/= ent owner_ent)
              (progn
                (setq d (distance endpoint vertex_pt))
                (if (and (< d fuzz) (or (not minDist) (< d minDist)))
                  (progn (setq minDist d) (setq nearPt vertex_pt))
                )
              )
            )
          )
          (if nearPt
            (setq connecting_lines
              (cons
                (entmakex
                  (list (cons 0 "LWPOLYLINE") (cons 100 "AcDbEntity") (cons 8 currLayer)
                        (cons 62 1) (cons 100 "AcDbPolyline") (cons 90 2)
                        (cons 10 endpoint) (cons 10 nearPt) (cons 70 0)
                  )
                )
                connecting_lines
              )
            )
          )
        )
      )

      ;; Final Join Attempt
      (princ "\rFinalizing join...                                  \n") ; Clear progress line
      (setq final_selection (append newents connecting_lines))
      (if (> (length final_selection) 1)
        (progn
          (command "._PEDIT" "M")
          (foreach ent final_selection (command ent))
          (command "" "J" (rtos fuzz 2 8) "")
          (setq final_ss (ssget "_P"))
        )
        (setq final_ss (ssget "L")) ; If only one segment, select it
      )

      ;; --- ENHANCEMENT: Delete originals if requested ---
      (if (= del_originals "Yes")
        (command "._ERASE" ss "")
      )
      
      (command "._UNDO" "_End")

      ;; --- ENHANCEMENT: Detailed final report ---
      (terpri)
      (prompt "--- Trace and Join Complete ---")
      (prompt (strcat "\n  - Original objects selected: " (itoa (sslength ss))))
      (prompt (strcat "\n  - Initial segments created: " (itoa (length newents))))
      (prompt (strcat "\n  - Gaps bridged with new lines: " (itoa (length connecting_lines))))
      (prompt (strcat "\n  - Final joined polylines: " (if final_ss (itoa (sslength final_ss)) "0")))
      (prompt (strcat "\n  - Originals deleted: " del_originals))
      (terpri)

    )
    (prompt "\nNo valid selection.")
  )

  (princ)
)