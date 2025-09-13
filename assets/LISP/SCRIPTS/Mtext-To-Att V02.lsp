(defun c:Mtext-To-Att (/ *error* adoc mspace ref-block-ent block-name ss idx mtext-ent mtext-str mtext-pt new-block-obj attribs att found-att blkData xScale yScale zScale blkLayer)

  ;; --- Error Handler ---
  (defun *error* (msg)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (princ "\n--- MTEXT to Block Attribute Tool ---")
  (vl-load-com)
  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace adoc))

  ;; --- Select Reference Block ---
  (prompt "\nSelect a reference block instance: ")
  (setq ref-block-ent (entsel))
  (if (and ref-block-ent
           (setq ref-block-ent (car ref-block-ent))
           (= (cdr (assoc 0 (entget ref-block-ent))) "INSERT")
      )
    (progn
      (setq blkData   (entget ref-block-ent))
      (setq block-name (cdr (assoc 2 blkData)))
      (setq xScale     (cdr (assoc 41 blkData)))
      (setq yScale     (cdr (assoc 42 blkData)))
      (setq zScale     (cdr (assoc 43 blkData)))
      (setq blkLayer   (cdr (assoc 8 blkData))) ; <-- Get the layer

      (prompt "\nSelect one or more MTEXT objects: ")
      (setq ss (ssget "_:L" '((0 . "MTEXT"))))

      (if ss
        (progn
          (repeat (setq idx (sslength ss))
            (setq mtext-ent (ssname ss (setq idx (1- idx))))
            (setq mtext-str (cdr (assoc 1 (entget mtext-ent))))
            (setq mtext-pt  (cdr (assoc 10 (entget mtext-ent))))

            ;; Clean formatting and convert to uppercase
            (while (wcmatch mtext-str "*\\{*")
              (setq mtext-str (vl-string-subst "" (substr mtext-str (vl-string-search "{\\" mtext-str) 2) mtext-str)))
            (while (wcmatch mtext-str "*\\}*")
              (setq mtext-str (vl-string-subst "" "}" mtext-str)))
            (setq mtext-str (vl-string-trim " " mtext-str))
            (setq mtext-str (strcase mtext-str)) ; <-- UPPERCASE

            ;; Insert new block using same scale and layer as reference
            (setq new-block-obj
                   (vla-insertblock mspace
                                    (vlax-3d-point mtext-pt)
                                    block-name
                                    xScale yScale zScale
                                    0.0))

            ;; Set the new block's layer
            (vla-put-layer new-block-obj blkLayer)

            ;; Update SPACEUSAGE attribute
            (if (= (vla-get-hasattributes new-block-obj) :vlax-true)
              (progn
                (setq attribs (vlax-invoke new-block-obj 'GetAttributes)
                      found-att nil)
                (foreach att attribs
                  (if (= (strcase (vla-get-tagstring att)) "SPACEUSAGE")
                    (progn
                      (vla-put-textstring att mtext-str)
                      (setq found-att T))))
                (if found-att
                  (princ (strcat "\nInserted block with SPACEUSAGE = \"" mtext-str "\"."))
                  (princ "\nBlock inserted, but no 'SPACEUSAGE' attribute found.")
                )
              )
              (princ "\nWarning: Block inserted without attributes.")
            )
          )
        )
        (princ "\nNo MTEXT objects selected.")
      )
    )
    (princ "\nInvalid selection. Please select a valid block.")
  )

  (princ)
)

(princ "\nLISP loaded. Type MTEXT-TO-ATT to run.")
(princ)
