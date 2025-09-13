;;; WBXT.LSP - WBLOCK objects using selected text for the filename

;;;------------------------------------------------------------------

;;; Command: WBXT

;;; Purpose: Automates creating WBLOCKs by selecting objects and then

;;;          selecting a text or mtext object for the filename.

;;;          A single save path is set at the beginning.

;;;------------------------------------------------------------------



(defun sanitize-filename (fname)

  "Removes characters invalid for a filename."

  (vl-load-com)

  (setq fname (vl-string-subst "" "<" fname))

  (setq fname (vl-string-subst "" ">" fname))

  (setq fname (vl-string-subst "" ":" fname))

  (setq fname (vl-string-subst "" "\"" fname))

  (setq fname (vl-string-subst "" "/" fname))

  (setq fname (vl-string-subst "" "\\" fname))

  (setq fname (vl-string-subst "" "|" fname))

  (setq fname (vl-string-subst "" "?" fname))

  (setq fname (vl-string-subst "" "*" fname))

  fname

)



(defun c:WBXT (/ save-path ss obj-text ent-data text-val file-name full-path temp-file)

  (vl-load-com)



  ;; --- 1. Get the Save Path (only once) ---

  (setq temp-file

         (getfiled "Select the folder to save all units"

                   (getvar "dwgprefix")

                   "dwg"

                   1

         )

  )



  (if temp-file

    (progn

      (setq save-path (vl-filename-directory temp-file))

      (prompt (strcat "\n✅ All units will be saved in: " save-path))



      ;; --- 2. Start the main loop ---

      (while

        (progn

          (setq ss nil obj-text nil) ; Reset variables

          (prompt "\n\nSelect objects for the WBLOCK (unit)... ")

          (setq ss (ssget "_:L")) ; Get selection set, prevent selecting nothing

        ) ; progn

         (if ss

           (progn

             (prompt "\nSelect a TEXT or MTEXT object for the filename... ")

             (setq obj-text (entsel)) ; Select a single entity for the name



             (if (and obj-text

                      (setq ent-data (entget (car obj-text)))

                      (or (= "TEXT" (cdr (assoc 0 ent-data)))

                          (= "MTEXT" (cdr (assoc 0 ent-data)))

                      )

                 )

                ; --- If a valid text object was selected ---

               (progn

                 (setq text-val (cdr (assoc 1 ent-data)))

                 (setq file-name (sanitize-filename text-val))

                 (setq full-path (strcat save-path "\\" file-name ".dwg"))



                 (if (findfile full-path)

                   (prompt (strcat "\n⚠️  Warning: Overwriting existing file: " file-name ".dwg"))

                 )

                 

                 ;; --- Execute the WBLOCK command ---

                 (command "_.WBLOCK" full-path "" "0,0,0" ss "")

                 

                 (prompt (strcat "\n✅ Wrote " file-name ".dwg to " save-path))

               )

                ; --- If something other than text was selected ---

               (prompt "\n❌ Error: You did not select a valid TEXT or MTEXT object. Please try again.")

             ) ; if obj-text is valid

           )   ; progn

         )     ; if ss

      ) ; while

    ) ; progn

    (prompt "\n❌ No save folder selected. Command cancelled.")

  ) ; if temp-file

  (princ) ; Clean exit

)



(prompt "\n>> Type WBXT to run the WBLOCK by Text routine.")

(princ)