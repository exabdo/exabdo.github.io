; filepath: c:\Users\Admin\.vscode\autocad-explode-blocks\WBXT.LSP
;;; WBXT.LSP - WBLOCK objects using selected text for the filename (only text on current layer)

;;;---------------------------------------------------------------
;;; Command: WBXT
;;; Purpose: Automates creating WBLOCKs by selecting objects, then
;;;          selecting a TEXT or MTEXT object (on the current layer)
;;;          for the filename. All files are saved to a chosen folder.
;;;---------------------------------------------------------------

(defun sanitize-filename (fname)
  "Remove invalid filename characters."
  (vl-load-com)
  (foreach c '("<" ">" ":" "\"" "/" "\\" "|" "?" "*")
    (setq fname (vl-string-subst "" c fname))
  )
  fname
)

(defun c:WBXT (/ save-path ss obj-text ent-data text-val file-name full-path temp-file sel)
  (vl-load-com)

  ;; 1. Prompt user to select a folder for saving all WBLOCKs
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

      ;; 2. Main loop: select objects and filename text for each WBLOCK
      (while
        (progn
          (prompt "\n\nSelect objects for the WBLOCK (unit)... ")
          (setq ss (ssget "_:L")) ; User selects objects
        )
        (if ss
          (progn
            ;; Only allow selection of TEXT/MTEXT on current layer for filename
            (setq obj-text nil)
            (while (not obj-text)
              (prompt (strcat "\nSelect a TEXT or MTEXT object for the filename (must be on current layer: " (getvar "clayer") ")... "))
              (setq sel (entsel))
              (if sel
                (progn
                  (setq ent-data (entget (car sel)))
                  (if (and
                        (member (cdr (assoc 0 ent-data)) '("TEXT" "MTEXT"))
                        (= (cdr (assoc 8 ent-data)) (getvar "clayer"))
                      )
                    (setq obj-text sel)
                    (prompt "\n❌ Error: Selected object is not TEXT/MTEXT on the current layer. Please try again.")
                  )
                )
              )
            )

            ;; Use the selected text as the filename
            (setq ent-data (entget (car obj-text)))
            (setq text-val (cdr (assoc 1 ent-data)))
            (setq file-name (sanitize-filename text-val))
            (setq full-path (strcat save-path "\\" file-name ".dwg"))

            (if (findfile full-path)
              (prompt (strcat "\n⚠️  Warning: Overwriting existing file: " file-name ".dwg"))
            )

            ;; Create the WBLOCK
            (command "_.WBLOCK" full-path "" "0,0,0" ss "")

            (prompt (strcat "\n✅ Wrote " file-name ".dwg to " save-path))
          )
        )
      )
    )
    (prompt "\n❌ No save folder selected. Command cancelled.")
  )

  (princ)
)

(prompt "\n>> Type WBXT to run the WBLOCK by Text routine.")
(princ)