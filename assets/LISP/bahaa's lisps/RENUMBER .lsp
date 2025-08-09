(defun C:RENUMBER ()
  (vl-load-com)

  ;; Prompt user for input
  (setq start-find (getint "\nEnter START number to find: "))
  (setq end-find (getint "\nEnter END number to find: "))
  (setq start-replace (getint "\nEnter START number to replace with: "))

  ;; Setup
  (setq acadApp (vlax-get-acad-object))
  (setq modelSpace (vla-get-ModelSpace (vla-get-ActiveDocument acadApp)))

  ;; Calculate number of items to process
  (setq total (- end-find start-find))
  (setq i 0)

  ;; Loop through the number range
  (repeat (+ 1 total) ; inclusive range
    (setq current-find (+ start-find i))
    (setq current-replace (+ start-replace i))
    (setq find-string (itoa current-find))
    (setq replace-string (itoa current-replace))

    (vlax-for ent modelSpace
      (cond
        ((and
           (eq (vla-get-ObjectName ent) "AcDbText")
           (= (vla-get-TextString ent) find-string)
         )
         (vla-put-TextString ent replace-string)
        )
        ((and
           (eq (vla-get-ObjectName ent) "AcDbMText")
           (= (vla-get-TextString ent) find-string)
         )
         (vla-put-TextString ent replace-string)
        )
      )
    )
    (setq i (1+ i))
  )

  ;; Done message
  (princ (strcat "\nReplaced numbers " (itoa start-find) " to " (itoa end-find)
                 " with " (itoa start-replace) " to "
                 (itoa (+ start-replace total)) "."))
  (princ)
)
