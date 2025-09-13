(defun c:RenumberSpaceNumbersAuto (/ ent obj layername ss i blk att tag count)
  (vl-load-com)
  (setq count 1)

  (princ "\nSelect any block or attribute on the desired layer... ")
  (setq ent (car (entsel))) ; User selects a block or attribute

  (if ent
    (progn
      (setq obj (vlax-ename->vla-object ent))
      
      ;; If user clicks an attribute directly, get its parent block
      (if (eq "AcDbAttribute" (vla-get-objectname obj))
        (setq obj (vla-objectidtoobject (vla-get-activedocument (vlax-get-acad-object)) (vla-get-ownerid obj)))
      )

      ;; Get layer name of the selected block
      (setq layername (vla-get-layer obj))
      (princ (strcat "\nTarget layer: " layername))

      ;; Get all blocks on that layer with attributes
      (setq ss (ssget (list '(0 . "INSERT") '(66 . 1) (cons 8 layername))))

      (if ss
        (progn
          (setq i 0)
          (while (< i (sslength ss))
            (setq blk (vlax-ename->vla-object (ssname ss i)))
            (foreach att (vlax-invoke blk 'GetAttributes)
              (setq tag (strcase (vla-get-tagstring att)))
              (if (= tag "SPACENUMBER")
                (progn
                  (vla-put-textstring att (itoa count))
                  (setq count (1+ count))
                )
              )
            )
            (setq i (1+ i))
          )
          (princ (strcat "\nUpdated " (itoa (1- count)) " SPACENUMBER attributes on layer " layername "."))
        )
        (princ "\nNo matching blocks found on that layer.")
      )
    )
    (princ "\nNothing selected.")
  )

  (princ)
)
