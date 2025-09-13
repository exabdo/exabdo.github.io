(defun C:TB (/ ss)
  (if (setq ss (ssget "x" (list '(0 . "insert") (cons 2 "UnitDetail")))) ; Block name is "UnitDetail"
    (progn
      (setq ss (mapcar 'cadr (ssnamex ss)))
      (foreach ename ss
        (progn
          (setq AcDbBlockReference (vlax-ename->vla-object ename))
          (foreach AcDbAttrib (vlax-invoke AcDbBlockReference 'getAttributes)
            (if (eq (strcase (vla-get-tagString AcDbAttrib)) (strcase "UNITCATEGORY"))
              (vla-put-textString AcDbAttrib "Unit")
              (if (eq (strcase (vla-get-tagString AcDbAttrib)) (strcase "UNITCLASSIFICATION"))
                (vla-put-textString AcDbAttrib "Townhouse")
              )
            )
          )
          (vlax-release-object AcDbBlockReference)
        )
      )
    )
    (princ "\nNo blocks named UnitDetail found.")
  )
   (if (setq ss (ssget "x" (list '(0 . "insert") (cons 2 "DrawingDetail")))) ; Block name is "UnitDetail"
    (progn
      (setq ss (mapcar 'cadr (ssnamex ss)))
      (foreach ename ss
        (progn
          (setq AcDbBlockReference (vlax-ename->vla-object ename))
          (foreach AcDbAttrib (vlax-invoke AcDbBlockReference 'getAttributes)
            (if (eq (strcase (vla-get-tagString AcDbAttrib)) (strcase "DRAWINGTYPE"))
              (vla-put-textString AcDbAttrib "As Built")
              
            )
          )
          (vlax-release-object AcDbBlockReference)
        )
      )
    )
    (princ "\nNo blocks named DrawingDetail found.")
  )
  (princ)
)