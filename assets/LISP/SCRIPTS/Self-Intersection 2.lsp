; Self-Intersection Test with Circle Markers by Lee McDonnell (modified)

(defun c:SelfInt (/ ent iArr vLst iLst ptLst)
  (vl-load-com)
  (if (and (setq ent (vlax-ename->vla-object (car (entsel "\nSelect LWPline..."))))
           (eq "AcDbPolyline" (vla-get-ObjectName ent))
           (> (vlax-safearray-get-u-bound
               (setq iArr (vlax-variant-value
                            (vla-IntersectWith ent ent acExtendNone))) 1) 0))
    (progn
      (setq vLst (mapcar '(lambda (x) (append (cdr x) '(0.0)))
                       (vl-remove-if-not '(lambda (x) (= 10 (car x)))
                                         (entget (vlax-vla-object->ename ent)))))
      (setq iLst (vlax-safearray->list iArr))
      (while (not (zerop (length iLst)))
        (setq ptLst (cons (list (car iLst) (cadr iLst) (caddr iLst)) ptLst)
              iLst (cdddr iLst)))
      (setq ptLst (vl-remove-if '(lambda (x) (member x vLst)) ptLst))
      (if ptLst
        (progn  ; Start of the changes
          (alert (strcat "Object Intersects Itself in " (rtos (length ptLst) 2 0) " place(s).\n\n"
                         (vl-princ-to-string ptLst)))
          (foreach pt ptLst  ; Loop through intersection points
            (command "CIRCLE" pt 0.1) ; Draw a circle at each point. Adjust 0.1 for radius
            ) ; end foreach
          ) ; End of the changes
        (princ "\n<!> Object Doesn't Intersect Itself <!>")))
    (princ "\n<!> Object isn't a LWPline, or doesn't Intersect Itself <!>"))
  (princ))