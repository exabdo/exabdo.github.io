;  Self-Intersection Test by Lee McDonnell  ~  17.03.09

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
   (alert (strcat "Object Intersects Itself in " (rtos (length ptLst) 2 0) " place(s).\n\n"
              (vl-princ-to-string ptLst)))
   (princ "\n<!> Object Doesn't Intersect Itself <!>")))
   (princ "\n<!> Object isn't a LWPline, or doesn't Intersect Itself <!>"))
 (princ))