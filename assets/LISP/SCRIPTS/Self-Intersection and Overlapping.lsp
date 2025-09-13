; Self-Intersection and Overlapping Vertex Test with Circle Markers

(defun c:SelfIntOvlp (/ ent iArr vLst iLst ptLst ovlpLst)
  (vl-load-com)
  (if (and (setq ent (vlax-ename->vla-object (car (entsel "\nSelect LWPline..."))))
           (eq "AcDbPolyline" (vla-get-ObjectName ent)))
    (progn
      (setq vLst (mapcar '(lambda (x) (append (cdr x) '(0.0)))
                       (vl-remove-if-not '(lambda (x) (= 10 (car x)))
                                         (entget (vlax-vla-object->ename ent)))))

      ; Self-Intersection Check
      (setq iArr (vlax-variant-value (vla-IntersectWith ent ent acExtendNone)))
      (if (> (vlax-safearray-get-u-bound iArr 1) 0)
        (progn
          (setq iLst (vlax-safearray->list iArr))
          (while (not (zerop (length iLst)))
            (setq ptLst (cons (list (car iLst) (cadr iLst) (caddr iLst)) ptLst)
                  iLst (cdddr iLst)))
          (setq ptLst (vl-remove-if '(lambda (x) (member x vLst)) ptLst))
          (if ptLst
            (progn
              (alert (strcat "Object Intersects Itself in " (rtos (length ptLst) 2 0) " place(s).\n\n"
                              (vl-princ-to-string ptLst)))
              (foreach pt ptLst
                (command "CIRCLE" pt 0.1)) ; Draw a circle at each intersection point.
              )
            (princ "\n<!> Object Doesn't Intersect Itself <!>")))
        )
      )

      ; Overlapping Vertex Check (Simplified and Corrected)
      (setq ovlpLst '())
      (foreach v1 vLst
        (foreach v2 vLst
          (if (and (not (eq v1 v2)) ; Don't compare a vertex to itself
                   (equal (car v1) (car v2)) ; X-coordinates match
                   (equal (cadr v1) (cadr v2)) ; Y-coordinates match
                   (not (member v1 ovlpLst)) ; Check if v1 has already been marked
                   (not (member v2 ovlpLst)) ; Check if v2 has already been marked
                   )
            (progn
              (setq ovlpLst (append ovlpLst (list v1 v2))) ; Store both vertices
              (command "CIRCLE" v1 0.1) ; Draw circle at overlapping vertex 1
              (command "CIRCLE" v2 0.1) ; Draw circle at overlapping vertex 2
            )
          )
        )
      )

      (if ovlpLst
        (alert (strcat "Object has overlapping vertices at " (length (remove-duplicates (mapcar 'car ovlpLst))) " place(s).\n\n"
                        (vl-princ-to-string (remove-duplicates (mapcar 'car ovlpLst))))) ; Display the list of overlapping points (no duplicates)
        (princ "\n<!> Object has no overlapping vertices <!>"))

    ); <- This closing parenthesis was missing!
    (princ "\n<!> Object isn't a LWPline <!>"))
  (princ))