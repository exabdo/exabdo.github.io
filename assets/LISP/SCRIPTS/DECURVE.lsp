(defun c:Jm ()
  (vl-load-com) ; Load Visual LISP extensions
  (setq ss (ssget '((0 . "LWPOLYLINE,CIRCLE,ARC")))) ; Select polylines, arcs, and circles

  (if ss
    (progn
      (command "_.PEDIT" "_M" ss "_J" "_Y" "") ; Join selected objects
      (princ "\nSuccessfully joined the selected objects into a single polyline.")
    )
    (princ "\nNo valid objects selected.")
  )
  (princ)
)
