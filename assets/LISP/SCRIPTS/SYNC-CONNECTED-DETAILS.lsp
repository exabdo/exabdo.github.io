;; AutoLISP program to synchronize attributes of connected doors and windows.
;; Version: 1.0.0
;; Command: SYNC-CONNECTED-DETAILS

(vl-load-com) ; Enable Visual LISP functions

;; Helper to get all vertex coordinates of a polyline
(defun get-polyline-vertices (pline-ent / vertices)
  (setq vertices (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget pline-ent))))
  vertices
)

;; Helper to check if two polylines share any vertex
(defun polylines-share-vertex-p (pline1-vlist pline2-vlist / found)
  (setq found nil)
  (foreach v1 pline1-vlist
    (if (not found)
      (foreach v2 pline2-vlist
        (if (equal v1 v2 1e-6) ; Use a tolerance for floating point comparison
          (setq found T)
        )
      )
    )
  )
  found
)

;; Helper to update attributes of a detail block inside a given polyline
(defun update-detail-block-in-polyline (pline-ent block-name floor-num unit-num space-num / vertices detail-set detail-obj attribs)
  (setq vertices (get-polyline-vertices pline-ent))
  (if vertices
    (progn
      (setq detail-set (ssget "_CP" vertices (list '(0 . "INSERT") (cons 2 block-name))))
      (if (and detail-set (= 1 (sslength detail-set)))
        (progn
          (setq detail-obj (vlax-ename->vla-object (ssname detail-set 0)))
          (setq attribs (vlax-invoke detail-obj 'GetAttributes))
          (foreach attrib attribs
            (cond
              ((= (strcase (vla-get-tagstring attrib)) "FLOORNUMBER") (vla-put-textstring attrib floor-num))
              ((= (strcase (vla-get-tagstring attrib)) "UNITNUMBER") (vla-put-textstring attrib unit-num))
              ((= (strcase (vla-get-tagstring attrib)) "SPACENUMBER") (vla-put-textstring attrib space-num))
            )
          )
          (princ (strcat "\n    - Updated '" block-name "' in polyline: " (vla-get-handle (vlax-ename->vla-object pline-ent))))
        )
        (princ (strcat "\n    - Warning: Found 0 or >1 '" block-name "' blocks in connected polyline. Skipping."))
      )
    )
  )
)

;; Main command
(defun c:SYNC-CONNECTED-DETAILS ( / *error* adoc all-spaces all-doors all-windows i j space-ent space-vlist door-ent window-ent floor-val unit-val space-val)
  (defun *error* (msg)
    (if adoc (vla-endundomark adoc))
    (if (not (wcmatch (strcase msg) "*QUIT*,*CANCEL*")) (princ (strcat "\nError: " msg)))
    (princ)
  )

  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark adoc)

  (princ "\n--- STARTING DETAIL SYNCHRONIZATION ---")

  ;; Get all relevant polylines and blocks once
  (setq all-spaces (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Space") (-4 . "&") (70 . 1))))
  (setq all-doors (ssget "_X" '((0 . "LWPOLYLINE") (8 . "door") (-4 . "&") (70 . 1))))
  (setq all-windows (ssget "_X" '((0 . "LWPOLYLINE") (8 . "window") (-4 . "&") (70 . 1))))

  (if all-spaces
    (progn
      (princ (strcat "\nFound " (itoa (sslength all-spaces)) " space boundaries to process."))
      (setq i 0)
      (repeat (sslength all-spaces)
        (setq space-ent (ssname all-spaces i))
        (princ (strcat "\n\n- Processing Space: " (vla-get-handle (vlax-ename->vla-object space-ent))))

        ;; Get SpaceDetail attributes
        (setq space-vlist (get-polyline-vertices space-ent))
        (setq space-detail-set (ssget "_CP" space-vlist '((0 . "INSERT") (2 . "SpaceDetail"))))

        (if (and space-detail-set (= 1 (sslength space-detail-set)))
          (progn
            (setq space-detail-obj (vlax-ename->vla-object (ssname space-detail-set 0)))
            (setq floor-val "" unit-val "" space-val "")
            (foreach attrib (vlax-invoke space-detail-obj 'GetAttributes)
              (cond
                ((= (strcase (vla-get-tagstring attrib)) "FLOORNUMBER") (setq floor-val (vla-get-textstring attrib)))
                ((= (strcase (vla-get-tagstring attrib)) "UNITNUMBER") (setq unit-val (vla-get-textstring attrib)))
                ((= (strcase (vla-get-tagstring attrib)) "SPACENUMBER") (setq space-val (vla-get-textstring attrib)))
              )
            )
            (princ (strcat "\n  - Found SpaceDetail. F:" floor-val " U:" unit-val " S:" space-val))

            ;; Find and update connected doors
            (if all-doors
              (progn
                (setq j 0)
                (repeat (sslength all-doors)
                  (setq door-ent (ssname all-doors j))
                  (if (polylines-share-vertex-p space-vlist (get-polyline-vertices door-ent))
                    (update-detail-block-in-polyline door-ent "DoorDetail" floor-val unit-val space-val)
                  )
                  (setq j (1+ j))
                )
              )
            )

            ;; Find and update connected windows
            (if all-windows
              (progn
                (setq j 0)
                (repeat (sslength all-windows)
                  (setq window-ent (ssname all-windows j))
                  (if (polylines-share-vertex-p space-vlist (get-polyline-vertices window-ent))
                    (update-detail-block-in-polyline window-ent "WindowDetail" floor-val unit-val space-val)
                  )
                  (setq j (1+ j))
                )
              )
            )
          )
          (princ "\n  - Warning: Found 0 or >1 'SpaceDetail' blocks. Skipping space.")
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo space boundaries found on layer 'Space'.")
  )

  (vla-endundomark adoc)
  (princ "\n\n--- SYNCHRONIZATION COMPLETE ---")
  (princ)
)

(princ "\nLoaded command: SYNC-CONNECTED-DETAILS")
(princ)
