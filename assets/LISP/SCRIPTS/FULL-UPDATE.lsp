;; AutoLISP program to run a full update process on the drawing.
;; Combines unit updates and floor cleanup operations.
;; Version: 1.0.0
;; Command: FULL-UPDATE

(vl-load-com) ; Enable Visual LISP functions

;;;-------------------------------------------------------------------
;;; UNIT UPDATE HELPERS (from UPDATE-DETAILS.lsp)
;;;-------------------------------------------------------------------

;; Helper to update attributes based on a UnitDetail block
(defun update-unit-child-attributes (blockName vertices floorNum unitNum / detailSet i detailObj detailAttribs attrib)
  (setq detailSet (ssget "_CP" vertices (list '(0 . "INSERT") (cons 2 blockName))))
  (if detailSet
    (progn
      (princ (strcat "\n    - Updating " (itoa (sslength detailSet)) " '" blockName "' blocks..."))
      (setq i 0)
      (repeat (sslength detailSet)
        (setq detailObj (vlax-ename->vla-object (ssname detailSet i)))
        (setq detailAttribs (vlax-invoke detailObj 'GetAttributes))
        (foreach attrib detailAttribs
          (cond
            ((= (strcase (vla-get-tagstring attrib)) "UNITNUMBER") (vla-put-textstring attrib unitNum))
            ((= (strcase (vla-get-tagstring attrib)) "FLOORNUMBER") (vla-put-textstring attrib floorNum))
          )
        )
        (setq i (1+ i))
      )
    )
  )
)

;; Processes a single unit polyline
(defun process-unit-polyline (polyline / vertices unitDetailSet unitDetailObj unitAttribs floorNumVal unitNumVal)
  (princ (strcat "\n  - Processing unit boundary: " (vla-get-handle (vlax-ename->vla-object polyline))))
  (setq vertices (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget polyline))))
  (if (not vertices)
    (princ "\n    - Warning: Could not get vertices. Skipping.")
    (progn
      (setq unitDetailSet (ssget "_CP" vertices '((0 . "INSERT") (2 . "UnitDetail"))))
      (cond
        ((not unitDetailSet) (princ "\n    - Warning: No 'UnitDetail' block found. Skipping."))
        ((> (sslength unitDetailSet) 1) (princ "\n    - Warning: Multiple 'UnitDetail' blocks found. Skipping."))
        (t
          (setq unitDetailObj (vlax-ename->vla-object (ssname unitDetailSet 0)))
          (setq unitAttribs (vlax-invoke unitDetailObj 'GetAttributes))
          (foreach attrib unitAttribs
            (cond
              ((= (strcase (vla-get-tagstring attrib)) "UNITNUMBER") (setq unitNumVal (vla-get-textstring attrib)))
              ((= (strcase (vla-get-tagstring attrib)) "FLOORNUMBER") (setq floorNumVal (vla-get-textstring attrib)))
            )
          )
          (if (or (not unitNumVal) (not floorNumVal))
            (princ "\n    - Warning: UNIT/FLOOR attributes not found in UnitDetail. Skipping.")
            (progn
              (princ (strcat "\n      - Found UnitDetail. Floor: " floorNumVal " | Unit: " unitNumVal))
              (update-unit-child-attributes "SpaceDetail" vertices floorNumVal unitNumVal)
              (update-unit-child-attributes "DoorDetail" vertices floorNumVal unitNumVal)
              (update-unit-child-attributes "WindowDetail" vertices floorNumVal unitNumVal)
            )
          )
        )
      )
    )
  )
)

;;;-------------------------------------------------------------------
;;; FLOOR CLEANUP HELPERS (from CLEANUP-FLOOR-DETAILS.lsp)
;;;-------------------------------------------------------------------

;; Helper to update and clean attributes based on a FloorDetail block
(defun update-floor-child-attributes (blockName vertices floorNum / detailSet i detailObj detailAttribs attrib clearedCount updatedCount)
  (setq detailSet (ssget "_CP" vertices (list '(0 . "INSERT") (cons 2 blockName))))
  (if detailSet
    (progn
      (setq clearedCount 0)
      (setq updatedCount 0)
      (princ (strcat "\n    - Processing " (itoa (sslength detailSet)) " '" blockName "' blocks..."))
      (setq i 0)
      (repeat (sslength detailSet)
        (setq detailObj (vlax-ename->vla-object (ssname detailSet i)))
        (setq detailAttribs (vlax-invoke detailObj 'GetAttributes))
        (foreach attrib detailAttribs
          (cond
            ((= (strcase (vla-get-tagstring attrib)) "FLOORNUMBER")
             (vla-put-textstring attrib floorNum)
             (setq updatedCount (1+ updatedCount))
            )
            ((and (= (strcase (vla-get-tagstring attrib)) "UNITNUMBER") (= (vla-get-textstring attrib) "000"))
             (vla-put-textstring attrib "")
             (setq clearedCount (1+ clearedCount))
            )
          )
        )
        (setq i (1+ i))
      )
      (princ (strcat "\n      - Updated FLOORNUMBER for " (itoa updatedCount) " blocks."))
      (if (> clearedCount 0)
        (princ (strcat "\n      - Cleared UNITNUMBER for " (itoa clearedCount) " blocks."))
      )
    )
  )
)

;; Processes a single floor polyline
(defun process-floor-polyline (polyline / vertices floorDetailSet floorDetailObj floorAttribs floorNumVal)
  (princ (strcat "\n  - Processing floor boundary: " (vla-get-handle (vlax-ename->vla-object polyline))))
  (setq vertices (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget polyline))))
  (if (not vertices)
    (princ "\n    - Warning: Could not get vertices. Skipping.")
    (progn
      (setq floorDetailSet (ssget "_CP" vertices '((0 . "INSERT") (2 . "FloorDetail"))))
      (cond
        ((not floorDetailSet) (princ "\n    - Warning: No 'FloorDetail' block found. Skipping."))
        ((> (sslength floorDetailSet) 1) (princ "\n    - Warning: Multiple 'FloorDetail' blocks found. Skipping."))
        (t
          (setq floorDetailObj (vlax-ename->vla-object (ssname floorDetailSet 0)))
          (setq floorAttribs (vlax-invoke floorDetailObj 'GetAttributes))
          (foreach attrib floorAttribs
            (if (= (strcase (vla-get-tagstring attrib)) "FLOORNUMBER")
              (setq floorNumVal (vla-get-textstring attrib))
            )
          )
          (if (not floorNumVal)
            (princ "\n    - Warning: FLOORNUMBER attribute not found in FloorDetail. Skipping.")
            (progn
              (princ (strcat "\n      - Found FloorDetail. Extracted FLOORNUMBER: " floorNumVal))
              (update-floor-child-attributes "SpaceDetail" vertices floorNumVal)
              (update-floor-child-attributes "DoorDetail" vertices floorNumVal)
              (update-floor-child-attributes "WindowDetail" vertices floorNumVal)
            )
          )
        )
      )
    )
  )
)

;;;-------------------------------------------------------------------
;;; MAIN COMMAND
;;;-------------------------------------------------------------------

(defun c:FULL-UPDATE ( / *error* adoc allUnitPolylines allFloorPolylines i total)
  (defun *error* (msg)
    (if adoc (vla-endundomark adoc))
    (if (not (wcmatch (strcase msg) "*QUIT*,*CANCEL*")) (princ (strcat "\nError: " msg)))
    (princ)
  )

  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark adoc)

  (princ "\n--- STARTING FULL UPDATE PROCESS ---")

  ;; 1. Run Unit Updates
  (princ "\n\n--- Step 1: Processing all unit boundaries ---")
  (setq allUnitPolylines (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Unit") (-4 . "&") (70 . 1))))
  (if allUnitPolylines
    (progn
      (setq total (sslength allUnitPolylines))
      (princ (strcat "\nFound " (itoa total) " unit boundaries."))
      (setq i 0)
      (repeat total
        (process-unit-polyline (ssname allUnitPolylines i))
        (setq i (1+ i))
      )
    )
    (princ "\nNo unit boundaries found on layer 'Unit'.")
  )

  ;; 2. Run Floor Updates
  (princ "\n\n--- Step 2: Processing all floor boundaries ---")
  (setq allFloorPolylines (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Floor") (-4 . "&") (70 . 1))))
  (if allFloorPolylines
    (progn
      (setq total (sslength allFloorPolylines))
      (princ (strcat "\nFound " (itoa total) " floor boundaries."))
      (setq i 0)
      (repeat total
        (process-floor-polyline (ssname allFloorPolylines i))
        (setq i (1+ i))
      )
    )
    (princ "\nNo floor boundaries found on layer 'Floor'.")
  )

  ;; 3. Renumber Spaces within Building Outlines
  (princ "\n\n--- Step 3: Renumbering spaces within building outlines ---")
  (setq allBuildingOutlines (ssget "_X" '((0 . "LWPOLYLINE") (8 . "BuildingOutline") (-4 . "&") (70 . 1))))
  (if allBuildingOutlines
    (progn
      (setq total (sslength allBuildingOutlines))
      (princ (strcat "\nFound " (itoa total) " building outlines."))
      (setq i 0)
      (repeat total
        (process-building-outline (ssname allBuildingOutlines i))
        (setq i (1+ i))
      )
    )
    (princ "\nNo building outlines found on layer 'BuildingOutline'.")
  )

  (princ "\n\n--- FULL UPDATE PROCESS COMPLETE ---")
  (vla-endundomark adoc)
  (princ)
)


;;;-------------------------------------------------------------------
;;; BUILDING OUTLINE HELPERS
;;;-------------------------------------------------------------------

;; Processes a single building outline polyline to renumber spaces
(defun process-building-outline (polyline / vertices ss count i blk att tag)
  (princ (strcat "\n  - Processing building outline: " (vla-get-handle (vlax-ename->vla-object polyline))))
  (setq vertices (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget polyline))))
  (if (not vertices)
    (princ "\n    - Warning: Could not get vertices. Skipping.")
    (progn
      (setq ss (ssget "_CP" vertices '((0 . "INSERT") (2 . "SpaceDetail") (66 . 1))))
      (if ss
        (progn
          (setq count 1)
          (setq i 0)
          (princ (strcat "\n    - Found " (itoa (sslength ss)) " 'SpaceDetail' blocks to renumber."))
          (repeat (sslength ss)
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
          (princ (strcat "\n    - Successfully renumbered " (itoa (- count 1)) " blocks."))
        )
        (princ "\n    - No 'SpaceDetail' blocks found within this outline.")
      )
    )
  )
)

(princ "\nLoaded FULL-UPDATE command.")
(princ)
