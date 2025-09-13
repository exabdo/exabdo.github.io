(vl-load-com)

;;;-------------------------------------------------------------------
;;; COMMON HELPER FUNCTIONS
;;;-------------------------------------------------------------------

(defun get-attribute-value (block-obj tag-name / result)
  (setq tag-name (strcase tag-name))
  (setq result (vl-some '(lambda (att)
                          (if (= (strcase (vla-get-tagstring att)) tag-name)
                            (vla-get-textstring att)
                          )
                        )
                       (vlax-invoke block-obj 'GetAttributes)
                ))
  ;; Ensure we always return a string or nil
  (if (and result (= (type result) 'STR))
    result
    nil
  )
)

(defun set-attribute-value (block-obj tag-name new-value)
  (setq tag-name (strcase tag-name))
  (foreach att (vlax-invoke block-obj 'GetAttributes)
    (if (= (strcase (vla-get-tagstring att)) tag-name)
      (vla-put-textstring att new-value)
    )
  )
)

(defun get-polyline-vertices (pline-ent)
  (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget pline-ent)))
)

(defun get-all-block-attributes (block-obj / attribs result)
  "Returns a list of all attribute tag-value pairs for a block"
  (setq attribs (vlax-invoke block-obj 'GetAttributes))
  (setq result nil)
  (foreach att attribs
    (setq result (cons (list (vla-get-tagstring att) (vla-get-textstring att)) result))
  )
  result
)

;;;-------------------------------------------------------------------
;;; RELATIONSHIP ANALYSIS FUNCTIONS
;;;-------------------------------------------------------------------

(defun analyze-door-space-relationships ( / all-doors all-spaces door-relationships i j door-ent door-vlist door-handle door-detail-set door-material door-type door-detail-obj space-ent space-vlist space-detail-set space-detail-obj space-usage space-num unit-num floor-num connected-spaces)
  "Analyzes relationships between doors and spaces"
  (princ "\n\n=== DOOR-SPACE RELATIONSHIP ANALYSIS ===")
  (setq all-doors (ssget "_X" '((0 . "LWPOLYLINE") (8 . "door") (-4 . "&") (70 . 1))))
  (setq all-spaces (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Space") (-4 . "&") (70 . 1))))
  (setq door-relationships nil)
  
  (if (and all-doors all-spaces)
    (progn
      (princ (strcat "\nFound " (itoa (sslength all-doors)) " doors and " (itoa (sslength all-spaces)) " spaces."))
      (setq i 0)
      (repeat (sslength all-doors)
        (setq door-ent (ssname all-doors i))
        (setq door-vlist (get-polyline-vertices door-ent))
        (setq door-handle (vla-get-handle (vlax-ename->vla-object door-ent)))
        
        ;; Find door detail block and get material info
        (setq door-detail-set (ssget "_CP" door-vlist '((0 . "INSERT") (2 . "DoorDetail"))))
        (setq door-material "Unknown")
        (setq door-type "Unknown")
        
        (if (and door-detail-set (= 1 (sslength door-detail-set)))
          (progn
            (setq door-detail-obj (vlax-ename->vla-object (ssname door-detail-set 0)))
            (setq door-material (or (get-attribute-value door-detail-obj "MATERIAL") 
                                   (get-attribute-value door-detail-obj "DOORTYPE")
                                   (get-attribute-value door-detail-obj "TYPE")
                                   "Not Specified"))
            (setq door-type (or (get-attribute-value door-detail-obj "DOORTYPE")
                               (get-attribute-value door-detail-obj "TYPE")
                               "Standard"))
            ;; Ensure door-material and door-type are always strings
            (if (not (= (type door-material) 'STR)) (setq door-material "Not Specified"))
            (if (not (= (type door-type) 'STR)) (setq door-type "Standard"))
          )
        )
        
        ;; Find connected spaces
        (setq connected-spaces nil)
        (setq j 0)
        (repeat (sslength all-spaces)
          (setq space-ent (ssname all-spaces j))
          (setq space-vlist (get-polyline-vertices space-ent))
          
          (if (polylines-share-vertex-p door-vlist space-vlist)
            (progn
              ;; Get space details
              (setq space-detail-set (ssget "_CP" space-vlist '((0 . "INSERT") (2 . "SpaceDetail"))))
              (if (and space-detail-set (= 1 (sslength space-detail-set)))
                (progn
                  (setq space-detail-obj (vlax-ename->vla-object (ssname space-detail-set 0)))
                  (setq space-usage (or (get-attribute-value space-detail-obj "SPACEUSAGE") "Unknown"))
                  (setq space-num (or (get-attribute-value space-detail-obj "SPACENUMBER") "?"))
                  (setq unit-num (or (get-attribute-value space-detail-obj "UNITNUMBER") "?"))
                  (setq floor-num (or (get-attribute-value space-detail-obj "FLOORNUMBER") "?"))
                  ;; Ensure all values are strings
                  (if (not (= (type space-usage) 'STR)) (setq space-usage "Unknown"))
                  (if (not (= (type space-num) 'STR)) (setq space-num "?"))
                  (if (not (= (type unit-num) 'STR)) (setq unit-num "?"))
                  (if (not (= (type floor-num) 'STR)) (setq floor-num "?"))
                  (setq connected-spaces (cons (list space-usage space-num unit-num floor-num) connected-spaces))
                )
              )
            )
          )
          (setq j (1+ j))
        )
        
        ;; Store relationship data
        (setq door-relationships (cons (list door-handle door-material door-type connected-spaces) door-relationships))
        
        ;; Print relationship info
        (princ (strcat "\n\nDoor " (if door-handle door-handle "Unknown") ":"))
        (princ (strcat "\n  Material/Type: " (if door-material door-material "Unknown") " (" (if door-type door-type "Unknown") ")"))
        (princ (strcat "\n  Connected to " (itoa (length connected-spaces)) " spaces:"))
        (foreach space-info connected-spaces
          (if (and space-info (>= (length space-info) 4))
            (princ (strcat "\n    - " (if (car space-info) (car space-info) "Unknown") " (Space:" (if (cadr space-info) (cadr space-info) "?") 
                          ", Unit:" (if (caddr space-info) (caddr space-info) "?") ", Floor:" (if (cadddr space-info) (cadddr space-info) "?") ")"))
            (princ "\n    - Invalid space data")
          )
        )
        
        (setq i (1+ i))
      )
    )
    (princ "\nNo doors or spaces found for analysis.")
  )
  
  door-relationships
)

(defun auto-set-door-materials-by-space (door-relationships / door-info door-handle connected-spaces space-info space-usage door-ent door-vlist door-detail-set door-detail-obj material-to-set updated-count)
  "Automatically sets door materials based on connected space types"
  (if door-relationships
    (progn
      (princ "\n\n=== AUTOMATIC DOOR MATERIAL ASSIGNMENT ===")
      (setq updated-count 0)
      
      (foreach door-info door-relationships
        (setq door-handle (car door-info))
        (setq connected-spaces (cadddr door-info))
        (setq material-to-set nil)
        
        ;; Check space types and determine material
        (foreach space-info connected-spaces
          (setq space-usage (car space-info))
          ;; Ensure space-usage is a string before using strcase
          (if (and space-usage (= (type space-usage) 'STR))
            (progn
              (setq space-usage (strcase space-usage))
              (cond
                ;; Lift doors should be Steel
                ((wcmatch space-usage "*LIFT*,*ELEVATOR*")
                 (setq material-to-set "Steel")
                )
                ;; Balcony doors should be Aluminum
                ((wcmatch space-usage "*BALCONY*,*TERRACE*")
                 (setq material-to-set "Aluminum")
                )
                ;; Main entrance doors should be Steel
                ((wcmatch space-usage "*ENTRANCE*,*LOBBY*,*FOYER*")
                 (setq material-to-set "Steel")
                )
                ;; Bathroom doors can be Wood (moisture resistant)
                ((wcmatch space-usage "*BATH*,*WC*,*W.C*,*TOILET*")
                 (if (not material-to-set) (setq material-to-set "Wood"))
                )
              )
            )
          )
        )
        
        ;; Update door material if determined
        (if material-to-set
          (progn
            (setq door-ent (handent door-handle))
            (if door-ent
              (progn
                (setq door-vlist (get-polyline-vertices door-ent))
                (setq door-detail-set (ssget "_CP" door-vlist '((0 . "INSERT") (2 . "DoorDetail"))))
                (if (and door-detail-set (= 1 (sslength door-detail-set)))
                  (progn
                    (setq door-detail-obj (vlax-ename->vla-object (ssname door-detail-set 0)))
                    (set-attribute-value door-detail-obj "MATERIAL" material-to-set)
                    (set-attribute-value door-detail-obj "DOORTYPE" material-to-set)
                    (set-attribute-value door-detail-obj "TYPE" material-to-set)
                    (princ (strcat "\n  Door " (if door-handle door-handle "Unknown") " -> " material-to-set " (connected to: "))
                    (foreach space-info connected-spaces
                      (if (and space-info (car space-info))
                        (princ (strcat (car space-info) " "))
                      )
                    )
                    (princ ")")
                    (setq updated-count (1+ updated-count))
                  )
                )
              )
            )
          )
        )
      )
      
      (princ (strcat "\n\nAutomatically updated " (itoa updated-count) " doors based on space types."))
    )
  )
)

(defun modify-door-materials (door-relationships / choice door-handle new-material door-info current-material door-type connected-spaces door-ent door-vlist door-detail-set door-detail-obj)
  "Allows user to modify door materials based on analysis"
  (if door-relationships
    (progn
      (princ "\n\n=== DOOR MATERIAL MODIFICATION ===")
      (princ "\nOptions:")
      (princ "\n1. Auto-assign materials based on space types")
      (princ "\n2. Manual material assignment")
      (princ "\n3. Skip material modification")
      (princ "\nSelect option [1/2/3]: ")
      (initget "1 2 3")
      (setq choice (getkword))
      
      (cond
        ((= choice "1")
         (auto-set-door-materials-by-space door-relationships)
        )
        ((= choice "2")
         (princ "\n\n=== MANUAL DOOR MATERIAL ASSIGNMENT ===")
         (foreach door-info door-relationships
           (setq door-handle (car door-info))
           (setq current-material (cadr door-info))
           (setq door-type (caddr door-info))
           (setq connected-spaces (cadddr door-info))
           
           (princ (strcat "\n\nDoor " (if door-handle door-handle "Unknown") " (Current: " (if current-material current-material "Unknown") ")"))
           (princ "\nConnected spaces: ")
           (foreach space-info connected-spaces
             (if (and space-info (car space-info))
               (princ (strcat (car space-info) " "))
             )
           )
           
           (princ "\nChange material? [Wood/Steel/Aluminum/Glass/Skip]: ")
           (initget "Wood Steel Aluminum Glass Skip")
           (setq new-material (getkword))
           
           (if (and new-material (not (= new-material "Skip")))
             (progn
               ;; Find and update the door detail block
               (setq door-ent (handent door-handle))
               (if door-ent
                 (progn
                   (setq door-vlist (get-polyline-vertices door-ent))
                   (setq door-detail-set (ssget "_CP" door-vlist '((0 . "INSERT") (2 . "DoorDetail"))))
                   (if (and door-detail-set (= 1 (sslength door-detail-set)))
                     (progn
                       (setq door-detail-obj (vlax-ename->vla-object (ssname door-detail-set 0)))
                       ;; Try different possible attribute names for material
                       (set-attribute-value door-detail-obj "MATERIAL" new-material)
                       (set-attribute-value door-detail-obj "DOORTYPE" new-material)
                       (set-attribute-value door-detail-obj "TYPE" new-material)
                       (princ (strcat "\n  Updated door material to: " new-material))
                     )
                     (princ "\n  Warning: Could not find door detail block to update.")
                   )
                 )
                 (princ "\n  Warning: Could not find door entity.")
               )
             )
           )
         )
        )
        ((= choice "3")
         (princ "\nSkipping material modification.")
        )
      )
    )
  )
)

(defun polylines-share-vertex-p (pline1-vlist pline2-vlist / found)
  (setq found nil)
  (foreach v1 pline1-vlist
    (if (not found)
      (foreach v2 pline2-vlist
        (if (equal v1 v2 1e-6)
          (setq found T)
        )
      )
    )
  )
  found
)

;;;-------------------------------------------------------------------
;;; ORIGINAL FUNCTIONS (with minor modifications)
;;;-------------------------------------------------------------------

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

(defun process-unit-polyline (polyline / vertices unitDetailSet unitDetailObj unitAttribs floorNumVal unitNumVal)
  (princ (strcat "\n  - Processing unit boundary: " (vla-get-handle (vlax-ename->vla-object polyline))))
  (setq vertices (get-polyline-vertices polyline))
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
            ((= (strcase (vla-get-tagstring attrib)) "UNITNUMBER")
             (vla-put-textstring attrib "")
             (setq clearedCount (1+ clearedCount))
            )
          )
        )
        (setq i (1+ i))
      )
      (if (> updatedCount 0)
        (princ (strcat "\n      - Updated FLOORNUMBER for " (itoa updatedCount) " blocks."))
      )
      (if (> clearedCount 0)
        (princ (strcat "\n      - Cleared UNITNUMBER for " (itoa clearedCount) " blocks."))
      )
    )
  )
)

(defun process-floor-polyline (polyline / vertices floorDetailSet floorDetailObj floorAttribs floorNumVal)
  (princ (strcat "\n  - Processing floor boundary: " (vla-get-handle (vlax-ename->vla-object polyline))))
  (setq vertices (get-polyline-vertices polyline))
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
          (setq floorNumVal (get-attribute-value floorDetailObj "FLOORNUMBER"))
          (if (not floorNumVal)
            (princ "\n    - Warning: FLOORNUMBER attribute not found in FloorDetail. Skipping.")
            (progn
              (princ (strcat "\n      - Found FloorDetail. Floor: " floorNumVal))
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

(defun process-building-outline (polyline / vertices ss count i blk att tag)
  (princ (strcat "\n  - Processing building outline: " (vla-get-handle (vlax-ename->vla-object polyline))))
  (setq vertices (get-polyline-vertices polyline))
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

;;;-------------------------------------------------------------------
;;; MAIN LOGIC FUNCTIONS WITH USER PROMPTS
;;;-------------------------------------------------------------------

(defun run-full-update-logic ( / allUnitPolylines allFloorPolylines i total allBuildingOutlines)
  (princ "\n--- STARTING FULL UPDATE PROCESS ---")
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
  (princ "\n--- FULL UPDATE PROCESS COMPLETE ---")
)

(defun run-sync-logic ( / all-spaces all-doors all-windows i j space-ent space-vlist door-ent window-ent floor-val unit-val space-val space-detail-set space-detail-obj attrib)
  (princ "\n\n--- STARTING DETAIL SYNCHRONIZATION ---")
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
  (princ "\n--- SYNCHRONIZATION COMPLETE ---")
)

(defun run-calc-net-area-logic ( / all-units-ss i unit-ent unit-obj unit-area unit-vertices unit-detail-ss unit-detail-obj unit-num-val spaces-ss j space-ent space-obj space-vertices space-area space-detail-ss space-detail-obj space-usage bed-count bath-count total-deduction-area all-deductions-area void-shaft-area unit-classification duplex-level net-area)
  (princ "\n\n--- STARTING NET AREA AND ROOM COUNT CALCULATION ---")
  (setq all-units-ss (ssget "_X" '((0 . "LWPOLYLINE") (8 . "Unit") (-4 . "&") (70 . 1))))
  (if all-units-ss
    (progn
      (setq i 0)
      (repeat (sslength all-units-ss)
        (setq unit-ent (ssname all-units-ss i))
        (setq unit-obj (vlax-ename->vla-object unit-ent))
        (setq unit-area (vla-get-area unit-obj))
        (setq unit-vertices (get-polyline-vertices unit-ent))
        (princ (strcat "\n\nProcessing Unit: " (vla-get-handle unit-obj)))
        (princ (strcat "\n  - Gross Area: " (rtos unit-area)))
        (setq total-deduction-area 0.0 all-deductions-area 0.0 void-shaft-area 0.0 bed-count 0 bath-count 0)
        (setq unit-detail-ss (ssget "_CP" unit-vertices '((0 . "INSERT") (2 . "UnitDetail"))))
        (if (and unit-detail-ss (= 1 (sslength unit-detail-ss)))
          (progn
            (setq unit-detail-obj (vlax-ename->vla-object (ssname unit-detail-ss 0)))
            (setq unit-num-val (get-attribute-value unit-detail-obj "UNITNUMBER"))
            (setq spaces-ss (ssget "_CP" unit-vertices '((0 . "LWPOLYLINE") (8 . "space"))))
            (if spaces-ss
              (progn
                (setq j 0)
                (repeat (sslength spaces-ss)
                  (setq space-ent (ssname spaces-ss j))
                  (setq space-obj (vlax-ename->vla-object space-ent))
                  (setq space-area (vla-get-area space-obj))
                  (setq space-vertices (get-polyline-vertices space-ent))
                  (setq space-detail-ss (ssget "_CP" space-vertices '((0 . "INSERT") (2 . "SpaceDetail"))))
                  (if (and space-detail-ss (= 1 (sslength space-detail-ss)))
                    (progn
                      (setq space-detail-obj (vlax-ename->vla-object (ssname space-detail-ss 0)))
                      (setq space-usage (strcase (get-attribute-value space-detail-obj "SPACEUSAGE")))
                      (princ (strcat "\n    - Found Space with usage: '" space-usage "'. Area: " (rtos space-area)))
                      (if (and (wcmatch space-usage "*BATH*,*WC*,*W.C*,*TOILET*") (not (wcmatch space-usage "*MAID*")))
                        (setq bath-count (1+ bath-count))
                      )
                      (if (and (wcmatch space-usage "*BED*") (not (wcmatch space-usage "*MAID*")))
                        (setq bed-count (1+ bed-count))
                      )
                      (if (member space-usage '("SHAFT" "VOID"))
                          (setq void-shaft-area (+ void-shaft-area space-area))
                      )
                      (if (member space-usage '("SHAFT" "VOID" "LIFT" "STAIR" "STAIRS"))
                          (setq all-deductions-area (+ all-deductions-area space-area))
                      )
                    )
                    (princ (strcat "\n    - Warning: Found a space polyline without a single valid 'SpaceDetail' block inside. Skipping area calculation for this space."))
                  )
                  (setq j (1+ j))
                )
              )
            )
            (princ (strcat "\n  Select classification for Unit " unit-num-val ":"))
            (princ "\n    1: Duplex")
            (princ "\n    2: Triplex")
            (princ "\n    3: Apartment")
            (princ "\n    4: Unit")
            (princ "\n    5: Retail shop")
            (princ "\n    6: Office")
            (initget "1 2 3 4 5 6")
            (setq user-choice (getkword "\nEnter choice [1-6]: "))
            (setq unit-classification 
              (cond
                ((= user-choice "1") "Duplex")
                ((= user-choice "2") "Triplex")
                ((= user-choice "3") "Apartment")
                ((= user-choice "4") "Unit")
                ((= user-choice "5") "Retail shop")
                ((= user-choice "6") "Office")
              )
            )
            (setq total-deduction-area void-shaft-area)
            (if (member unit-classification '("Duplex" "Triplex"))
                (progn
                    (initget "Upper Lower")
                    (setq duplex-level (getkword (strcat "\n  Is Unit " unit-num-val " Upper or Lower? [Upper/Lower]: ")))
                    (if (= duplex-level "Upper")
                        (setq total-deduction-area all-deductions-area)
                    )
                )
            )
            (setq net-area (- unit-area total-deduction-area))
            (princ (strcat "\n  - Total Deduction: " (rtos total-deduction-area)))
            (princ (strcat "\n  - Calculated Net Area: " (rtos net-area)))
            (princ (strcat "\n  - Bathroom Count: " (itoa bath-count)))
            (princ (strcat "\n  - Bedroom Count: " (itoa bed-count)))
            (set-attribute-value unit-detail-obj "UNITAREA" (rtos net-area 2 2))
            (set-attribute-value unit-detail-obj "UNITCLASSIFICATION" unit-classification)
            (set-attribute-value unit-detail-obj "UNITNUMBATHROOM" (itoa bath-count))
            (set-attribute-value unit-detail-obj "UNITNUMBEDROOM" (itoa bed-count))
            (princ (strcat "\n  - Updated attributes in UnitDetail block."))
          )
          (princ (strcat "\n  - Warning: Found zero or multiple 'UnitDetail' blocks inside unit. Skipping this unit."))
        )
        (setq i (1+ i))
      )
    )
    (princ "\nNo unit boundaries found on layer 'Unit'.")
  )
  (princ "\n--- NET AREA CALCULATION COMPLETE ---")
)

;;;-------------------------------------------------------------------
;;; ENHANCED MASTER COMMAND WITH USER PROMPTS
;;;-------------------------------------------------------------------

(defun c:RUN-ALL-ENHANCED ( / *error* adoc detail-ss k block-obj original-scales restore-flag restore-choice insertion-point scale-factor user-choice door-relationships)
  
  (defun *error* (msg)
    (if (and original-scales restore-flag)
      (progn
        (princ "\nError occurred. Restoring original block scales.")
        (foreach block-data original-scales
          (setq block-obj (car block-data))
          (if (not (vlax-object-released-p block-obj))
            (progn
              (setq insertion-point (vlax-safearray->list (vlax-variant-value (cadr block-data))))
              (setq scale-factor (/ (caddr block-data) (vla-get-xscalefactor block-obj)))
              (vla-scaleentity block-obj (vlax-3d-point insertion-point) scale-factor)
            )
          )
        )
        (princ " Done.")
      )
    )
    (if adoc (vla-endundomark adoc))
    (if (not (wcmatch (strcase msg) "*QUIT*,*CANCEL*"))
      (princ (strcat "\nError: " msg))
    )
    (princ)
  )

  (setq adoc (vla-get-activedocument (vlax-get-acad-object)))
  (vla-startundomark adoc)

  ;; Scale down detail blocks for processing
  (setq original-scales nil restore-flag nil)
  (setq detail-ss (ssget "_X" '((0 . "INSERT") (8 . "DoorDetail,WindowDetail,SpaceDetail"))))
  (if detail-ss
    (progn
      (setq restore-flag T)
      (princ "\nScaling down detail blocks...")
      (setq k 0)
      (repeat (sslength detail-ss)
        (setq block-obj (vlax-ename->vla-object (ssname detail-ss k)))
        (setq insertion-point (vla-get-insertionpoint block-obj))
        (setq original-scales (cons (list block-obj insertion-point (vla-get-xscalefactor block-obj)) original-scales))
        (vla-scaleentity block-obj (vlax-3d-point (vlax-safearray->list (vlax-variant-value insertion-point))) 0.001)
        (setq k (1+ k))
      )
      (princ (strcat " Done. " (itoa (length original-scales)) " blocks scaled."))
    )
  )

  ;; Main menu with user choices
  (princ "\n\n=== ENHANCED MASTER COMMAND ===")
  (princ "\nAvailable operations:")
  (princ "\n1. Full Update Process (Unit/Floor/Building boundaries)")
  (princ "\n2. Detail Synchronization (Doors/Windows with Spaces)")
  (princ "\n3. Net Area Calculation")
  (princ "\n4. Door-Space Relationship Analysis")
  (princ "\n5. Run All Operations")
  
  (setq continue-processing T)
  
  (while continue-processing
    (princ "\n\nSelect operation [1/2/3/4/5/Quit]: ")
    (initget "1 2 3 4 5 Quit")
    (setq user-choice (getkword))
    
    (cond
      ((= user-choice "1")
       (princ "\n\nStarting Full Update Process...")
       (run-full-update-logic)
      )
      ((= user-choice "2")
       (princ "\n\nStarting Detail Synchronization...")
       (run-sync-logic)
      )
      ((= user-choice "3")
       (princ "\n\nStarting Net Area Calculation...")
       (run-calc-net-area-logic)
      )
      ((= user-choice "4")
       (princ "\n\nStarting Door-Space Relationship Analysis...")
       (setq door-relationships (analyze-door-space-relationships))
       (modify-door-materials door-relationships)
      )
      ((= user-choice "5")
       (princ "\n\nRunning All Operations...")
       (run-full-update-logic)
       (run-sync-logic)
       (run-calc-net-area-logic)
       (setq door-relationships (analyze-door-space-relationships))
       (modify-door-materials door-relationships)
       (setq continue-processing nil)
      )
      ((= user-choice "Quit")
       (setq continue-processing nil)
      )
      (t
       (princ "\nInvalid choice. Please try again.")
      )
    )
    
    (if continue-processing
      (progn
        (princ "\n\nContinue with another operation? [Yes/No]: ")
        (initget "Yes No")
        (setq user-choice (getkword))
        (if (= user-choice "No")
          (setq continue-processing nil)
        )
      )
    )
  )

  ;; Restore block scales
  (if (and original-scales restore-flag)
    (progn
      (initget "Yes No")
      (setq restore-choice (getkword "\nDo you want to restore original block scales? [Yes/No]: "))
      (if (or (not restore-choice) (= restore-choice "Yes"))
        (progn
          (princ "\nRestoring original block scales...")
          (foreach block-data original-scales
            (setq block-obj (car block-data))
            (setq insertion-point (vlax-safearray->list (vlax-variant-value (cadr block-data))))
            (setq scale-factor (/ (caddr block-data) (vla-get-xscalefactor block-obj)))
            (if (not (vlax-object-released-p block-obj))
              (vla-scaleentity block-obj (vlax-3d-point insertion-point) scale-factor)
            )
          )
          (princ " Done.")
        )
      )
    )
  )

  (vla-endundomark adoc)
  (princ "\n\n--- ENHANCED MASTER COMMAND COMPLETE ---")
  (princ)
)

(princ "\nLoaded enhanced master command: RUN-ALL-ENHANCED")
(princ "\nNew features:")
(princ "\n- Interactive menu system")
(princ "\n- Door-space relationship analysis")
(princ "\n- Door material modification")
(princ "\n- Individual operation selection")
(princ)