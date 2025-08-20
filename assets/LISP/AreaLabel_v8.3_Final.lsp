;;--------------------=={ Area Label v8.3 Final (Custom Layers & Unit Type) }==--------------------;;
;;                                                                                              ;;
;;  This script allows a user to label areas for architectural units by selecting              ;;
;;  polylines on specified layers for Suite, Balcony, and Void areas. It calculates            ;;
;;  the total area for each category and determines the unit type (Apartment, Duplex, etc.)    ;;
;;  based on the number of suite polylines selected. The final data is written to a            ;;
;;  user-specified file (.txt, .csv, .xls).                                                    ;;
;;                                                                                              ;;
;;----------------------------------------------------------------------------------------------;;
;;  Enhanced by AI for Abd alRahman - 2025                                                      ;;
;;  Version 8.3: Final version with restored, fully functional centroid calculation.            ;;
;;----------------------------------------------------------------------------------------------;;

(defun c:AFN nil (AreaLabelCustomLayers)) ;; AFN = Area to File (Named)

;;------------------------------------------------------------;;

(defun AreaLabelCustomLayers ( / *error* _startundo _endundo _centroid_revised _text _open
                               acdoc acspc cf cm el fo n of om p1 pf pt sf st th ts tx ucsxang ucszdir
                               ;; New variables for enhanced functionality
                               t_num t_type t_suite t_balc t_void t_total ap as de
                               layer_suite layer_balcony layer_void use_defaults_choice ent_sel layer_name
                               action current-unit-id-string
                               suite-area suite-obj suite-polys suite-poly-count unit-type
                               balcony-area balcony-obj
                               void-area void-obj
                               total-calc-area main-unit-text-obj main-label-pt
                               ss ent vla-obj obj-layer idx fl
                         )

  ;;------------------------------------------------------------;;
  ;;                         Adjustments                        ;;
  ;;------------------------------------------------------------;;

  (setq t_num "Unit ID"           ;; Title for Unit Name/Number column
        t_type "Unit Type"        ;; Title for the new Unit Type column
        t_suite "Suite Area"      ;; Title for Suite Area column
        t_balc "Balcony Area"     ;; Title for Balcony Area column
        t_void "Void Area"        ;; Title for Void Area column
        t_total "Total Calc. Area" ;; Title for Total Calculated Area column

        ;; Default Layers
        layer_suite "ld_unit_boundary"
        layer_balcony "ld_unit_balcony"
        layer_void "Void"

        ap ""                     ;; Area Prefix (optional, "" if none)
        as " sqm"                 ;; Area Suffix (e.g., " sqm", " sqft")
        cf 1.0                    ;; Area Conversion Factor (e.g. 1e-6 for mm2->m2, 1.0 for current units)
  )

  ;;------------------------------------------------------------;;
  ;;                  Original Helper Functions                 ;;
  ;;------------------------------------------------------------;;

  (defun *error* ( msg )
    (if cm (setvar 'CMDECHO cm))
    (if el (progn (entdel el) (setq el nil)))
    (if acdoc (_EndUndo acdoc))
    (if (and of (eq 'FILE (type of))) (close of))
    (if (not (wcmatch (strcase msg) "*BREAK,*CANCEL*,*EXIT*"))
        (princ (strcat "\n--> Error: " msg))
    )
    (princ)
  )

  (defun _StartUndo ( doc ) (_EndUndo doc)
    (vla-StartUndoMark doc)
  )

  (defun _EndUndo ( doc )
    (if (= 8 (logand 8 (getvar 'UNDOCTL)))
      (vla-EndUndoMark doc)
    )
  )

  ;; Restored the original, fully functional _centroid_revised function
  (defun _centroid_revised ( space vla_objs_list / reg cen temp_vla_obj minpt maxpt )
    (setq cen nil minpt nil maxpt nil) ; Initialize local variables
    (if (and vla_objs_list (listp vla_objs_list) (car vla_objs_list)
             (vlax-method-applicable-p (car vla_objs_list) 'AddRegion))
        (progn
          (setq reg (vl-catch-all-apply 'vla-AddRegion (list space vla_objs_list)))
          (if (not (vl-catch-all-error-p reg))
              (if (and reg (listp reg) (car reg)) 
                (progn
                  (setq reg (car reg)) 
                  (setq cen (vlax-get reg 'centroid))
                  (vla-delete reg)
                  (setq cen (trans cen 1 0)) 
                )
                (progn (princ "\nWarning: AddRegion did not return a valid region object.") (setq cen nil))
              )
              (progn (princ (strcat "\nWarning: Error creating region for centroid: " (vl-catch-all-error-message reg))) (setq cen nil))
          )
        )
        (if (and vla_objs_list (listp vla_objs_list) (car vla_objs_list))
            (princ "\nWarning: AddRegion method not applicable to selected objects for centroid.")
            (if (null vla_objs_list) (princ "\nWarning: Object list for centroid was empty."))
        )
    )

    (if (not cen)
        (cond
          ((and vla_objs_list (listp vla_objs_list) (= 1 (length vla_objs_list))) 
           (setq temp_vla_obj (car vla_objs_list))
           (if (vlax-method-applicable-p temp_vla_obj 'GetBoundingBox)
               (progn
                 (vl-catch-all-apply 'vlax-invoke (list temp_vla_obj 'GetBoundingBox 'minpt 'maxpt))
                 (if (and minpt maxpt (eq 'SAFEARRAY (type minpt)) (eq 'SAFEARRAY (type maxpt)))
                     (progn
                       (setq minpt (vlax-safearray->list minpt)
                             maxpt (vlax-safearray->list maxpt)
                       )
                       (setq cen (mapcar (function (lambda (a b) (/ (+ a b) 2.0))) minpt maxpt))
                     )
                     (progn (princ "\nWarning: GetBoundingBox failed to return valid points for centroid fallback.") (setq cen nil))
                 )
               )
               (progn (princ "\nWarning: GetBoundingBox not applicable for single object centroid fallback.") (setq cen nil))
           )
          )
          ((and vla_objs_list (listp vla_objs_list) (> (length vla_objs_list) 1)) 
             (princ "\nWarning: Centroid via region failed for multiple objects. Fallback (BoundingBox) is for single object. Cannot determine centroid for multiple objects this way.")
             (setq cen nil)
          )
          (t 
             (if (and vla_objs_list (not (listp vla_objs_list))) (princ "\nWarning: Input for centroid fallback was not a list."))
             (if (null vla_objs_list) (princ "\nWarning: Object list for centroid fallback was empty."))
             (setq cen nil)
          )
        )
    )
    (if (not cen) (progn (princ "\nCannot determine centroid. Using default origin (0,0,0).") (setq cen '(0 0 0))))
    cen
  )

  (defun _text ( space point string height rotation / textobj )
    (setq textobj (vla-addtext space string (vlax-3D-point point) height))
    (vla-put-alignment textobj acalignmentmiddlecenter)
    (vla-put-textalignmentpoint textobj (vlax-3D-point point))
    (vla-put-rotation textobj rotation)
    textobj
  )

  (defun _Open ( target / Shell result )
    (if (setq Shell (vl-catch-all-apply 'vla-getInterfaceObject (list (vlax-get-acad-object) "Shell.Application")))
      (if (not (vl-catch-all-error-p Shell))
        (progn
          (setq result
            (and
              (or (eq 'INT (type target)) (setq target (findfile target)))
              (not
                (vl-catch-all-error-p
                  (vl-catch-all-apply 'vlax-invoke (list Shell 'Open target))
                )
              )
            )
          )
          (vlax-release-object Shell)
        )
      )
    )
    result
  )

  ;;------------------------------------------------------------;;
  ;;                  New Helper Functions                      ;;
  ;;------------------------------------------------------------;;

  (defun get-next-unit-id-prompt ( / last-id-str prefix num-str next-num suggested-id user-input i )
    (setq suggested-id nil)
    (if (and *last-unit-id* (eq 'STR (type *last-unit-id*)) (> (strlen *last-unit-id*) 0))
        (progn
          (setq last-id-str *last-unit-id*)
          (setq num-str "")
          (setq i (strlen last-id-str))
          (while (and (> i 0) (wcmatch (substr last-id-str i 1) "#"))
            (setq num-str (strcat (substr last-id-str i 1) num-str))
            (setq i (1- i))
          )
          (setq prefix (substr last-id-str 1 i))
          (if (> (strlen num-str) 0) ; Fix: Removed faulty check that failed on leading zeros (e.g., "01")
              (progn
                (setq next-num (1+ (atoi num-str)))
                ;; Reconstruct suggested-id, padding with leading zeros if original had them
                (setq suggested-id (strcat prefix (rtos next-num 2 (strlen num-str))))
              )
          )
        )
    )
    (setq user-input nil)
    (while (not user-input)
      (if suggested-id
          (progn
            (princ (strcat "\nEnter Unit Name/Identifier <" suggested-id ">: "))
            (setq user-input (getstring t))
            (if (or (null user-input) (= "" user-input)) (setq user-input suggested-id))
          )
          (progn
            (princ "\nEnter Unit Name/Identifier: ")
            (setq user-input (getstring nil))
          )
      )
      (if (or (null user-input) (= "" user-input))
          (princ "\nUnit Name cannot be empty. Please try again.")
      )
    )
    (setq *last-unit-id* user-input)
    user-input
  )

  (defun get_next_action ( / cmd_opt )
    (princ "\n------------------------------------")
    (initget "Add Exit")
    (setq cmd_opt (getkword "\nEnter command [Add unit/Exit] <Add>: "))
    (if (or (null cmd_opt) (eq cmd_opt "Add")) "ADD" "EXIT")
  )

  (defun get-layer-from-selection ( prompt-string / ent_sel obj )
    (setq ent_sel nil)
    (while (not ent_sel)
      (setq ent_sel (entsel (strcat "\n" prompt-string)))
      (if (not ent_sel) (princ "\nNo object selected. Please try again."))
    )
    (setq obj (vlax-ename->vla-object (car ent_sel)))
    (vla-get-layer obj)
  )

  ;;------------------------------------------------------------;;
  ;;                       Main Function Body                   ;;
  ;;------------------------------------------------------------;;

  (setq acdoc (vla-get-activedocument (vlax-get-acad-object))
        acspc (vlax-get-property acdoc (if (= 1 (getvar 'CVPORT)) 'Paperspace 'Modelspace))
        ucszdir (trans '(0. 0. 1.) 1 0 t)
        ucsxang (angle '(0. 0. 0.) (trans (getvar 'UCSXDIR) 0 ucszdir))
  )
  (_StartUndo acdoc)
  (setq cm (getvar 'CMDECHO)) (setvar 'CMDECHO 0)

  (setq ts (getvar 'TEXTSIZE))

  (princ "\n--- Layer Configuration ---")
  (initget "Yes No")
  (setq use_defaults_choice (getkword (strcat "\nUse default layers? (Suite: " layer_suite ", Balcony: " layer_balcony ", Void: " layer_void ") [Yes/No] <Yes>: ")))
  (if (eq use_defaults_choice "No")
      (progn
        (princ "\nPlease specify the layers for calculation by selecting one object on each layer.")
        (setq layer_suite   (get-layer-from-selection "Select an object on the SUITE layer:"))
        (setq layer_balcony (get-layer-from-selection "Select an object on the BALCONY layer:"))
        (setq layer_void    (get-layer-from-selection "Select an object on the VOID layer:"))
        (princ (strcat "\nLayers set to -> Suite: " layer_suite ", Balcony: " layer_balcony ", Void: " layer_void))
      )
      (princ "\nUsing default layers.")
  )
  (princ "\n---------------------------")

  (setq fl (getfiled "Create Output File" (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) ".txt") "txt;csv;xls" 1))
  
  (if (and fl (setq of (open fl "w")))
      (progn
        (setq de (cdr (assoc (strcase (vl-filename-extension fl) t) '((".txt" . "\t") (".csv" . ",") (".xls" . "\t")))))
        
        (write-line (strcat t_num de t_type de t_suite de t_balc de t_void de t_total) of)

        (setq action "ADD")
        (while (eq action "ADD")
          (setq current-unit-id-string (get-next-unit-id-prompt))

          (if (or (null current-unit-id-string) (= "" current-unit-id-string))
              (princ "\nUnit Name cannot be empty. Unit not added to file.")
              (progn
                (princ (strcat "\n--- Adding Unit: " current-unit-id-string " for file ---"))
                (princ (strcat "\nSelect all polylines for this Unit (Suite, Balcony, Void)..."))
                
                (setq ss (ssget (list (cons 0 "LWPOLYLINE,POLYLINE") (cons 8 (strcat layer_suite "," layer_balcony "," layer_void))))) 

                (if (not ss)
                    (princ "\nNo objects selected. Unit not added to file.")
                    (progn
                      (setq suite-area 0.0 balcony-area 0.0 void-area 0.0
                            suite-polys nil suite-poly-count 0
                            idx 0)

                      (while (setq ent (ssname ss idx))
                        (setq vla-obj (vlax-ename->vla-object ent))
                        (if (and vla-obj (vlax-property-available-p vla-obj 'Area))
                            (if (vlax-curve-isclosed ent)
                                (progn
                                  (setq obj-layer (vla-get-layer vla-obj))
                                  (cond
                                    ((= (strcase obj-layer) (strcase layer_suite))
                                      (setq suite-area (+ suite-area (vla-get-area vla-obj)))
                                      (setq suite-polys (cons vla-obj suite-polys))
                                      (setq suite-poly-count (1+ suite-poly-count))
                                    )
                                    ((= (strcase obj-layer) (strcase layer_balcony))
                                      (setq balcony-area (+ balcony-area (vla-get-area vla-obj)))
                                    )
                                    ((= (strcase obj-layer) (strcase layer_void))
                                      (setq void-area (+ void-area (vla-get-area vla-obj)))
                                    )
                                  )
                                )
                                (princ (strcat "\nWarning: Object " (vl-princ-to-string ent) " is not closed. Ignored."))
                            )
                        )
                        (setq idx (1+ idx))
                      )
                      (setq suite-polys (reverse suite-polys))

                      (if (null suite-polys)
                          (princ (strcat "\nNo Suite polylines selected for Unit [" current-unit-id-string "] . Unit not added."))
                          (progn
                            (setq unit-type 
                              (cond
                                ((= suite-poly-count 1) "Apartment")
                                ((= suite-poly-count 2) "Duplex")
                                ((= suite-poly-count 3) "Triplex")
                                ((> suite-poly-count 3) (strcat (itoa suite-poly-count) "-plex"))
                                (t "N/A")
                              )
                            )
                            
                            (setq total-calc-area (- (+ suite-area balcony-area) void-area))
                            
                            (if (and suite-polys (listp suite-polys) (car suite-polys)) 
                                (setq main-label-pt (_centroid_revised acspc suite-polys))
                                (setq main-label-pt nil)
                            )
                            (if main-label-pt (_text acspc main-label-pt current-unit-id-string ts ucsxang))

                            (write-line
                              (strcat current-unit-id-string " "
                                      unit-type " "
                                      (rtos (* cf suite-area) 2 2) " "       
                                      (rtos (* cf balcony-area) 2 2) " "
                                      (rtos (* cf void-area) 2 2) " "
                                      (rtos (* cf total-calc-area) 2 2)
                              ) of
                            )
                            (princ (strcat "\nUnit [" current-unit-id-string "] written to file."))
                          )
                      )
                    )
                )
              )
          )
          ;; Get next action AFTER processing a unit.
          (setq action (get_next_action))
        )
        (close of)
        (princ (strcat "\nData successfully written to: " fl))
        (_Open fl) 
      )
      (princ "\nFile could not be opened for writing or operation was cancelled.")
  )

  (setvar 'CMDECHO cm)
  (_EndUndo acdoc)
  (princ)
)

(vl-propagate '*last-unit-id*)
(if (not (boundp '*last-unit-id*)) (setq *last-unit-id* nil))

;;------------------------------------------------------------;;
(vl-load-com)
(princ "\n:: AreaLabel v8.3 Final (Custom Layers) Loaded ::")
(princ "\n:: Enhanced for Abd alRahman 2025 ::")
(princ "\n:: Command: \"AFN\" to calculate your Area to a File ::")
(princ)
;;------------------------------------------------------------;;
;;                         End of File                        ;;
;;------------------------------------------------------------;;
