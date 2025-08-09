(defun c:COPYBLOCKTOALLCLOSED ( / selBlock blockName blockAttribs blkLayer blkScale polySel polyLayer ss i ent entObj pt newBlk)
  (vl-load-com)

  ;; Helper: Get block attributes
  (defun GetBlockAttributes (blk / result)
    (setq result '())
    (if blk
      (foreach att (vlax-invoke (vlax-ename->vla-object blk) 'GetAttributes)
        (setq result (cons (list (vla-get-TagString att) (vla-get-TextString att)) result))
      )
    )
    result
  )

  ;; NEW HELPER: Get the approximate centroid of a polyline
  (defun GetPolylineCentroid (pline_obj / coords num_verts i sum_x sum_y)
    (setq coords (vlax-get pline_obj 'Coordinates)
          num_verts (/ (length coords) 2)
          i 0
          sum_x 0.0
          sum_y 0.0
    )
    (repeat num_verts
      (setq sum_x (+ sum_x (nth i coords))
            sum_y (+ sum_y (nth (1+ i) coords))
            i (+ i 2)
      )
    )
    (list (/ sum_x num_verts) (/ sum_y num_verts) 0.0)
  )


  ;; Ask user to select the source block
  (prompt "\n📌 Select the source block reference (with attributes): ")
  (setq selBlock (car (entsel "\nSelect block: ")))
  (if (not selBlock)
    (progn (prompt "\n⚠️ No block selected. Exiting.") (exit))
  )

  ;; Validate block type
  (if (/= (cdr (assoc 0 (entget selBlock))) "INSERT")
    (progn (prompt "\n❌ That is not a block. Exiting.") (exit))
  )

  ;; Extract block details
  (setq blockName    (cdr (assoc 2 (entget selBlock))))
  (setq blockAttribs (GetBlockAttributes selBlock))
  (setq blkLayer     (cdr (assoc 8 (entget selBlock))))
  (setq blkScale     (cdr (assoc 41 (entget selBlock)))) ; assume uniform scale

  ;; Ask user to select a sample polyline
  (prompt "\n📌 Select one closed polyline to define the target layer: ")
  (setq polySel (car (entsel "\nSelect closed polyline: ")))
  (if (not polySel)
    (progn (prompt "\n⚠️ No polyline selected. Exiting.") (exit))
  )

  ;; Check it's a closed LWPOLYLINE
  (if (or (/= (cdr (assoc 0 (entget polySel))) "LWPOLYLINE")
          (/= 1 (logand (cdr (assoc 70 (entget polySel))) 1)))
    (progn (prompt "\n❌ That is not a closed LWPOLYLINE. Exiting.") (exit))
  )

  ;; Get polyline layer
  (setq polyLayer (cdr (assoc 8 (entget polySel))))

  ;; Select all closed polylines on the same layer
  (setq ss (ssget "_X" (list (cons 0 "LWPOLYLINE") (cons 8 polyLayer) (cons -4 "<AND") (cons 70 1) (cons -4 "AND>"))))
  (if (not ss)
    (progn (prompt "\n⚠️ No matching closed polylines found. Exiting.") (exit))
  )

  ;; Start placing blocks
  (prompt (strcat "\n🔁 Inserting block \"" blockName "\" into closed polylines..."))

  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i))
    (setq entObj (vlax-ename->vla-object ent))

    (if (= (vla-get-Closed entObj) :vlax-true)
      (progn
        ;; ***MODIFIED LINE***: Use the new centroid function instead of the midpoint of the perimeter
        (setq pt (GetPolylineCentroid entObj))

        ;; Insert block
        (setq newBlk (vla-InsertBlock
                       (vla-get-ModelSpace (vla-get-ActiveDocument (vlax-get-acad-object)))
                       (vlax-3d-point pt)
                       blockName
                       blkScale blkScale blkScale
                       0.0))

        ;; Match source layer
        (vla-put-Layer newBlk blkLayer)

        ;; Copy attributes
        (if newBlk
            (if blockAttribs
                (foreach att (vlax-invoke newBlk 'GetAttributes)
                  (foreach attrPair blockAttribs
                    (if (= (vla-get-TagString att) (car attrPair))
                      (vla-put-TextString att (cadr attrPair))
                    )
                  )
                )
            )
        )
      )
    )
    (setq i (1+ i))
  )

  (prompt "\n✅ DONE: Blocks inserted into the center of all closed polylines.")
  (princ)
)