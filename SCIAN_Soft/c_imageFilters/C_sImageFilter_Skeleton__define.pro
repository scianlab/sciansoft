;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Skeleton
;
; PURPOSE:
;       - Skeleton filter (ACL80).
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Skeleton' )
;
; METHOHDS:
;   function  ->apply, image = image                                   ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_Skeleton::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Skeleton::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image
    ;return, thin(image, /neig)
    return, self->skeletonize(image)
end


function C_sImageFilter_Skeleton::skeletonize, binImage
    ; Erase the previously computed structures
    ptr_free, self.pThinnedImage

    ; Normalize input image to 1
    binImage /= max(binImage)

    ; Add background borders to the image to avoid dim problems with image-boundary ROI pixels
    thinnedImage = s_Expand_Mirror(binImage, 1)
    dimImage = size(thinnedImage, /dim) ; dim2[0] rows, dim2[1] columns, dim2[n] for more dims... ToDo

    thinnedImage[0, *] = 0
    thinnedImage[*, 0] = 0
    thinnedImage[dimImage[0]-1, *] = 0
    thinnedImage[*, dimImage[1]-1] = 0

    iter = 0

    print, "C_sBinaryImageThinner_ACL80goto: Begin of thinning repeat cycle"

    repeat begin
        iter += 1
        oldImage = thinnedImage
        roiPositionss = where(thinnedImage eq 1)

        roiPositions = [-1]
        for i = 0l, n_elements(roiPositionss)-1 do begin
          neighbors = wherePoints(roiPositionss[i], thinnedImage)
          sForCurrentPoint = countTransitions(roiPositionss[i], thinnedImage, neighbors[1:*])
          if (sForCurrentPoint eq 1) then roiPositions = [roiPositions, roiPositionss[i]]
        endfor
        points2delete = [-1]

        for i = 1l, n_elements(roiPositions)-1 do begin
            ; M1
            if ((thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0]]     eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 0) and $
                ;(thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 1) and $ ;;add
                (thinnedImage[roiPositions[i] + dimImage[0]]     eq 1) and $
                (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]
            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 0, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)

            ; N1
            if ((thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0]]     eq 0) and $
                (thinnedImage[roiPositions[i] + 1]               eq 0) and $
                (thinnedImage[roiPositions[i] - 1]               eq 1) and $
                (thinnedImage[roiPositions[i] + dimImage[0]]     eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]

            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 1, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)

            ; M2
            if ((thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 0) and $
                (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 0) and $
                (thinnedImage[roiPositions[i] + 1]               eq 0) and $
                ;(thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 1) and $ ;;add
                (thinnedImage[roiPositions[i] - 1]               eq 1) and $
                (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]

            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 2, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)
            ; N2
            if ((thinnedImage[roiPositions[i] + dimImage[0]]     eq 0) and $
                (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 0) and $
                (thinnedImage[roiPositions[i] + 1]               eq 0) and $
                (thinnedImage[roiPositions[i] - 1]               eq 1) and $
                (thinnedImage[roiPositions[i] - dimImage[0]]     eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]

            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 3, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)

            ; M3
            if ((thinnedImage[roiPositions[i] + dimImage[0]]     eq 0) and $
                (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 0) and $
                (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0]]     eq 1) and $
                ;(thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 1) and $ ;;add
                (thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]

            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 4, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)
            ; N3
            if ((thinnedImage[roiPositions[i] - 1]               eq 0) and $
                (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 0) and $
                (thinnedImage[roiPositions[i] + dimImage[0]]     eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0]]     eq 1) and $
                (thinnedImage[roiPositions[i] + 1]               eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]

            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 5, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)

            ; M4
            if ((thinnedImage[roiPositions[i] - 1]               eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 0) and $
                (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 0) and $
                ;(thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 1) and $ ;;add
                (thinnedImage[roiPositions[i] + 1]               eq 1) and $
                (thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]

            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 6, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)

            ; N4
            if ((thinnedImage[roiPositions[i] - 1]               eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0]]     eq 0) and $
                (thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 0) and $
                (thinnedImage[roiPositions[i] + 1]               eq 1) and $
                (thinnedImage[roiPositions[i] + dimImage[0]]     eq 1)) $
            then points2delete = [points2delete, roiPositions[i]]

            if (n_elements(points2delete) ne 1) then begin
                thinnedImage[points2delete[1:*]] = 0
                points2delete = [-1]
            endif
;window, 7, xsize = 600, ysize = 600
;tvscl, congrid (thinnedImage, 600, 600)

        endfor

        if (n_elements(points2delete) ne 1) then thinnedImage[points2delete[1:*]] = 0
        ;print, "thinned image after iteration ", iter
        ;print, thinnedImage

    ; Check if more changes can be made
    endrep until (min(thinnedImage eq oldImage) eq 1)

    print, "C_sBinaryImageThinner_ACL80goto: Thinning completed at iteration ", iter
    self.pThinnedImage = ptr_new(thinnedImage[1:dimImage[0]-2, 1:dimImage[1]-2])

    ; Collect the skeleton points from the thinned image
    ; TODO not implemented yet.
    ;self.pSkeletonPoints = ptr_new(where(*self.pThinnedImage eq 1))

    return, *self.pThinnedImage

end

function C_sImageFilter_Skeleton::init

    filterStruct = {Name: 'C_Skeleton',$       ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$       ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.


       ; Parameters of C_Roberts.
    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['Max_Value_Norm']   ;   Normalize Sobel Values -> [0 'no gradient', 1 'perfect']
    filterParamActive = [1]                        ;   ( active)
    filterParamMin = [1.]
    filterParamMax = [ 2.*255.]
    filterParamValues = [ 2.*255.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

function wherePoints, pointIndex, image
   dim2 = size(image, /dim)
   ; dim2[0] para filas, dim2[1] columnas, dim2[n] implica subir dim... ToDo
;   ; Some error handling... disable and let the error to be free?
;   if (dim2[0] eq 1) or (dim2[1] eq 1) then return, -1
;   if (pointIndex lt 0) or (pointIndex ge n_elements(image) then return, -1
;
;   wherePoints = [-1]
;
;   case 1 of
;      (pointIndex eq (n_elements(image)-1)): begin
;         wherePoints = [pointIndex - 1]
;      end
;      pointIndex eq 0: begin
;         wherePoints = [pointIndex + 1]
;      end
;      else: begin
;         wherePoints = [pointIndex - 1, pointIndex + 1]
;      end
;   endcase
;
;   rowP = ceil(pointIndex / dim2[0]) - 1
;   colP = (pointIndex mod dim2[0]) - 1
;
;   if (rowP gt 0) then begin
;      wherePoints = [wherePoints, pointIndex - dim2[0]]
;   endif
;
;   if (rowP lt dim2[0] - 1) then begin
;      wherePoints = [wherePoints, pointIndex + dim2[0]]
;   endif
   p2 = pointIndex - dim2[0]
   p3 = pointIndex - dim2[0] + 1
   p4 = pointIndex + 1
   p5 = pointIndex + dim2[0] + 1
   p6 = pointIndex + dim2[0]
   p7 = pointIndex + dim2[0] - 1
   p8 = pointIndex - 1
   p9 = pointIndex - dim2[0] - 1
   points = [p9, p2, p3, p8, pointIndex, p4, p7, p6, p5]
   points = [pointIndex, p2, p3, p4, p5, p6, p7, p8, p9]
   ; p9 p2 p3
   ; p8 p1 p4
   ; p7 p6 p5
   ; p1 given by pointIndex
   return, points
end

function countTransitions, pointIndex, image, points
   result = [image[points[0]] - image[points[1]],$
             image[points[1]] - image[points[2]],$
             image[points[2]] - image[points[3]],$
             image[points[3]] - image[points[4]],$
             image[points[4]] - image[points[5]],$
             image[points[5]] - image[points[6]],$
             image[points[6]] - image[points[7]],$
             image[points[7]] - image[points[0]]]
   ; print, result
   ; Choose positives (could be negatives) differences
   transitions = where(result eq 1)
   dimt = size (transitions, /dim)
   if (dimt eq 0) then if (transitions eq -1) then return, 0 else return, transitions else return, n_elements(transitions)
end

pro C_sImageFilter_Skeleton::cleanup

    ptr_free, self.pImage
    ptr_free, self.pThinnedImage
end

pro C_sImageFilter_Skeleton__define
  tmp = {C_sImageFilter_Skeleton,$
          pParamStruct: ptr_new(),$
          pImage: ptr_new(),$         ; Pointer to the input image
          pThinnedImage: ptr_new(),$ ; Pointer to the processed (with thinned ROIs) image
          inherits C_sImageFilter}
end