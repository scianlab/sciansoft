;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_InterObjBorderDist
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_InterObjBorderDist' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_InterObjBorderDist::apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = C_sROIGroupObj

    whParam = [(where( *(*self.pParamStruct).pNames eq 'Minimal B-Distance'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Highest B-Distance Difference [n - (n-1)]'))[0] ]

    (*(*self.pParamStruct).pActive)[whParam[0]] = 1 ;'Minimal B-Distance' is  always active
    whParamActive = whParam * 0
    if (n_elements(position) gt 0) then begin
       if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[position[i]] = 1
    endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

        ;  Set xy-Distance Unit
    if (not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] and $
        not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] ) then xySizePerPixel = [1.,1.]

    (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[0]
    (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[1]

       ; return if not enough objects exist to calculate parameters:2 objects must exist  !
    nObjects = C_sROIGroupObj->count()
    if (nObjects le 1) then begin
       *(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
       *(*self.pParamStruct).pROIConnectMatrix = [-1,-1]
       *(*(*self.pValueStruct)[0]).pROIParamVect = -1
       *(*(*self.pValueStruct)[1]).pROIParamVect = -1
       return
    endif

       ; set Object Number Vector
    *(*self.pParamStruct).pROINumberVect = (C_sROIGroupObj->getObjectNumberVector())

       ; renumber objects -> 1, 2, ...N
    newNumMask = s_apop_flaeche(mask gt 0)
    szMask = size(newNumMask, /dim)

        ;  Get Distances for selected Number of Neighbors
    whereNeighborNumber = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Number of Nearest Neighbors to Consider'))[0]
    if (( *(*(*self.pValueStruct)[0]).pActive)[whereNeighborNumber]) then $
       neighborNumber = ((*(*(*self.pValueStruct)[0]).pValues)[whereNeighborNumber]) < (nObjects-1) else neighborNumber = nObjects - 1

       ; Define 2D-object borders and number them in respect to object size.
    borderMasks = s_rand4(newNumMask) * newNumMask

   window, 10, xsize = szMask[0], ysize = szMask[1]
   tvscl, borderMasks

       ; Get x- and y-borderIndices -> borderIndices[borderIndices[i]:borderIndices[i+1]-1] contain postition of ith border in borderMasks !
    hist = histogram(borderMasks, min = 1, max = nObjects,  /nan, reverse_indices = borderIndices)

       ; Calculate x- and y-BorderPositions -> x(y)PixPos[borderIndices[i]:borderIndices[i+1]-1] contain postition of ith border in borderMasks !
    xPixPos = borderIndices[borderIndices[0]:*] mod szMask[0]
    yPixPos = floor(borderIndices[borderIndices[0]:*] * (1./szMask[0]))
    borderIndices = borderIndices[0:n_elements(hist)] - borderIndices[0]

       ; Define Distance Matrix:[i,j] defines distance between Object i and j (- 1. is default)
    distanceMatrix = fltArr(nObjects, nObjects) - 1.
    posDPM = lonArr(2, nObjects, nObjects)

       ; totalNumBorderPixels is Total Number of BorderPixels
    totalNumBorderPixels = n_elements(xPixPos)

       ; Calculate BorderPixel Distance for each object i
    for i = 0, nObjects-2 do begin

         ; Define distancePixelMatrix -> [Number of BorderPixels of object i, Number of BorderPixels of all remaining objects i+1]
       distancePixelMatrix = fltArr(hist[i], borderIndices[n_elements(hist)] - borderIndices[i+1] )

         ; Calculate BorderPixel Distance for each pixel j of object i in respect to all other object pixels
       for j = borderIndices[i], borderIndices[i+1]-1 do $
         distancePixelMatrix[j-borderIndices[i], *] = sqrt( (xySizePerPixel[0] * ((xPixPos[borderIndices[i+1]:*] - xPixPos[j])))^2 + $
                                                            (xySizePerPixel[1] * ((yPixPos[borderIndices[i+1]:*] - yPixPos[j])))^2 )

       plots, ([ transpose(xPixPos[borderIndices[i+1]:*]*0+ xPixPos[j]) , transpose(xPixPos[borderIndices[i+1]:*]) ]) ,$
              ([ transpose(yPixPos[borderIndices[i+1]:*]*0+ yPixPos[j]) , transpose(yPixPos[borderIndices[i+1]:*]) ]), color = (j*10 mod 255), /device

         ; Find min BorderPixel Distance for each pixel of object i in respect to all other objects j
       for j = i+1, nObjects-1 do begin
          ; Get Pixel Distance Matrix [pixel i, pixel j]
         dummy = distancePixelMatrix[*, borderIndices[j]-borderIndices[i+1]:borderIndices[j+1]-1 -borderIndices[i+1]]
          ; Set min Object Distance Matrix [i, j]
         k = (where(dummy eq min(dummy)))[0]
         distanceMatrix[i,j] = dummy[k]
          ; Get min Pixel-Positions in Object i & j [posDPM[0,i,j] & posDPM[1,i,j]]
         posObj_i = k mod (borderIndices[i+1]-borderIndices[i]) + borderIndices[i]
         posObj_j = floor(k * (1./(borderIndices[i+1]-borderIndices[i]))) + borderIndices[j]
         posDPM[0,i,j] = xPixPos[posObj_i]; x-Position of Object i
         posDPM[0,j,i] = yPixPos[posObj_i]; y-Position of Object i
         posDPM[1,i,j] = xPixPos[posObj_j]; x-Position of Object j
         posDPM[1,j,i] = yPixPos[posObj_j]; y-Position of Object j

      plots, [posDPM[0,i,j], posDPM[1,i,j]], [posDPM[0,j,i], posDPM[1,j,i]], color = 255, /device
       endfor
     tvscl, borderMasks
    endfor

    distanceMatrix >= transpose(distanceMatrix)

    objNumberVector = intArr(nObjects)
    for i = 1, nObjects do objNumberVector[i-1] = mask[(where(newNumMask eq i))[0]]
    *(*self.pParamStruct).pROINumberVect = objNumberVector

       ;  Set Object Distance &  Coordinate  Matrix
    redDistMatrix = fltArr(nObjects, neighborNumber)
    redConnectMatrix = intArr(nObjects, neighborNumber)
    for i = 0, nObjects-1 do begin
       a = (distanceMatrix)[i,*]
       minSort = (sort(a))[1:neighborNumber]
       redDistMatrix[i,*] = a[minSort]
       redConnectMatrix[i,*] = minSort
    endfor
    *(*self.pParamStruct).pROIDistanceMatrix = redDistMatrix
    *(*self.pParamStruct).pROIConnectMatrix = redConnectMatrix
    *(*self.pParamStruct).pROICoordinateMatrix = posDPM

    openW, 2, 'c:/rsi/InterObjBorderDist.dat'
    for i = 0, nObjects-1 do printF, 2, format = strCompress('(' + string(fix(neighborNumber)) + 'F)'), reform(string((*(*self.pParamStruct).pROIDistanceMatrix)[i,*]))
    close, 2

    if whParamActive[0] then $
       *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = (*(*self.pParamStruct).pROIDistanceMatrix)[*,0]

    if whParamActive[1] then begin
       a = (*(*self.pParamStruct).pROIDistanceMatrix)[*, 0:neighborNumber-1]
       szA = size(a, /dim)
       if (size(a, /n_dim) ne 1) then begin
         if (szA[1] gt 2) then begin
          a[*, 0:szA[1] - 2] = a[*, 1:szA[1] - 1]  -  a[*, 0:szA[1] - 2]
          for i = 0, szA[0]-1 do a[i, 0] = (where(a[i, 0:szA[1] - 2] eq max(a[i, 0:szA[1] - 2] ) ))[0] + 1
          *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = a[*,0]
         endif else *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = a[*,0] * 0. + 1
       endif else *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = a
    endif
end


pro C_sROIParam_InterObjBorderDist::cleanup
    for i = 0, n_tags((*self.pParamStruct))-1 do begin
        case size((*self.pParamStruct).(i), /tname) of
            'POINTER':ptr_free, (*self.pParamStruct).(i)
            'OBJREF':obj_destroy, (*self.pParamStruct).(i)
            else:
        endcase
     endfor
    for j = 0, n_elements(*self.pValueStruct)-1 do begin
       for i = 0, n_tags((*(*self.pValueStruct)[j]))-1 do begin
         case size((*(*self.pValueStruct)[j]).(i), /tname) of
               'POINTER':ptr_free, (*(*self.pValueStruct)[j]).(i)
               'OBJREF':obj_destroy, (*(*self.pValueStruct)[j]).(i)
               else:
         endcase
        endfor
        ptr_free, (*self.pValueStruct)[j]
    endfor
    ptr_free, self.pValueStruct
    ptr_free, self.pParamStruct
end


function C_sROIParam_InterObjBorderDist::init

    ROIParamStruct = {name:'Inter-Object Border Distance',$   ;  ROI Name.
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$   ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$        ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$    ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$     ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$         ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new(),$     ; Pointer on ROI-Obj Number Vector
                    pROIDistanceMatrix:ptr_new([-1,-1], /no_copy),$    ; Pointer on ROI-Obj Distance Matrix.
                                                            ; each row (obj#, 1,2,3, ...,N) contains a list of distances (ordered by size)
                    pROIConnectMatrix:ptr_new([-1,-1], /no_copy),$     ; Pointer on ROI-Obj Object-Connect Matrix.
                                                            ; each row (obj#, 1,2,3, ...,N) contains a list of objects which referr to pROIDistanceMatrix
                    pROICoordinateMatrix:ptr_new([-1,-1], /no_copy) }  ; Pointer on ROI-Obj Distance-Position-Matrix.

    self.pValueStruct = ptr_new(ptrArr(2))
    ROIParamWidgetType = ['widget_slider', 'widget_slider']
    ROIParamNames = ['Minimal B-Distance',$                   ; vector contains the distance of each object to its nearest neighbour
                     'Highest B-Distance Difference [n - (n-1)]' ]   ; vector contains the number of the neighbour where the biggest distance difference results to the next neigbour
    ROIParamActive = [1, 1]
    ROIParamMin = [0, 0]
    ROIParamMax = [0, 0]
    ROIParamValues = [0, 0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:'Minimal B-Distance',$
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$   ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$        ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$    ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$     ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$         ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueNames = ['Use Real x-Size (On) or Unit Size (off)', 'Use Real y-Size (On) or Unit Size (off)',$
                        'Number of Nearest Neighbors to Consider' ,$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueWidgetType = ['widget_slider',  'widget_slider',$
                        'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider']
    ROIValueActive = [1, 1,$
                        1,$
                        0, 0,$
                        0, 0,$
                        0, 0,$
                        0, 0 ]
    ROIValueMin = [0., 0.,$
                        1.,$
                        0., 0.,$
                        0., 0.,$
                        0., 0.,$
                        0.,0. ]
    ROIValueMax = [100., 100.,$
                        1000.,$
                        1., 1.,$
                        1., 1.,$
                        1., 1.,$
                        1.,1. ]
    ROIValueValues =[1., 1.,$
                        10.,$
                        0., 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Highest B-Distance Difference [n - (n-1)]',$
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$   ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$        ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$    ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$     ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$         ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ ROIValueWidgetType[3:*]]
    ROIValueNames = [ ROIValueNames[3:*]]
    ROIValueActive = [ ROIValueActive[3:*]]
    ROIValueMin = [ ROIValueMin[3:*]]
    ROIValueMax = [ ROIValueMax[3:*]]
    ROIValueValues =[ ROIValueValues[3:*]]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_InterObjBorderDist__define
   tmp = {C_sROIParam_InterObjBorderDist, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
end