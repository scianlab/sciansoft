;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_InterObjCenterDist
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_InterObjCenterDist' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_InterObjCenterDist::apply, centerCoordinates = centerCoordinates, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = C_sROIGroupObj

    whParam = [(where( *(*self.pParamStruct).pNames eq 'Minimal C-Distance'))[0],$
               (where( *(*self.pParamStruct).pNames eq 'Highest C-Distance Difference [n - (n-1)]'))[0],$
               (where( *(*self.pParamStruct).pNames eq 'C-x Position'))[0],$
               (where( *(*self.pParamStruct).pNames eq 'C-y Position'))[0]]

       ;'Minimal C-Distance' is  always active
    (*(*self.pParamStruct).pActive)[whParam[0]] = 1

    whParamActive = whParam * 0
    if (n_elements(position) gt 0) then begin
       if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[position[i]] = 1
    endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

    nObjects = C_sROIGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       *(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
       *(*self.pParamStruct).pROIConnectMatrix = [-1,-1]
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
       return
    endif

       ; set Object Number Vector
    *(*self.pParamStruct).pROINumberVect = (C_sROIGroupObj->getObjectNumberVector())

        ;  Set xy-Distance Unit
    if (not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] and $
        not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] ) then $
       xySizePerPixel = [1.,1.]
    (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[0]
    (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[1]

        ;  Get Distances for selected Number of Neighbors
    whereNeighborNumber = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Number of Nearest Neighbors to Consider'))[0]
    if (( *(*(*self.pValueStruct)[0]).pActive)[whereNeighborNumber]) then $
       neighborNumber = ((*(*(*self.pValueStruct)[0]).pValues)[whereNeighborNumber]) < (nObjects-1) else neighborNumber = nObjects-1

    dummy = 1
    if ptr_valid((*self.pParamStruct).pROICoordinateMatrix) then begin
       dim_1 = size((*(*self.pParamStruct).pROICoordinateMatrix), /dim)
       dim_2 = size(centerCoordinates[1:2, *], /dim)
       if ( total(dim_1 eq dim_2) eq 2) then begin
         if ( total((*(*self.pParamStruct).pROICoordinateMatrix) eq centerCoordinates[1:2, *]) eq ((dim_1[0]>dim_2[0]) *  (dim_1[1]>dim_2[1])) ) then dummy = 0
       endif
    endif

    if dummy then begin
         ;  Set Object Number Vector
       *(*self.pParamStruct).pROINumberVect = fltArr(nObjects) + centerCoordinates[0,*]
         ;  Set Object Coordinate Vector ([x y], ObjNumber)
       *(*self.pParamStruct).pROICoordinateMatrix = intArr(2, nObjects) + centerCoordinates[1:2,*]
         ;  Calculate Center Distance for each Object
       distanceMatrix = fltArr(nObjects , nObjects) - 1. ; Define Distance Matrix:[i,j] defines distance between Object i and j (- 1. is default)
       for i = 0, nObjects-2 do distanceMatrix[i, i+1:*] = sqrt( ( xySizePerPixel[0] * (centerCoordinates[1, i+1:*] - centerCoordinates[1,i]))^2 + $
                                                                 ( xySizePerPixel[1] * (centerCoordinates[2, i+1:*] - centerCoordinates[2,i]))^2 )
       distanceMatrix = distanceMatrix > transpose(distanceMatrix)

       openW, 2, 'c:/rsi/AllInterObjCentreDist.dat'
       for i = 0, nObjects-1 do printF, 2, format = strCompress('(' + string(fix(nObjects)) + 'F)'), string(distanceMatrix[i,*])
       close, 2

         ;  Set Object Distance &  Coordinate  Matrix
       redDistMatrix = fltArr(nObjects, neighborNumber)
       redConnectMatrix = intArr(nObjects, neighborNumber)
       for i = 0, nObjects-1 do begin
         a = (distanceMatrix)[i,*]
         minSort = (sort(a))[1:neighborNumber]
         redDistMatrix[i, *] = a[minSort]
         redConnectMatrix[i, *] = minSort
       endfor
       *(*self.pParamStruct).pROIDistanceMatrix = redDistMatrix
       *(*self.pParamStruct).pROIConnectMatrix = redConnectMatrix
    endif

    openW, 2, 'c:/rsi/InterObjCentreDist.dat'
    for i = 0, nObjects-1 do printF, 2, format = strCompress('(' + string(fix(neighborNumber)) + 'F)'), reform(string((*(*self.pParamStruct).pROIDistanceMatrix)[i,*]))
    close, 2

    if whParamActive[0] then $
       *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = (*(*self.pParamStruct).pROIDistanceMatrix)[*,0]

    if whParamActive[1] then begin
       a = (*(*self.pParamStruct).pROIDistanceMatrix)[*, 0:neighborNumber-1]
       sizeA = size(a, /dim)
       if (size(a, /n_dim) ne 1) then begin
         if (sizeA[1] gt 2) then begin
          a[*, 0:sizeA[1] - 2] = a[*, 1:sizeA[1] - 1]  -  a[*, 0:sizeA[1] - 2]
          for i = 0, sizeA[0]-1 do a[i, 0] = (where(a[i, 0:sizeA[1] - 2] eq max(a[i, 0:sizeA[1] - 2] ) ))[0] + 1
          *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = a[*,0]
         endif else *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = a[*,0] * 0. + 1
       endif else *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = a
    endif

    if whParamActive[2] then $
       *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = centerCoordinates[1,*]

    if whParamActive[3] then $
       *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = centerCoordinates[2,*]
end


function C_sROIParam_InterObjCenterDist::init

    ROIParamStruct = {name:'Inter-Object Center Distance',$     ;  ROI Name.
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new(),$       ; Pointer on ROI-Obj Number Vector
                    pROIDistanceMatrix:ptr_new([-1,-1], /no_copy),$    ; Pointer on ROI-Obj Distance Matrix.
                                                            ; each row (obj#, 1,2,3, ...,N) contains a list of distances (ordered by size)
                    pROIConnectMatrix:ptr_new([-1,-1], /no_copy),$     ; Pointer on ROI-Obj Object-Connect Matrix.
                                                            ; each row (obj#, 1,2,3, ...,N) contains a list of objects which referr to pROIDistanceMatrix
                    pROICoordinateMatrix:ptr_new([-1,-1], /no_copy) }  ; Pointer on ROI-Obj Distance-Position-Matrix.

    self.pValueStruct = ptr_new(ptrArr(4))
    ROIParamWidgetType = make_array(4, /string, value = 'widget_slider')
    ROIParamNames = ['Minimal C-Distance',$                ; vector contains the distance of each object to its nearest neighbour
                     'Highest C-Distance Difference [n - (n-1)]',$     ; vector contains the number of the neighbour where the biggest distance difference results to the next neigbour
                     'C-x Position',$
                     'C-y Position']
    ROIParamActive = [1,1,1,1]
    ROIParamMin = [0,0,0,0]
    ROIParamMax = [0,0,0,0]
    ROIParamValues = [0,0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:'Minimal C-Distance',$
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueNames = ['Use Real x-Size (On) or Unit Size (off)', 'Use Real y-Size (On) or Unit Size (off)',$
                        'Number of Nearest Neighbors to Consider' ,$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueWidgetType = make_array(11, /string, value = 'widget_slider')
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

    ROIValueStruct = {name:'Highest C-Distance Difference [n - (n-1)]',$
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ ROIValueWidgetType[3:*]]
    ROIValueNames = [ ROIValueNames[3:*]]
    ROIValueActive = [ ROIValueActive[3:*]]
    ROIValueMin = [ ROIValueMin[3:*]]
    ROIValueMax = [ ROIValueMax[3:*]]
    ROIValueValues =[ ROIValueValues[3:*]]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'C-x Position',$
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIValueWidgetType
    ROIValueNames = ROIValueNames
    ROIValueActive = ROIValueActive
    ROIValueMin = ROIValueMin
    ROIValueMax = ROIValueMax
    ROIValueValues = ROIValueValues
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'C-y Position',$
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIValueWidgetType
    ROIValueNames = ROIValueNames
    ROIValueActive = ROIValueActive
    ROIValueMin = ROIValueMin
    ROIValueMax = ROIValueMax
    ROIValueValues = ROIValueValues
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_InterObjCenterDist__define
    tmp = {C_sROIParam_InterObjCenterDist, pParamStruct:ptr_new(),$
                                  pValueStruct:ptr_new(),$
                                  inherits C_sROIParam }
end