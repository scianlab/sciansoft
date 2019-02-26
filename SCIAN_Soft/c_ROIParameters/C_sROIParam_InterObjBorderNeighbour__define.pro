;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_InterObjBorderNeighbour
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_InterObjBorderNeighbour' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_InterObjBorderNeighbour::apply, stack_tlb = stack_tlb, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = C_sROIGroupObj

   whParam = [(where( *(*self.pParamStruct).pNames eq '2D Border Shared LT x [%]'))[0]]

   whParamActive = whParam * 0
   if (n_elements(position) gt 0) then begin
      if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[position[i]] = 1
   endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

       ;  Set xy-Distance Unit
   if ( not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] and $
        not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] ) then $
      xySizePerPixel = [1.,1.]
   (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[0]
   (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[1]

      ; return if not enough objects exist to calculate parameters:2 objects must exist  !
   nObjects = C_sROIGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
      *(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
      return
   endif

   if (nObjects eq 1) then begin
      *(*self.pParamStruct).pROINumberVect = (C_sROIGroupObj->getObjectNumberVector())
      *(*(*self.pValueStruct)[0]).pROIParamVect = 0.
      *(*self.pParamStruct).pROICoordinateMatrix = [-1,-1]
      return
   endif

      ; set Object Number Vector
   *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()
   *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

      ; get distance that defines a neighbor
   whereNeighborDistance = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Shared Border Distance'))[0]
   if (( *(*(*self.pValueStruct)[0]).pActive)[whereNeighborDistance]) then $
      neighborDistance = ((*(*(*self.pValueStruct)[0]).pValues)[whereNeighborDistance]) else neighborDistance = 1
   ((*(*(*self.pValueStruct)[0]).pValues)[whereNeighborDistance]) = neighborDistance

      ; check if surface models exist
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
   widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
      oBorderModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('2D Active Contours')
      if obj_valid(oBorderModel) then f2DAC = 1b else f2DAC = 0b
      if not(obj_valid(oBorderModel)) then oBorderModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('2D Border Model')
   widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy

   szMask = size(mask, /dim)
   if obj_valid(oBorderModel) then begin
      countI = 0
      for i = 0, oBorderModel->count()-1 do begin

            (oBorderModel->get(position = i))->getProperty, data = polygon, uValue = uValue
            print, uValue.name
            print, uValue.number
            if (uValue.name eq 'BaseObject') then begin
               if (countI eq 0) then begin
                  xPos = transpose(polygon[0,*])
                  yPos = transpose(polygon[1,*])
                  borderIndices = [0, n_elements(transpose(polygon[0,*]))]
               endif else begin
                  xPos = [xPos, transpose(polygon[0,*])]
                  yPos = [yPos, transpose(polygon[1,*])]
                  borderIndices = [borderIndices, borderIndices[countI] + n_elements(transpose(polygon[0,*])) ]
               endelse
               countI += 1
            endif
      endfor
   endif else begin

          ; renumber objects -> 1, 2, ...N
       newNumMask = C_sROIGroupObj->getGroupMaskInNumberOrder()

          ; define 2D-object borders and number them in respect to object size.
       borderMasks = s_rand4(newNumMask) * newNumMask

          ; get x- and y-borderIndices -> borderIndices[borderIndices[i]:borderIndices[i+1]-1] contain postition of ith border in borderMasks !
       hist = histogram(borderMasks, min = 1, max = nObjects,  /nan, reverse_indices = borderIndices)

          ; calculate x- and y-BorderPositions -> x(y)PixPos[borderIndices[i]:borderIndices[i+1]-1] contain postition of ith border in borderMasks !
       xPos = borderIndices[borderIndices[0]:*] mod szMask[0]
       yPos = floor(borderIndices[borderIndices[0]:*] * (1./szMask[0]))
       borderIndices = borderIndices[0:n_elements(hist)] - borderIndices[0]

    endelse

    xPos *= xySizePerPixel[0]
    yPos *= xySizePerPixel[1]

    xPosNeighbour = make_array(max(borderIndices), /float, value = -1.)
    yPosNeighbour = xPosNeighbour

       ; calculate BorderPixel Distance for each object i
    maxVal = xySizePerPixel[0] * szMask[0] + xySizePerPixel[1] * szMask[1]
    for i = 0, nObjects-1 do begin

       distCount = 0.
       for j = borderIndices[i], borderIndices[i+1]-1 do begin
          distVect = sqrt((xPos - xPos[j])^2 + (yPos - yPos[j])^2)
          distVect[borderIndices[i]:borderIndices[i+1]-1] = maxVal

          whereLT = where(distVect le neighborDistance, count)
          if (count ne 0) then begin
             whereMin = where(distVect eq min(distVect))
             xPosNeighbour[j] = xPos[whereMin[0]]
             yPosNeighbour[j] = yPos[whereMin[0]]
             distCount += 1
          endif

       endfor

       (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = distCount / (borderIndices[i+1]-borderIndices[i]) * 100.

    endfor

    whereN = where(xPosNeighbour ne -1, count)
    if (count ne 0) then begin
;       window, 10
;       plot, xPos, yPos, color = 150
;       for i = 0, count-1 do oPlot, [ xPos[whereN[i]], xPosNeighbour[whereN[i]] ] , [ yPos[whereN[i]], yPosNeighbour[whereN[i]] ]

       *(*self.pParamStruct).pROICoordinateMatrix = fltArr(4, count)
       (*(*self.pParamStruct).pROICoordinateMatrix)[0,*] = xPos[whereN] / xySizePerPixel[0]
       (*(*self.pParamStruct).pROICoordinateMatrix)[1,*] = xPosNeighbour[whereN] / xySizePerPixel[0]
       (*(*self.pParamStruct).pROICoordinateMatrix)[2,*] = yPos[whereN] / xySizePerPixel[1]
       (*(*self.pParamStruct).pROICoordinateMatrix)[3,*] = yPosNeighbour[whereN] / xySizePerPixel[1]
    endif

end


pro C_sROIParam_InterObjBorderNeighbour::cleanup
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


function C_sROIParam_InterObjBorderNeighbour::init

    ROIParamStruct = {name:'Inter-Object Border Neighbour',$   ;  ROI Name.
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$   ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$        ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$    ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$     ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$         ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new(),$     ; Pointer on ROI-Obj Number Vector
                    pROICoordinateMatrix:ptr_new([-1,-1], /no_copy)}    ; Pointer on ROI-Obj Distance Matrix.
                                                            ; each row (obj#, 1,2,3, ...,N) contains a list of distances (ordered by size)
    self.pValueStruct = ptr_new(ptrArr(1))
    ROIParamWidgetType = ['widget_slider']
    ROIParamNames = ['2D Border Shared LT x [%]']   ; vector contains the % of border points that fall into the neigbour distance
    ROIParamActive = [1]
    ROIParamMin = [0]
    ROIParamMax = [0]
    ROIParamValues = [0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:'2D Border Shared LT x [%]',$
                    type:'Inter ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$   ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$        ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$    ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$     ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$         ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueNames = ['Use Real x-Size (On) or Unit Size (off)', 'Use Real y-Size (On) or Unit Size (off)',$
                        'Shared Border Distance' ,$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueWidgetType = ['widget_slider', 'widget_slider',$
                        'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider']
    ROIValueActive = [1,1,$
                        1,$
                      0,0,$
                      0,0,$
                      0,0,$
                      0,0]
    ROIValueMin = [0.,0.,$
                      0.,$
                   0.,0.,$
                   0.,0.,$
                   0.,0.,$
                   0.,0.]
    ROIValueMax = [100., 100.,$
                   1000.,$
                   1.,1.,$
                   1.,1.,$
                   1.,1.,$
                   1.,1.]
    ROIValueValues =[1., 1.,$
                     20.,$
                     0.,1.,$
                     0.,1.,$
                     0.,1.,$
                     0.,1.]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_InterObjBorderNeighbour__define
    tmp = {C_sROIParam_InterObjBorderNeighbour, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
end