;_____________________________IOISIOI____________________
; NAME:
;      C_sROIGroupObj
;
; PURPOSE:
;       - Encapsulation of IDLgrROIGroup object
;       - Creates the following graphic objects:
;      ->  oPhaseModel = obj_new('IDLgrModel', uValue = 'oPhaseModel')                       ;IDLgrModel-Container of all Sub-Models below
;                    add->  oObjectModel = obj_new('IDLgrModel', uValue = 'oObjectModel')       ; contains 1st and 2nd segmented Objects (1/0)
;                       ->    oImageModel = obj_new('IDLgrModel', uValue = 'oImageModel')        ;   contains Image-Object
;                       ->    oSubPhaseModel = obj_new('IDLgrModel', uValue = 'oSubPhaseModel')   ;   contains Sub-Phase
;                       ->    oObjectBorderModel = obj_new('IDLgrModel', uValue = 'oObjectBorderModel')   ; contains Object Border as Polygons
;                       ->    oBorderDistanceModel = obj_new('IDLgrModel', uValue = 'oBorderDistanceModel')  ; contains Object Border Distances as Polygons
;                       ->    oCenterDistanceModel = obj_new('IDLgrModel', uValue = 'oCenterDistanceModel')  ; contains Object Centrer Distances as Polygons
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIGroupObj' )
;
; METHOHDS:
;     function C_sROIGroupObj::init, Name = Name, Number = Number, ImagePixSize = ImagePixSize, WhereBorderPoints = WhereBorderPoints
;                          Name: Name of Object
;                          Number: Number of Object
;                          ImagePixSize: 2D- or 3D-embedding Object-Space
;_____________________________IOISIOI____________________

;_BEGIN_INTERNAL_FUNCTIONS_____________________________________________________________________
pro C_sROIGroupObj::setRealROIGroupShowBoxSize
   *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [real]'))[0]] = $
      *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]] * 1. $
      *(*(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ShowBox in Original Picture [x1]'))[0]]  $
      - *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ShowBox in Original Picture [x0]'))[0]] + 1. ) $
      / *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]]

   *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [real]'))[0]] = $
      *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]] * 1. $
      *(*(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ShowBox in Original Picture [y1]'))[0]]  $
      - *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'ShowBox in Original Picture [y0]'))[0]] +1. ) $
      / *(*self.pParamStruct).pValues[(where( *(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
end


pro C_sROIGroupObj::setIDLgrROIGroupXYZCoord
    xyzFramePix = 1.* [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] ]

    xyzFract = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]] / xyzFramePix[0],$
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]] / xyzFramePix[1],$
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Interval [real]'))[0]] ]

    if (xyzFract[2] le 0) then xyzFract = [xyzFract / (min(xyzFract) * max(xyzFramePix))] $
                          else xyzFract = xyzFract / (min(xyzFract) * max(xyzFramePix))

    if (xyzFramePix[0] gt xyzFramePix[1]) then xyFact = [1., xyzFramePix[1] / xyzFramePix[0]] else xyFact = [xyzFramePix[0] / xyzFramePix[1], 1.] 

    self->setProperty, xCoord_conv = [-0.5*xyFact[0], xyzFract[0]], yCoord_conv = [-0.5*xyFact[1], xyzFract[1]], zCoord_conv = [0., xyzFract[2]]
end

function C_sROIGroupObj::getxyzDim
   return, [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]]]
end

pro C_sROIGroupObj::setActiveGroupParameterNameList
    oParams = self.oParamContainer->get(count = count, /all)
    if (count gt 0) then begin
       dummy = strArr(count)
       for i = 0, count-1 do dummy[i] = (*(oParams[i]->getpParamStruct())).Name
       if (ptr_valid(self.pActiveGroupParameterNameList)) then ptr_free, self.pActiveGroupParameterNameList
       self.pActiveGroupParameterNameList = ptr_new(dummy, /no_copy)
    endif
end


pro C_sROIGroupObj::cleanup
    self->IDLgrROIGroup::cleanup
    if ptr_valid(self.pActiveGroupParameterNameList) then ptr_free, self.pActiveGroupParameterNameList
    if ptr_valid(self.pParamStruct) then begin
       ptr_free, (*self.pParamStruct).pNames
       for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
       ptr_free, self.pParamStruct
    endif
    if obj_valid(self.oParamContainer) then begin
       oROIParams = self.oParamContainer->get(/all)
       self.oParamContainer->remove, /all
       if obj_valid(oROIParams[0]) then for i = 0, n_elements(oROIParams)-1 do obj_destroy, oROIParams[i]
       obj_destroy, self.oParamContainer
    endif
    if obj_valid(self) then begin
       objs = self->get(/all)
       if obj_valid(Objs[0]) then begin
          for i = 0, n_elements(Objs)-1 do obj_destroy, Objs[i]
          self->remove, /all
       endif
       obj_destroy, self
    endif
end
;_END_INTERNAL_FUNCTIONS_____________________________________________________________________


;_BEGIN_SET_FUNCTIONS_____________________________________________________________________
pro C_sROIGroupObj::set, pParamStruct = pParamStruct
    prt_free, self.pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end


pro C_sROIGroupObj::setParamAsStruct, paramStruct
    parameterNameList = self->getParameterNameList()
    whParam = (where(parameterNameList[0] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.a
    whParam = (where(parameterNameList[1] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.b
    whParam = (where(parameterNameList[2] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.c
    whParam = (where(parameterNameList[3] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.d
    whParam = (where(parameterNameList[4] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.e
    whParam = (where(parameterNameList[5] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.f
    whParam = (where(parameterNameList[6] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.g
    whParam = (where(parameterNameList[7] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.h
    whParam = (where(parameterNameList[8] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.i
    whParam = (where(parameterNameList[9] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.j
    whParam = (where(parameterNameList[10] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.k
    whParam = (where(parameterNameList[11] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.l
    whParam = (where(parameterNameList[12] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.m
    whParam = (where(parameterNameList[13] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.n
    whParam = (where(parameterNameList[14] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.o
    whParam = (where(parameterNameList[15] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.p
    whParam = (where(parameterNameList[16] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.q
    whParam = (where(parameterNameList[17] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.r
    whParam = (where(parameterNameList[18] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.s
    whParam = (where(parameterNameList[19] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.t
    whParam = (where(parameterNameList[20] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.u
    whParam = (where(parameterNameList[21] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.v
    whParam = (where(parameterNameList[22] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.w
    whParam = (where(parameterNameList[23] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.x
    whParam = (where(parameterNameList[24] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.y
    whParam = (where(parameterNameList[25] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.z
    whParam = (where(parameterNameList[26] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aa
    whParam = (where(parameterNameList[27] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ab
    whParam = (where(parameterNameList[28] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ac
    whParam = (where(parameterNameList[29] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ad
    whParam = (where(parameterNameList[30] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ae
    whParam = (where(parameterNameList[31] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.af
    whParam = (where(parameterNameList[32] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ag
    whParam = (where(parameterNameList[33] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ah
    whParam = (where(parameterNameList[34] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ai
    whParam = (where(parameterNameList[35] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.aj
    whParam = (where(parameterNameList[36] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.ak
    whParam = (where(parameterNameList[37] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.al
    whParam = (where(parameterNameList[38] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.am

end


;_BEGIN_GET_FUNCTIONS_____________________________________________________________________
function C_sROIGroupObj::getpParamStruct
   return, self.pParamStruct
end


function C_sROIGroupObj::getParamAsStruct
    parameterNameList = self->getParameterNameList()
    pArrValues = ptrArr(n_elements(parameterNameList))
    for i = 0, n_elements(parameterNameList)-1 do begin
       whParam = (where(parameterNameList[i] eq *(*self.pParamStruct).pNames))[0]
       if (whParam eq -1) then begin
         pArrValues[i] = ptr_new('NOT CONTAINED')
         print, *pArrValues[i]
       endif else pArrValues[i] = ptr_new(*(*self.pParamStruct).pValues[whParam])
    endfor

    paramStruct = {a: *pArrValues[0],$
          b: *pArrValues[1],$
          c: *pArrValues[2],$
          d: *pArrValues[3],$
          e: *pArrValues[4],$
          f: *pArrValues[5],$
          g: *pArrValues[6],$
          h: *pArrValues[7],$
          i: *pArrValues[8],$
          j: *pArrValues[9],$
          k: *pArrValues[10],$
          l: *pArrValues[11],$
          m: *pArrValues[12],$
          n: *pArrValues[13],$
          o: *pArrValues[14],$
          p: *pArrValues[15],$
          q: *pArrValues[16],$
          r: *pArrValues[17],$
          s: *pArrValues[18],$
          t: *pArrValues[19],$
          u: *pArrValues[20],$
          v: *pArrValues[21],$
          w: *pArrValues[22],$
          x: *pArrValues[23],$
          y: *pArrValues[24],$
          z: *pArrValues[25],$
          aa: *pArrValues[26],$
          ab: *pArrValues[27],$
          ac: *pArrValues[28],$
          ad: *pArrValues[29],$
          ae: *pArrValues[30],$
          af: *pArrValues[31],$
          ag: *pArrValues[32],$
          ah: *pArrValues[33],$
          ai: *pArrValues[34],$
          aj: *pArrValues[35],$
          ak: *pArrValues[36],$
          al: *pArrValues[37],$
          am: *pArrValues[38] }

    for i = 0, n_elements(parameterNameList)-1 do ptr_free, pArrValues[i]
    return, paramStruct
end


function C_sROIGroupObj::getGroupMask, mask = mask
    nROIGroupObjs = self->count()
    if (n_elements(mask) eq 0) then begin
       if ( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]] eq -1 ) then $
       mask = intArr( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]] ) else $
       mask = intArr( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] )
    endif

    for i = 0, nROIGroupObjs-1 do mask[*((self->get(position = i))->getpWherePoints())] = (self->get(position = i))->getNumber()
    return, mask
end


function C_sROIGroupObj::getObjectNumberVector
    objNumberVector = -1
    for i = 0, (self->count())-1 do objNumberVector = [objNumberVector, (self->get(position = i))->getNumber()]
    if (n_elements(objNumberVector) eq 1) then return, objNumberVector else return, objNumberVector[1:*]
end


pro C_sROIGroupObj::rescaleGroupObjIntensities, minMaxScales = minMaxScales
    nROIGroupObjs = self->count()
    if (n_elements(minMaxScales) eq 0) then minMaxScales = [-1, -1]
    minMaxScalesOld = minMaxScales
    if ((minMaxScales[0] eq -1) or (minMaxScales[1] eq -1)) then begin
       for i = 0, 0<(nROIGroupObjs-1) do begin
         minMaxScales[0] = min(*( (self->get(position = i))->getpPointValues()))
         minMaxScales[1] = max(*( (self->get(position = i))->getpPointValues()))
       endfor
       for i = 1, nROIGroupObjs-1 do begin
         minMaxScales[0] = minMaxScales[0] < min(*( (self->get(position = i))->getpPointValues()))
         minMaxScales[1] = minMaxScales[1] > max(*( (self->get(position = i))->getpPointValues()))
       endfor
    endif
    if (minMaxScalesOld[0] gt -1) then minMaxScales[0] = minMaxScalesOld[0]
    if (minMaxScalesOld[1] gt -1) then minMaxScales[1] = minMaxScalesOld[1]
    for i = 0, nROIGroupObjs-1 do (self->get(position = i))->rescalepPointValues, minMaxScales = minMaxScales
end


function C_sROIGroupObj::getGroupMaskIntensity, maskIntensity = maskIntensity
   nROIGroupObjs = self->count()
   if (n_elements(maskIntensity) eq 0) then maskIntensity = $
      intArr( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
              *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] )
   for i = 0, nROIGroupObjs-1 do maskIntensity[ *( (self->get(position = i))->getpWherePoints()) ] = *( (self->get(position = i))->getpPointValues())
   return, maskIntensity
end


function C_sROIGroupObj::getObjectInArray, objNumber = objNumber
   xyPoints = (self->get(position = objNumber))->getxyzPoints()
   xyPoints[0,*] -= (min(xyPoints[0,*]) + 1)
   xyPoints[1,*] -= (min(xyPoints[1,*]) + 1)
   mask = bytArr(max(xyPoints[0,*])+2, max(xyPoints[1,*])+2)
   intValues = (1. * mask) - 1.
   mask[xyPoints[0,*], xyPoints[1,*]] = 1
   intValues[where(mask)] = *( (self->get(position = objNumber))->getpPointValues())
   return, {mask: mask, intValues: intValues}
end


function C_sROIGroupObj::getGroupCenterXYZ
   nROIGroupObjs = self->count()
   ctrCoord = make_array(3, nROIGroupObjs > 1, /float, value = -1.)
   for i = 0, nROIGroupObjs-1 do ctrCoord[*,i] = [(self->get(position = i))->getNumber(),(self->get(position = i))->getCenterXYZ()]
   return, ctrCoord
end;_END_GET_FUNCTIONS_____________________________________________________________________


;_BEGIN_ADD_FUNCTIONS_____________________________________________________________________
pro C_sROIGroupObj::addROIObjectsFromSegImage, maskImage = maskImage,$
                                               intImage = intImage,$
                                               objNumberVector = objNumberVector,$   ; Make Object Numbers From Mask Image Number,$
                                               eightNeighbor = eightNeighbor        ; eightNeighbor: If set, 8-neighbor is definition in label_region. Default: 4-neighbor
       ; upDate Object Characteristics
    xyzSizePerPixel = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [real]'))[0]] * 1. / $
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [real]'))[0]] * 1. / $
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]],$
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Interval [real]'))[0]] ]
    zSizeDimension = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]
    dimMaskImage = size(maskImage, /dim)

       ; upDate preset Graphic Characteristics
    ROIStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Style'))[0]] > 0 < 2
    ROIThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Thick'))[0]] > 1. < 10.
    ROILineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI LineStyle'))[0]] > 0 < 6
    ROIHide = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Hide'))[0]] > 0 < 1
    ROIColor = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Color_r'))[0]],$
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Color_g'))[0]],$
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Color_b'))[0]]  ] > [0,0,0] < [255,255,255]

    if (n_elements(objNumberVector) gt 0) then begin
       for i = 0, n_elements(objNumberVector)-1 do begin
          whObj = where(maskImage eq objNumberVector[i])
          if (whObj[0] ne -1) then begin
            dummy = obj_new('C_sROIObject', segPhase = '1st Phase',$
                                            number = objNumberVector[i],$
                                            framePixSize = [dimMaskImage, zSizeDimension],$
                                            xyzSizePerPixel = xyzSizePerPixel,$
                                            ROIColor = ROIColor,$
                                            ROIStyle = ROIStyle,$
                                            ROIThick = ROIThick,$
                                            ROILineStyle = ROILineStyle,$
                                            ROIHide = ROIHide,$
                                            wherePoints = whObj,$
                                            pointValues = intImage[whObj] )
   
            dummy->setProperty, color = [255, 255, 255]
            self->add, dummy
          endif
       endfor
       return
    endif

       ; Label_Regions first segmentation plane | Border Pixels are not set to 0 (see label_region)
    dummy = bytArr(dimMaskImage[0]+2, dimMaskImage[1]+2)
    dummy[1:dimMaskImage[0], 1:dimMaskImage[1]] = maskImage ne 0
    if keyWord_set(eightNeighbor) then labelImage = (label_region(dummy ne 0, /all, /uLong))[1:dimMaskImage[0], 1:dimMaskImage[1]] $
       else labelImage = (label_region(dummy ne 0, /uLong))[1:dimMaskImage[0], 1:dimMaskImage[1]]
    for i = 0, max(labelImage)-1 do begin
       number = i+1
       pos = i
       whObj = where(labelImage eq number)
       self->add, obj_new('C_sROIObject', segPhase = '1st Phase',$
                                          number = number,$
                                          framePixSize = [dimMaskImage, zSizeDimension],$
                                          xyzSizePerPixel = xyzSizePerPixel,$
                                          ROIColor = ROIColor,$
                                          ROIStyle = ROIStyle,$
                                          ROIThick = ROIThick,$
                                          ROILineStyle = ROILineStyle,$
                                          ROIHide = ROIHide,$
                                          wherePoints = whObj,$
                                          pointValues = intImage[whObj]), position = pos
    endfor
end


; AUTHOR:
;        Jorge Jara W. (2012)
; NOTES:
;        Method based on C_sROI3DGroupObj::init3DModelsFromPaintedMasks, although some differences exist.
;        There is no clusPos attribute in C_sROIGroupObj
pro C_sROIGroupObj::init2DModelsFromPaintedMasks, stack_tlb           = stack_tlb,$
                                                  fValueType          = fValueType,$
                                                  maskPathCacheStruct = maskPathCacheStruct,$
                                                  clusPos             = clusPos
  slash = path_sep()
  xyzFrameRealSize = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]],$
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]],$
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Interval [real]'))[0]] * *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]]
  xyzDim = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]]
  xyzSizePerPixel = xyzFrameRealSize / xyzDim
  ; There is no clusPos attribute in C_sROIGroupObj
  ;clusPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Cluster Position'))[0]]
  tPos    = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]]
  chPos   = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]]
  zPos    = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Position'))[0]]

    ; get imageStackInfoObject
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy

  imageStackInfoObject->get, pParamStruct = pParamStruct
  paintedMaskPath = [*(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Mask Path'))[0]], 'X']

    ; get mask; use the maskPathCacheStruct variable if available to avoid repeated pickfile dialog
  if (keyword_set(maskPathCacheStruct) eq 0) then begin
    file = dialog_pickfile(/read, path = paintedMaskPath[0], get_path = path, filter = '*.tif', title = 'Select the first mask image file for reading')
    mPath = testAndAssemblePath(tPos = tPos, chPos = chPos, clusPos = clusPos, samplePath = path, pathStruct = maskPathCacheStruct)
  endif else mPath = testAndAssemblePath(tPos = tPos, chPos = chPos, clusPos = clusPos, pathStruct = maskPathCacheStruct)

  cellNumVect = [-1]
    ; look for the folders with masks in the directory that match the pattern
  pattern = '*fill_c*'
  cellMaskFoldersList = file_search(mPath, pattern, /test_directory, count = nRois)

  if (nRois lt 1) then begin
    print, 'no masks found for t =', tPos, ' ch =', chPos, 'in directory ', mPath
    return
  endif

    ; get the number of the cells whose masks are located as subfolders in the current folder
    ; (not always the full cell set is stored, nor are they found in correlatives)
  for i = 0L, n_elements(cellMaskFoldersList)-1 do begin
    cPos = strPos(cellMaskFoldersList[i], '_c', /reverse_s)
    cNum = s_getRightNumberFromString(strMid(cellMaskFoldersList[i], cPos))
    cellNumVect = [cellNumVect, cNum]
  endfor

  mask2D  = make_array(xyzDim[0:1], /byte)
  image2D = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)

    ; for loop, first element is a -1 index so it is ignored
  for i = 1L, nRois do begin
    whereRoi = 0
    mask2D   = 0
    mask2D   = make_array(xyzDim[0], xyzDim[1], /byte)
    zSlicesList  = file_search(cellMaskFoldersList[i-1], '*fill_c*_z*.tif', count = zCount)
    preNumberLen = strLen(strMid(zSlicesList[0], 0, strPos(zSlicesList[0], '.tif')))
    zIndices     = make_array(n_elements(zSlicesList), /uint)
    ; TODO JJ sorting file names before loading the files in main memory... perform speed tests without sorting
    for j = 0, zCount-1 do zIndices[j] = s_getRightNumberFromString(strMid(zSlicesList[j], 0, strLen(strMid(zSlicesList[j], 0, strPos(zSlicesList[j], '.tif')))))
    sortedZindices = sort(zIndices)
    for j = 0, xyzDim[2]-1 do if (j eq zPos) then begin
      print, 'Reading mask file ', zSlicesList[sortedZindices[j]]
      ; Always data values returned as non zero.
      ; Reminder: in getRightMaskFromTiff fValueType by default set to 1... i.e. data with non-zero values
      mask2d = getRightMaskFromTiff(name = zSlicesList[sortedZindices[j]], fValueType = fValueType)
      break
    endif
    whereRoi = -1
    whereMax = -1
    whereRoi = where(mask2D ne 0, countWhRoi)
    if (countWhRoi gt 0) then $
    self->add, obj_new('C_sROIObject', number = cellNumVect[i],$
                                       ;tPos = tPos,$
                                       ;chPos = chPos,$
                                       ;zPos = zPos,$
                                       framePixSize = xyzDim,$
                                       xyzSizePerPixel = xyzSizePerPixel,$
                                       pointValues = mask2D[whereRoi],$
                                       wherePoints = whereRoi);,$
                                       ;zSliceInitial = zPos)
  endfor
end


; function C_sROIGroupObj::getRightMaskFromTiff, name = name, fValueType = fValueType
; For some unknown reason C_sROI3DGroupObj doesn't inherit C_sROIGroupObj class. 
; AUThOR: FASL 2012
function getRightMaskFromTiff, name = name, fValueType = fValueType

  fValueType = keyword_set(fValueType) ? fValueType : 1 ; Default: 1 .. data with no zero values
; fValueType = 0 ... data is represented by low values  
; fValueType = 1 ... data is represented by high values
; fValueType = 2 ... data is represented by low rate portion of volume (neurons)
; fValueType = 3 ... data is represented by high rate portion of volume

  if(fValueType eq 1) then return, read_tiff(name)

  dummy = read_tiff(name)
  ;whereLevelLow = where(dummy lt 125, /L64, COMPLEMENT = whereLevelHigh)
  ; the commented code maybe have sense ... only if... data are not ninary  
  whereLevelLow = where(dummy eq 0, /L64, COMPLEMENT = whereLevelHigh)
  dummy[*] = 0  
  case fValueType of
    2: if (n_elements(whereLevelLow) gt n_elements(whereLevelHigh)) then whereLevelLow = whereLevelHigh
    3: if (n_elements(whereLevelLow) lt n_elements(whereLevelHigh)) then whereLevelLow = whereLevelHigh
    else: begin
    end
  endcase
  if (whereLevelLow[0] ne -1) then dummy[whereLevelLow] = 255
  return, dummy
end


pro C_sROIGroupObj::manageCurrentPhaseModel, includeModelList = includeModelList, stack_tlb = stack_tlb

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
   widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
      fMovieKeepObjectPosition = stateObj.fMovieKeepObjectPosition
      fUpDateROI2DGroup = stateObj.fUpDateROI2DGroup
      fUpDateROI2DGroupProperties = stateObj.fUpDateROI2DGroupProperties
      poCurrROIGraphicModel = stateObj.poCurrROIGraphicModel
   widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy

    if not(obj_valid(*poCurrROIGraphicModel)) then *poCurrROIGraphicModel = obj_new('IDLgrModel', uValue = 'oPhaseModel')

       ; check state of CurrentROIPhaseModel
    for i = ((*poCurrROIGraphicModel)->count()-1), 0, -1 do begin
       oObjectModel = (*poCurrROIGraphicModel)->get(position = i)
       if ( obj_valid(oObjectModel) and (obj_class(oObjectModel) ne 'IDLEXMODELMANIP') ) then begin
         oObjectModel->getProperty, uValue = modelName
         whModel = (where(modelName eq includeModelList ))[0]
          ;cleanup, remove and desytroy oObjectModel
         if (whModel eq -1) then begin
            case modelName of
              '2D Object Model': begin
                   oObjectModel->remove, /all
                   if not(fMovieKeepObjectPosition) then begin
                      (*poCurrROIGraphicModel)->remove, oObjectModel
                      obj_destroy, oObjectModel
                   endif
                 endcase
              else: begin
                   if (strPos(modelName, '3D') eq -1) then begin
                    oSubModels = oObjectModel->get(/all)
                    oObjectModel->remove, /all
                    if obj_valid(oSubModels[0]) then for j = 0, n_elements(oSubModels)-1 do obj_destroy, oSubModels[j]
                    if not(fMovieKeepObjectPosition) then begin
                        (*poCurrROIGraphicModel)->remove, oObjectModel
                        obj_destroy, oObjectModel
                    endif
                   endif
              endcase
          endcase
         endif
       endif
    endfor

    for k = 0, n_elements(includeModelList)-1 do begin
       case includeModelList[k] of
         '2D Object Model': begin
              whParam = (where(*(*self.pParamStruct).pNames eq 'ROI Color_r'))[0]
              if (whParam ne -1) then begin
                 ROIColor = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Color_r'))[0]],$
                              *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Color_g'))[0]],$
                              *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Color_b'))[0]]  ] > [0,0,0] < [255,255,255]
              endif
              whParam = (where(*(*self.pParamStruct).pNames eq 'ROI Style'))[0]
              if (whParam ne -1) then begin
                 ROIStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Style'))[0]] > 0 < 2
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Style'))[0]] = ROIStyle
              endif else ROIStyle = 0
              whParam = (where(*(*self.pParamStruct).pNames eq 'ROI Thick'))[0]
              if (whParam ne -1) then begin
                 ROIThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Thick'))[0]] > 1. < 10.
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Thick'))[0]] = ROIThick
              endif else ROIThick = 1.
              whParam = (where(*(*self.pParamStruct).pNames eq 'ROI LineStyle'))[0]
              if (whParam ne -1) then begin
                 ROILineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI LineStyle'))[0]] > 0 < 6
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI LineStyle'))[0]] = ROILineStyle
              endif else ROILineStyle = 0
              whParam = (where(*(*self.pParamStruct).pNames eq 'ROI Hide'))[0]
              if (whParam ne -1) then begin
                 ROIHide = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Hide'))[0]] > 0 < 1
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI Hide'))[0]] = ROIHide
              endif else ROIHide = 0
              roiColor = [255,0,255]
              for i = 0, self->count()-1 do (self->get(position = i))->setProperty, style = ROIStyle, thick = ROIThick, lineStyle = ROILineStyle, hide = ROIHide, color = ROIColor

              oObjectModel = *poCurrROIGraphicModel->getByName('2D Object Model')
              if not(obj_valid(oObjectModel)) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Object Model', name = '2D Object Model')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 oObjectModel->add, self
              endif
          endcase
         '2D Sub Phase Model': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Sub Phase Model')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Sub Phase Model', name = '2D Sub Phase Model')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoSubPhaseModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
         '2D Image Model': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Image Model')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Image Model', name = '2D Image Model')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoImageModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
         '2D Border Model': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Border Model')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Border Model', name = '2D Border Model')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoBorderModel, oObjectModel
              endif
          endcase
         '2D Border Distance Model': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Border Distance Model')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Border Distance Model', name = '2D Border Distance Model')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoBorderDistanceModel, oObjectModel
              endif
          endcase
         '2D Border Neighbour Model': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Border Neighbour Model')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Border Neighbour Model', name = '2D Border Neighbour Model')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoBorderNeighbourModel, oObjectModel
              endif
          endcase
         '2D Center Distance Model': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Center Distance Model')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Center Distance Model', name = '2D Center Distance Model')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoCenterDistanceModel, oObjectModel
              endif
          endcase
         '2D Active Contours': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Active Contours')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Active Contours', name = '2D Active Contours')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoACModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
         '2D Adjacent Active Contours': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Adjacent Active Contours')
              if ~obj_valid(oObjectModel) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Adjacent Active Contours', name = '2D Adjacent Active Contours')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if ~(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoAABModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
         '2D Adjacent Active Contours from Skeleton (Mesh Contraction)': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Adjacent Active Contours from Skeleton (Mesh Contraction)')
              if ~obj_valid(oObjectModel) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Adjacent Active Contours from Skeleton (Mesh Contraction)', name = '2D Adjacent Active Contours from Skeleton (Mesh Contraction)')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if ~(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoAABModelFromSkeleton, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
         '2D Track Path': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Track Path')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Track Path', name = '2D Track Path')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoTrackModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
          '2D OF': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D OF')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D OF', name = '2D OF')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoOFModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
          '2D AAC+OF': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D AAC+OF')
              if (not(obj_valid(oObjectModel))) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D AAC+OF', name = '2D AAC+OF')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if not(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoAACOFModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase  
          '2D Voronoi Polygons from ROI mass center': begin
              oObjectModel = *poCurrROIGraphicModel->getByName('2D Voronoi Polygons from ROI mass center')
              if ~obj_valid(oObjectModel) then begin
                 oObjectModel = obj_new('IDLgrModel', uValue = '2D Voronoi Polygons from ROI mass center', name = '2D Voronoi Polygons from ROI mass center')
                 *poCurrROIGraphicModel->add, oObjectModel
              endif
              if ~(oObjectModel->isContained(self)) then begin
                 oObjectModel->remove, /all
                 self->getoVoronoiMCModel, oObjectModel, stack_tlb = stack_tlb
              endif
          endcase
         else:
       endcase
    endfor
end


pro C_sROIGroupObj::getoImageModel, oObjectModel, stack_tlb = stack_tlb

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
;   image = self->getGroupMaskIntensity()

   widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
      poCurrROI3DGroup = stateObj.poCurrROI3DGroup
   widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy

   if obj_valid(*poCurrROI3DGroup) then pVolState = (*poCurrROI3DGroup)->getpVolState()
   if ptr_valid(pVolState) then begin
      rgb_table = bytArr(3,256) + reform((*pVolState).rgbValues[0,*,*])
      opacVect_0 = bytArr(256) + (*pVolState).opacValues[0,*]
   endif
   if (n_elements(rgb_table) eq 0) then rgb_table = s_getPalette(3)
   if (n_elements(opacVect_0) eq 0) then opacVect_0 = make_array(255, /byte, value = 255)

   ;image = bytScl(image, min = 0, top = 255)
   image = 255 - image
   dimI = size(image, /dim)
   img = bytArr(4, dimI[0], dimI[1])

   img[0,*,*] = rgb_table[0,image]
   img[1,*,*] = rgb_table[1,image]
   img[2,*,*] = rgb_table[2,image]
   img[3,*,*] = 255;opacVect_0[image]

   xp = [-0.5, 0.5]
   yp = [-0.5, 0.5]
   self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   ;zp = zCoord_conv[1] * zPos
   zp = 0

   aspect_ratio = (1.*dimI[0])/dimI[1]
   case 1 of
    aspect_ratio gt 1: yp /= aspect_ratio
    aspect_ratio lt 1: xp /= aspect_ratio
    else:
   endcase

   ;bugfix, keep CENTERED aspect ratio
   fKeepCentered = 0b
   if fKeepCentered then begin
     div = 1. * (dimI[0]<dimI[1]) / (dimI[0]>dimI[1])
     if (dimI[0] lt dimI[1]) then xp *= div else yp *= div
     xp *= 0.5
     yp *= 0.5
   endif else begin
     ;xp = [-0.5, 0.5]
     ;yp = [-0.5, 0.5]
   endelse

   oTextureImage = obj_new('IDLgrImage', image,$
                            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                            interleave = 0,$
                            loc = [0,0],$
                            hide = 1)

   oObjectModel->add, obj_new('IDLgrPolygon',$
                              [[xp[0],yp[0],zp],[xp[1],yp[0],zp],[xp[1],yp[1],zp],[xp[0],yp[1],zp]],$
                              texture_coord = [[0,0],[1,0],[1,1],[0,1]],$
                              texture_map = oTextureImage, /texture_int,$
                              color = [255,255,255])
end


pro C_sROIGroupObj::getoSubPhaseModel, oObjectModel, stack_tlb = stack_tlb

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
   widget_control, stateObj_tlb, get_uValue = stateObj, /no_copy
      poCurrROI3DGroup = stateObj.poCurrROI3DGroup
   widget_control, stateObj_tlb, set_uValue = stateObj, /no_copy

    if obj_valid(*poCurrROI3DGroup) then pVolState = (*poCurrROI3DGroup)->getpVolState()
    if ptr_valid(pVolState) then begin
       rgb_table = bytArr(3,256) + reform((*pVolState).rgbValues[0,*,*])
       opacVect_0 = bytArr(256) + (*pVolState).opacValues[0,*]
    endif
    if (n_elements(rgb_table) eq 0) then rgb_table = s_getPalette(3)
    if (n_elements(opacVect_0) eq 0) then opacVect_0 = make_array(255, /byte, value = 255)

    rgb_table = s_getPalette(3)
    image = self->getGroupMaskIntensity()


    dimI = size(image, /dim)
    img = bytArr(4, dimI[0], dimI[1])

    image = bytScl(image, min = 0, top = 255)
    img[0,*,*] = rgb_table[0,image]
    img[1,*,*] = rgb_table[1,image]
    img[2,*,*] = rgb_table[2,image]
    img[3,*,*] = opacVect_0[image]

    self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    oTextureImage = obj_new('IDLgrImage', img, loc = [0.,0.], hide = 1, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )
    oObjectModel->add, oTextureImage

    xdata = (findgen(256)-128.)/256.
    ydata = (findgen(256)-128.)/256.
    xdata = (findgen(dimI[0])-floor(dimI[0]/2.))/dimI[0]
    ydata = (findgen(dimI[1])-floor(dimI[0]/2.))/dimI[1]
;    zCoord_conv = [-1,100.]
    oObjectModel->add, obj_new('IDLgrSurface', (image / 10000.),$
;                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                shading = 1,$
                                style = 2,$
                                datax = xdata,$
                                datay = ydata,$
                                color = [255,255,255],$
                                /texture_inter, texture_map = oTextureImage)
end


pro C_sROIGroupObj::getoBorderModel, oObjectModel, color = color
    if not(keyWord_set(color)) then color = [0,255,0]

    whParam = (where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]
    if (whParam ne -1) then begin
       color = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]],$
       *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]],$
       *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]] ] > [0,0,0] < [255,255,255]
    endif

    whParam = (where(*(*self.pParamStruct).pNames eq 'Border PolygonStyle'))[0]
    if (whParam ne -1) then begin
       modelStyle = (*(*self.pParamStruct).pValues[whParam] > 0) < 2
       *(*self.pParamStruct).pValues[whParam] = modelStyle
    endif

    whParam = (where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]
    if (whParam ne -1) then begin
       borderThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] > 1. < 10.
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] = borderThick
    endif else borderThick = 1.
borderThick = 9
    whParam = (where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]
    if (whParam ne -1) then begin
       borderLineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] > 0 < 6
                   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] = borderLineStyle
    endif else borderLineStyle = 0
borderLineStyle = 0
modelStyle = 1
color = [255,0,255]
    zPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Position [real]'))[0]]
    ;zPos = 0
    for i = 0, (self->count())-1 do $
      (self->get(position = i))->addObjectBorderPolygon, color = color, oModel = oObjectModel, modelStyle = modelStyle, objNumber = i,$
                                                         borderThick = borderThick, borderLineStyle = borderLineStyle, zPos = zPos
end


pro C_sROIGroupObj::getoBorderDistanceModel, oObjectModel, color = color, nNearestNeighbour = nNearestNeighbour
    if not(keyWord_set(color)) then color = [0,255,0]

    oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = 'Inter-Object Border Distance')
    if obj_valid(oROIParam) then begin
       pParamStruct = oROIParam->getpParamStruct()
       if not(ptr_valid((*pParamStruct).pROIConnectMatrix)) then return
       pValueStruct = oROIParam->getpValueStruct(position = (where((*(*pParamStruct).pNames) eq 'Minimal B-Distance'))[0])
       case ((*(*pValueStruct).pValues)[(where((*(*pValueStruct).pNames) eq 'Number of Nearest Neighbors to Consider'))[0]]) of
            1: nNearestNeighbour = 1
            else: nNearestNeighbour = ((*(*pValueStruct).pValues)[(where((*(*pValueStruct).pNames) eq 'Number of Nearest Neighbors to Consider'))[0]]) < ((size((*(*pParamStruct).pROIConnectMatrix), /dim))[1])
       endcase

       self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
       for i = 1, (size((*(*pParamStruct).pROIConnectMatrix), /dim))[0] -1 do begin
         for j = 0, nNearestNeighbour-1 do begin
            case 1 of
            ((*(*pParamStruct).pROIConnectMatrix)[i,j] eq -1): oObjectModel->add, obj_new('IDLgrPolyline', color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )
            ((*(*pParamStruct).pROIConnectMatrix)[i,j] ge i) : oObjectModel->add, obj_new('IDLgrPolyline', thick = 1,$
                                  [(*(*pParamStruct).pROICoordinateMatrix)[0, i, (*(*pParamStruct).pROIConnectMatrix)[i,j]],$
                                   (*(*pParamStruct).pROICoordinateMatrix)[1, i, (*(*pParamStruct).pROIConnectMatrix)[i,j]]],$
                                  [(*(*pParamStruct).pROICoordinateMatrix)[0, (*(*pParamStruct).pROIConnectMatrix)[i,j], i],$
                                   (*(*pParamStruct).pROICoordinateMatrix)[1, (*(*pParamStruct).pROIConnectMatrix)[i,j], i]],$
                                  color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )
            else: oObjectModel->add, obj_new('IDLgrPolyline', thick = 1,$
                                  [(*(*pParamStruct).pROICoordinateMatrix)[0, (*(*pParamStruct).pROIConnectMatrix)[i,j], i],$
                                   (*(*pParamStruct).pROICoordinateMatrix)[1, (*(*pParamStruct).pROIConnectMatrix)[i,j], i]],$
                                  [(*(*pParamStruct).pROICoordinateMatrix)[0, i, (*(*pParamStruct).pROIConnectMatrix)[i,j]],$
                                   (*(*pParamStruct).pROICoordinateMatrix)[1, i, (*(*pParamStruct).pROIConnectMatrix)[i,j]]],$
                                  color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )
            endcase
         endfor
       endfor
    endif
end


pro C_sROIGroupObj::getoBorderNeighbourModel, oObjectModel, color = color
    if not(keyWord_set(color)) then color = [0,255,0]

    oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = 'Inter-Object Border Neighbour')

    if obj_valid(oROIParam) then begin
       pParamStruct = oROIParam->getpParamStruct()
       if not(ptr_valid((*pParamStruct).pROICoordinateMatrix)) then return

       self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
       if ((*(*pParamStruct).pROICoordinateMatrix)[0] ne -1) then begin
          for i = 1, (size((*(*pParamStruct).pROICoordinateMatrix), /dim))[1] -1 do begin
             oObjectModel->add, obj_new('IDLgrPolyline',$
                                     [(*(*pParamStruct).pROICoordinateMatrix)[0, i],$
                                      (*(*pParamStruct).pROICoordinateMatrix)[1, i]],$
                                     [(*(*pParamStruct).pROICoordinateMatrix)[2, i],$
                                      (*(*pParamStruct).pROICoordinateMatrix)[3, i]],$
                                     color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )
          endfor
       endif
    endif
end


pro C_sROIGroupObj::getoCenterDistanceModel, oObjectModel, color = color
    if not(keyWord_set(color)) then color = [0,200,0]
    self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

    oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = 'Inter-Object Center Distance')
    if obj_valid(oROIParam) then begin
       pParamStruct = oROIParam->getpParamStruct()
       pValueStruct = oROIParam->getpValueStruct(position = (where( (*(*pParamStruct).pNames) eq 'Minimal C-Distance'  ))[0])
       case ((*(*pValueStruct).pValues)[(where((*(*pValueStruct).pNames) eq 'Number of Nearest Neighbors to Consider'))[0]]) of
            1: nNearestNeighbour = 1
            else: nNearestNeighbour = ((*(*pValueStruct).pValues)[(where((*(*pValueStruct).pNames) eq 'Number of Nearest Neighbors to Consider'))[0]]) < ((size((*(*pParamStruct).pROIConnectMatrix), /dim))[1])
       endcase

       for i = 0, (size((*(*pParamStruct).pROIConnectMatrix), /dim))[0] -1 do begin
         for j = 0, nNearestNeighbour -1 do begin
           oObjectModel->add, obj_new('IDLgrPolyline',$
                                  [(*(*pParamStruct).pROICoordinateMatrix)[0,i],$
                                   (*(*pParamStruct).pROICoordinateMatrix)[0,(*(*pParamStruct).pROIConnectMatrix)[i,j]]],$
                                  [(*(*pParamStruct).pROICoordinateMatrix)[1,i],$
                                   (*(*pParamStruct).pROICoordinateMatrix)[1,(*(*pParamStruct).pROIConnectMatrix)[i,j]]],$
                                  color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )
         endfor
       endfor
    endif
end


pro C_sROIGroupObj::getoACModel, oObjectModel, color = color, stack_tlb = stack_tlb

  nObj = self->count()
  if (nObj lt 1) then return

  if ~keyWord_set(color) then color = [0,255,0]

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]
  if (whParam ne -1) then begin
     color = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]],$
              *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]],$
              *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]] ] > [0,0,0] < [255,255,255]
  endif
  color = [0,0,255]

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border PolygonStyle'))[0]
  if (whParam ne -1) then begin
    modelStyle = (*(*self.pParamStruct).pValues[whParam] > 0) < 2
    *(*self.pParamStruct).pValues[whParam] = modelStyle
  endif

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]
  if (whParam ne -1) then begin
    borderThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] > 1. < 10.
                  *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] = borderThick
  endif else borderThick = 1.
borderThick = 9
  whParam = (where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]
  if (whParam ne -1) then begin
    borderLineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] > 0 < 6
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] = borderLineStyle
  endif else borderLineStyle = 0

  zPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Position [real]'))[0]]
  zPos = 0.
  for i = 0L, nObj-1 do $
    (self->get(position = i))->addObjectBorderPolygon, color = color, oModel = oObjectModel, modelStyle = modelStyle, objNumber = i,$
                                                       borderThick = borderThick, borderLineStyle = borderLineStyle, zPos = zPos
  snake = obj_new('C_sActiveContour', image,$
                                      alpha = 0.2,$
                                      beta  = 1.0,$
                                      gamma = 1.,$
                                      kappa = 1.,$
                                      mu    = .05,$
                                      iterations     = 80,$
                                      gvf_iterations = 20)
  snake->calcGGVF
  perimeterFactor = 1.

  for i = 0L, (oObjectModel->count())-1 do begin
    (oObjectModel->get(position = i))->getProperty, data = polygon
    snake->setContour, transpose(polygon[0,*]), transpose(polygon[1,*])
    npts = round((snake->getPerimeter()) * perimeterFactor) > 16
    snake->arcSample, points = npts
     result = snake->adjustContour()
    (oObjectModel->get(position = i))->setProperty, data = [transpose(snake->getXcoords()), transpose(snake->getYcoords())], color = color
  endfor
  obj_destroy, snake
end


; getoVoronoiMCModel
;  Returns the Voronoi polygons from each ROI's center of mass
pro C_sROIGroupObj::getoVoronoiMCModel, oObjectModel, color = color, stack_tlb = stack_tlb

  nObj = self->count()
  if (nObj lt 1) then return

  if ~keyword_set(color) then color = [0,255,0]
  ;whParam = (where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]
  ;if (whParam ne -1) then begin
  ;  color = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]],$
  ;           *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]],$
  ;           *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]] ] > [0,0,0] < [255,255,255]
  ;endif

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border PolygonStyle'))[0]
  if (whParam ne -1) then begin
    modelStyle = (*(*self.pParamStruct).pValues[whParam] > 0) < 2
    *(*self.pParamStruct).pValues[whParam] = modelStyle
  endif

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]
  if (whParam ne -1) then begin
    borderThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] > 1. < 10.
                  *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] = borderThick
  endif else borderThick = 1.

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]
  if (whParam ne -1) then begin
    borderLineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] > 0 < 6
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] = borderLineStyle
  endif else borderLineStyle = 0

  zPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Position [real]'))[0]]
  for i = 0L, nObj-1 do $
    (self->get(position = i))->addObjectBorderPolygon, color = color, oModel = oObjectModel, modelStyle = modelStyle, objNumber = i,$
                                                       borderThick = borderThick, borderLineStyle = borderLineStyle, zPos = zPos

  moments2D = s_2Dmoments(self->getGroupMask(), order = 3, fAreaNorm = 1b)
  xCMcoords = moments2D[*,1]
  yCMcoords = moments2D[*,2]
  xMax = *(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'x-Size [pixel]'))[0]]
  yMax = *(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'y-Size [pixel]'))[0]]
  fClipConvexHull = 1b
  vPolys = voronoiPolygons(xCMcoords, yCMcoords, xMaxCoord = xMax-1, yMaxCoord = yMax-1, clipPolygonX = clipPolyX, fClipConvexHull = fClipConvexHull, polyAreas = vPolyAreas)
;  sortedAreas = sort(vPolyAreas)
;  sortedAreasColorVec = bytArr(3, nObj)
;  deltaColor = nObj / 256
;  deltaColorPos = 0
;  for i = 0, 255 do begin
;    whColor = sortedAreas[deltaColorPos:deltaColorPos+deltaColor-1]
;    for j = 0, deltaColor-1 do $
;      sortedAreasColorVec[*, whColor[j] < (nObj-1)] = [i, 0, 0]
;    deltaColorPos += deltaColor
;  endfor

  for i = 0L, nObj-1 do begin
    (oObjectModel->get(position = i))->setProperty, data = [transpose((*vPolys[i])[*,0]), transpose((*vPolys[i])[*,1])],$
                                                    vert_color = transpose([[replicate(0, nObj)], [replicate(255, nObj)], [replicate(0, nObj)]]), shininess = 128.0, thick = borderThick, style = 1 ; 0 -> "only vertices"
    ptr_free, vPolys[i]
  endfor
end


; getoAABModel
;
; Uses Active Adjacent Boundary Model a.k.a. Adjacent Active Contours
; See C_sAABContainer__Define.pro
;
; History
;  2013.08.01 First version. JJara
;  TODO Victor
pro C_sROIGroupObj::getoAABModel, oObjectModel, color = color, stack_tlb = stack_tlb

  print, 'WARNING: THIS METHOD IS UNDER DEVELOPMENT!'
  nObj = self->count()
  if (nObj lt 1) then return

  if ~keyword_set(color) then color = [0,255,0]

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]
  if (whParam ne -1) then begin
    color = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]],$
             *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]],$
             *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]] ] > [0,0,0] < [255,255,255]
  endif else begin
    colorDf = [0,255,0]
    color   = ColorDf
  endelse

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border PolygonStyle'))[0]
  if (whParam ne -1) then begin
    modelStyle = (*(*self.pParamStruct).pValues[whParam] > 0) < 2
    *(*self.pParamStruct).pValues[whParam] = modelStyle
  endif

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]
  if (whParam ne -1) then begin
    borderThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] > 1. < 10.
                  *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] = borderThick
  endif else borderThick = 1.

  whParam = (where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]
  if (whParam ne -1) then begin
    borderLineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] > 0 < 6
                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] = borderLineStyle
  endif else borderLineStyle = 0

  zPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Position [real]'))[0]]
  zPos = 0.
  for i = 0L, nObj-1 do $
    (self->get(position = i))->addObjectBorderPolygon, color = color, oModel = oObjectModel, modelStyle = modelStyle, objNumber = i,$
                                                       borderThick = borderThick, borderLineStyle = borderLineStyle, zPos = zPos

  fComputeOnly              = 0b ; If set, no graphic output will be produced
  fSaveContours             = 0b ; Not yet
  fDoVectorFieldCalculation = 0b ; Can force or avoid vector field calculation
  fDoContourAdjustment      = 1b ; Option to avoid contour adjustment.
  fUseAABcolorCodes         = 1b ; Option to use AABM color codes for contour vertices (see documentation in C_sAABContainer__Define).

  ; Initialization options. If all are set to zero, the ROI polygons are used by default.
  fVoronoiPolygons          = 0b ; Initialize AABM polygons from the Voronoi diagram (using the centroid|mass center from each ROI polygon).
  fConvexHullPolygons       = 0b ; Initialize AABM polygons from the the convex hull polygon for each ROI.
  fSnake                    = 1b ; Initialize by applying "standalone" active contours first.
  fVoronoiPolygons    = fVoronoiPolygons    and ~fConvexHullPolygons ; Convex hull init. takes priority over Voronoi polygons.
  fConvexHullPolygons = fConvexHullPolygons and ~fSnake              ; Snakes init. takes priority over Convex hull. 
  fFilterROIcontoursBySize = 1b
  filterROIcontourMinSize  = 50
  snakeAlpha        = 0.03
  snakeBeta         = 0.1
  snakeGamma        = 1.25
  snakeKappa        = 0.2
  snakePerimFactor  = 1.0
  snakeVFtype       = 1
  snakeIterations   = 1000
  snakeIterationsVF = 100
  snakeMu   = 0.5
  aabmAlpha = 0.04
  aabmBeta  = 0.1
  aabmGamma = 1.2
  aabmKappa = 0.2
  aabmContourMaxIterations    = 300
  aabmContourConvergenceLimit = 0.01
  aabmPerimeterFactor         = 1.0

  aabmVFtype = 'GGVF'; 'GVF', 'GGVF' or 'EP-GVF'
  aabmVFmaxIterations      = 300
  aabmVFconvergenceLimit   = 0.01
  aabmVFmu                 = 0.1
  aabmEPGVFnoiseCut        = 0.1
  aabmEPGVFnoiseRange      = 0.2
  ;aabmEPGVFnumericalStep     = 1   ; optional parameter, could be left undefined
  aambEPGVFsigmaGaussianBlur = 0   ; 0-> no blur before vector field computation
  aabmFverbose               = 1b  ; set verbose mode ON while doing my PhD thesis ;)
  aabmFnoInterpolateAtFirst  = 1b  ; 1-> avoid interpolation while setting contours
  aabmMinContourPointCount   = 16
  aabmPointSamplingDistance  = 1.0
  aabmProximityDistance      = 2.0 ; adjacency detection threshold distance, in pixels
  aabmSeparationDistance     = 0.0 ; adjacency adjustment distance, in pixels

  nObj1 = oObjectModel->count()
  roiIndexVector = bytArr(nObj1) + 1
  if fFilterROIcontoursBySize then begin
    for i = 0u, nObj1-1 do begin
      (oObjectModel->get(position = i))->getProperty, data = polygon
      polygonArea = polyArea(xCoords = transpose(polygon[0,*]), yCoords = transpose(polygon[1,*]))
      if (polygonArea le filterROIcontourMinSize) then begin
        print, 'ROI ', i, ' is too small to adjust active contour at its scale (area ', polygonArea, '<=', filterROIcontourMinSize, ')'
        roiIndexVector[i] = 0
      endif
    endfor
  endif

  whROI = where(roiIndexVector eq 1, countROIs)
  nObj2 = countROIs
  if (nObj2 lt 1) then begin
    print, 'No ROIs to perform AAB adjustment. Returning...'
    return
  endif

  widget_control, stateObj_tlb, get_uValue = state, /no_copy
    roiGroupFilePathName = state.currROI2DGroupFileName
  widget_control, stateObj_tlb, set_uValue = state, /no_copy
  pathSep = path_sep()
  roiGroupFilePathNameBase = strMid(roiGroupFilePathName, 0, strPos(roiGroupFilePathName, '.', /reverse_search))
  case 1 of
    fVoronoiPolygons    eq 1: roiGroupFilePathNameBase += '_InitVoronoi_'
    fConvexHullPolygons eq 1: roiGroupFilePathNameBase += '_InitConvexHull_'
    fSnake              eq 1: roiGroupFilePathNameBase += '_InitSnake_'
    else:                     roiGroupFilePathNameBase += '_InitRoiPolygons_'
  endcase
  if aabmFnoInterpolateAtFirst then roiGroupFilePathNameBase += '_NoInitialInterp_'
  if fFilterROIcontoursBySize  then roiGroupFilePathNameBase += '_SizeFilter' + strCompress(string(filterROIcontourMinSize), /remove_all) + '_'

  fAddCustomFileNamePrefix = 1b
  customFileNamePrefix     = ''
  if (fAddCustomFileNamePrefix and (strLen(customFileNamePrefix) gt 0)) then $
  roiGroupFilePathNameBase = customFileNamePrefix + roiGroupFilePathNameBase

  oAABM = obj_new('C_sAABContainer',$
                  image,$
                  alphaVal = aabmAlpha,$
                  betaVal  = aabmBeta,$
                  gammaVal = aabmGamma,$
                  kappaVal = fDoVectorFieldCalculation gt 0 ? aabmKappa : 0.0,$
                  proximityDist           = aabmProximityDistance,$
                  separationDist          = aabmSeparationDistance,$
                  contourMaxIterations    = aabmContourMaxIterations,$
                  vfMaxIterations         = aabmVFmaxIterations,$
                  contourSamplingDistance = aabmPointSamplingDistance,$
                  fVerbose                = aabmFverbose,$
                  roiGroupFilePath        = roiGroupFilePathNameBase)
  if ~obj_valid(oAABM) then return

    ; Perform image vector field calculation (if fDoVectorFieldCalculation is non-0).
  if fDoVectorFieldCalculation then $
  fVFok = oAABM->calcVectorField(type = aabmVFtype, mu = aabmVFmu, iterations = aabmVFmaxIterations, convergenceLimit = aabmVFconvergenceLimit)

  ; Someday a special case for processing a different number of contours could be used. So...
  nObj2 = oObjectModel->count()
  if (nObj2 lt 1) then begin
    obj_destroy, oAABM
    return
  endif

  fSet = oAABM->preSetNumContours(nObj2)
  case 1 of

    (fConvexHullPolygons gt 0): begin
      for i = 0L, nObj2-1 do begin
        (oObjectModel->get(position = i))->getProperty, data = polygon
        convexHullPolyIndices = convexHullPolygon(transpose(polygon[0,*]), transpose(polygon[1,*]))
        polygonLineSample, (polygon[0,*])[convexHullPolyIndices], (polygon[1,*])[convexHullPolyIndices], xInt, yInt, nPointsPerPix = 1
        fContourAdded = oAABM->setContour(xInt, yInt, i, fNoInterpolate = 1)
      endfor
    endcase

    (fVoronoiPolygons gt 0): begin
      imgSize = size(image, /dim)
      xMax = imgSize[0]-1
      yMax = imgSize[1]-1
      xCentroids = fltArr(nObj2)
      yCentroids = fltArr(nObj2)
      for i = 0u, nObj2-1 do begin
        (oObjectModel->get(position = i))->getProperty, data = polygon
        centroid = polygonCentroid(transpose(polygon[0,*]), transpose(polygon[1,*]))
        xCentroids[i] = centroid[0]
        yCentroids[i] = centroid[1]
      endfor
      pContoursVoronoi = voronoiPolygons(xCentroids, yCentroids, xMaxCoord = xMax, yMaxCoord = yMax, /fInterpolateLines)
      fContourAdded    = oAABM->setContours(pContoursVoronoi, nObj2, fNoInterpolate = 1)
    endcase

    (fSnake gt 0): begin
      pSnakesX = ptrArr(nObj2)
      pSnakesY = ptrArr(nObj2)
      ; Create snake object and adjust contours.
      oSnake = obj_new('C_sActiveContour', image, alpha = snakeAlpha, beta = snakeBeta, gamma = snakeGamma, kappa = snakeKappa, mu = snakeMu, iterations = snakeIterations, gvf_iterations = snakeIterationsVF)
      case snakeVFtype of
        1   : oSnake->calcGGVF
        2   : oSnake->calcEPGVF
        else: oSnake->calcGVF
      endcase
      for i = 0L, nObj2-1 do begin
        (oObjectModel->get(position = i))->getProperty, data = polygon
        oSnake->setContour, transpose(polygon[0,*]), transpose(polygon[1,*])
        npts        = round((oSnake->getPerimeter()) * snakePerimFactor) > 16
        ;oSnake->arcSample, points = npts
        result      = oSnake->adjustContour(perimeterFactor = snakePerimFactor)
        pSnakesX[i] = ptr_new(transpose(oSnake->getXcoords()))
        pSnakesY[i] = ptr_new(transpose(oSnake->getYcoords()))
      endfor
      obj_destroy, oSnake
      ; Set snake contours as AABM initial contours.
      for i = 0L, nObj2-1 do $
        fContourAdded = oAABM->setContour(transpose(*pSnakesX[i]), transpose(*pSnakesY[i]), i, fNoInterpolate = aabmFnoInterpolateAtFirst)
      for i = nObj2-1, 0, -1 do ptr_free, pSnakesX[i], pSnakesY[i]
    endcase

    else: begin
      for i = 0L, nObj2-1 do begin
        (oObjectModel->get(position = i))->getProperty, data = polygon
        ; Interpolate linearly, but avoiding replicated points.
        polygonLineSample, polygon[0,*], polygon[1,*], xInt, yInt, nPointsPerPix = aabmPointSamplingDistance, /fCloseOutput
        nPtsClosed    = n_elements(xInt)
        fContourAdded = oAABM->setContour(xInt[0:nPtsClosed-1], yInt[0:nPtsClosed-1], i, fNoInterpolate = aabmFnoInterpolateAtFirst)
      endfor
    endcase

  endcase

  ; TODO JJW tmp flag for trial runs
  fDoC = 1 and fDoContourAdjustment
  if (fDoC ne 0) then begin
    ;distMap = oAABM->makeDistanceMapFromRois()
    fAdjust = oAABM->adjustContoursC(fDebug = aabmFverbose, outCoordsX = outCoordsX, outCoordsY = outCoordsY)
    nObjOut = n_elements(outCoordsX)
    aabmOut = oAABM->getContours(nContours = nObjOut, pAABMvertexColors = pContourColors)
    stop
    for i = 0L, nObjOut-1 do begin
      nPts  = n_elements(*(pContourColors[i]))
      vertColorCodes = *(pContourColors[i])
      wh1   = where(vertColorCodes eq 2, count_r)
      rvert = bytArr(nPts)
      if (count_r gt 0) then rvert[wh1] = 255   ; red for overlapping (unadjusted) vertices
      wh2   = where(vertColorCodes eq 1, count_g)
      gvert = bytArr(nPts)
      if (count_g gt 0) then gvert[wh2] = 255   ; green for proximal (unadjusted) vertices
      bvert = bytArr(nPts)
      whNot = where(vertColorCodes eq 0, count_b)
      if (count_b gt 0) then bvert[whNot] = 255 ; blue for free (unadjusted) vertices
      wh3   = where(vertColorCodes eq 3, count_y)
      if (count_y gt 0) then begin              ; yellow for shared (adjusted) vertices
        rvert[wh3] = 255
        gvert[wh3] = 255
      endif
      color = [transpose(rvert), transpose(gvert), transpose(bvert)]
      ;(oObjectModel->get(position = i))->setProperty, data = [transpose(*((aabmOut.pX)[i])), transpose(*((aabmOut.pY)[i]))],$
      ;(oObjectModel->get(position = i))->setProperty, data = [*outCoordsX[i], *outCoordsY[i]],$
      (oObjectModel->get(position = i))->setProperty, data = [transpose(*outCoordsX[i]), transpose(*outCoordsY[i])],$
                                                      vert_color = color, shininess = 128.0, thick = 2.0, style = 0 ; 0 -> "only vertices"
    endfor
    fCalcParams = 1b
    if fCalcParams then begin
      xySizePerPixel = [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [real]'))[0]] * 1. / $
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [real]'))[0]] * 1. / $
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] ]
      oParam = obj_new('C_sROIParam_ObjAABM_QualityIndicators')
      oParam->apply, mask = [-1], xySizePerPixel = xySizePerPixel, C_sROIGroupObj = self, stack_tlb = stack_tlb, $
                     baseFileName = roiGroupFilePathNameBase + 'aabmProximityDistance' + strCompress(string(aabmProximityDistance), /rem),$
                     pRoiCoordsX = outCoordsX, pRoiCoordsY = outCoordsY
    endif
    obj_destroy, oAABM
    return
  endif

    ; Perform contour adjustment (if fDoContourAdjustment is non-0).
  fAABMsuccess = fDoContourAdjustment ? oAABM->adjustContours(fDebug = aabmFverbose) : 0b
  if (fDoContourAdjustment ne 0) and (fAABMsuccess ne 1) then begin
    print, 'Something went wrong while adjusting the contours. Nothing to show :('
    obj_destroy, oAABM
    return
  endif

  if (fComputeOnly eq 1) then begin
    print, 'AABM compute-only option is active: contours computed but no graphical model. Returning...'
    if (fSaveContours eq 1) then begin
      print, 'Save AABM contours not yet implemented'
      stop
    endif
    obj_destroy, oAABM
    return
  endif

    ; Finally, get adjusted contours and add graphic objects.
  if (fUseAABcolorCodes and ~fDoContourAdjustment) then begin
    fColor = oAABM->computeAABcontourVertexTypes()
    if (fColor ge 0) then aabmOut = oAABM->getContours(nContours = nObjOut, pAABMvertexColors = pContourColors)
  endif else begin
    if (fUseAABcolorCodes and fDoContourAdjustment) $
    then aabmOut = oAABM->getContours(nContours = nObjOut, pAABMvertexColors = pContourColors) $
    else aabmOut = oAABM->getContours(nContours = nObjOut)
  endelse

  if (nObjOut ge 1) then begin

    fUseAABcolorCodes = fUseAABcolorCodes and (n_elements(pContourColors) gt 0)
    if fUseAABcolorCodes then print, 'Using AABM contour colors for rendering'

    if fUseAABcolorCodes then $
    for i = 0L, nObjOut-1 do begin

      nPts  = n_elements(*(pContourColors[i]))
      vertColorCodes = *(pContourColors[i])
      wh1   = where(vertColorCodes eq 2, count_r)
      rvert = bytArr(nPts)
      if (count_r gt 0) then rvert[wh1] = 255

      wh2   = where(vertColorCodes eq 1, count_g)
      gvert = bytArr(nPts)
      if (count_g gt 0) then gvert[wh2] = 255

      bvert = bytArr(nPts)
      whNot = where(vertColorCodes eq 0, count_b)
      if (count_b gt 0) then bvert[whNot] = 255

      color = [transpose(rvert), transpose(gvert), transpose(bvert)]
      ;winding = calcPolygonWinding(*((aabmOut.pX)[i]), *((aabmOut.pY)[i]))
      ;print, 'polygon winding is ', winding eq 1 ? 'CW' : 'CCW'
      objThick = 5.0
      objStyle = 0
      (oObjectModel->get(position = i))->setProperty, data = [transpose(*((aabmOut.pX)[i])), transpose(*((aabmOut.pY)[i]))],$
                                                      shininess = 128.0, thick = objThick, vert_color = color, style = objStyle ; 0 -> "only vertices"
    endfor else for i = 0L, nObjOut-1 do $
      (oObjectModel->get(position = i))->setProperty, data = [transpose(*((aabmOut.pX)[i])), transpose(*((aabmOut.pY)[i]))], color = color
  endif

  print, nObjOut, ' oAABM contour(s) retrieved'
  obj_destroy, oAABM
end


; getoAABModelFromSkeleton
; JJ: NOT QUITE READY
pro C_sROIGroupObj::getoAABModelFromSkeleton, oObjectModel, color = color, stack_tlb = stack_tlb

  fVerbose = 0b
  nObj = self->count()
  if (nObj lt 1) then return

  ; Skeleton parameter setting section
   ; i. Create a 3D ROI group to compute the skeleton 

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos, stateObj_tlb = stateObj_tlb
 ; widget_control, stack_tlb, get_uValue = stackState, /no_copy
 ;   imageStackInfoObject = *stackState.pImageStackInfoObject
 ; widget_control, stack_tlb, set_uValue = stackState, /no_copy
 ; maskImage = imageStackInfoObject->getSelectedClusterMask(tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos)
  totalZNum  = 1
  oROI3DGroup = obj_new('C_sROI3DGroupObj',$
                        clusPos = clusPos,$
                        tPos  = tPos,$
                        chPos = chPos,$
                        zPos  = zPos,$
                        zSliceInitial = zPos,$
                        timeAfterStart = *(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'Time After Start [s]'))[0]],$
                        xyzFramePixSize = [*(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'x-Size [pixel]'))[0]],$
                                           *(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'y-Size [pixel]'))[0]],$
                                           totalZNum],$
                        xyzFrameRealSize = [*(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'x-Size [real]'))[0]],$
                                            *(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'y-Size [real]'))[0]],$
                                            totalZNum * *(*(self.pParamStruct)).pValues[(where(*((*(self.pParamStruct)).pNames) eq 'z-Interval [real]'))[0]]])
  ; ii. Perform skeletonization
  oROI3DGroup->initialize3DModels, stack_tlb = stack_tlb, fAcceptAllObjects = 1
;  s_getoSkeletonFromMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, o3DroiGroup = oROI3DGroup, fSaveSkeletonInSavFile = 1b, fUseSavedSkeletons = 1b, fGetSkeletonData = 1,$; fNoObjectModel = 1,$
;                               pSkeletonVertices = pSkeletonVertices, pSkeletonConnectivity = pSkeletonConnectivity, pSkeletonMeshMapping = pSkeletonMeshMapping
  obj_destroy, oROI3DGroup
  for i = 0L, nObj-1 do begin
    ptr_free, pSkeletonVertices[i], pSkeletonConnectivity[i], pSkeletonMeshMapping[i]
  endfor
  stop

   ; iii. Set skeleton model parameters
  xyzSizePerPixel = xyzFrameRealSize
  xysSizePerPixel[0] /= xyzFramePixSize[0]
  xysSizePerPixel[1] /= xyzFramePixSize[1]
  xysSizePerPixel[2] /= xyzFramePixSize[2]
  fAllowLoops = 1b
  vEdgeRoot   = -1 ; Allows to manually define the root node for the skeleton graph (default:-1 to search for the largest edge)

  ; iv. Create skeleton graph data structure for extracting ROI contours
  finalVertices = *pSkeletonVertices[0]
  nEdges      = n_elements(*pSkeletonEdges[0])/3
  vEdges      = make_array(2, nEdges, /long)
  vEdges[0,*] = (*pEdges[0])[1:(3*nEdges)-1:3]
  vEdges[1,*] = (*pEdges[0])[2:(3*nEdges)-1:3]

  ; If set to -1, search for the largest edge with external node as root edge.
  if (vEdgeRoot eq -1) then begin
    distanceMax = 0.0
    vFlip       = 0b
    for index = 0L, nEdges-1 do begin
      edgeStart = finalVertices[*, vEdges[0,index]]
      edgeEnd   = finalVertices[*, vEdges[1,index]]
      delta     = edgeEnd - edgeStart
      distance  = sqrt(total(delta * delta))

      if (distance gt distanceMax) then begin
        wLeft  = where((vEdges[0,*] eq vEdges[0,index]) or (vEdges[1,*] eq vEdges[0,index]), /L64)
        wRight = where((vEdges[0,*] eq vEdges[1,index]) or (vEdges[1,*] eq vEdges[1,index]), /L64)

        if (((wLeft[0] ne -1) and (n_elements(wLeft) eq 1)) and (n_elements(wRight) gt 1)) then begin
          distanceMax = distance
          vEdgeRoot   = index
          vFlip       = 1b
        endif
        if (((wRight[0] ne -1) and (n_elements(wRight) eq 1)) and (n_elements(wLeft) gt 1)) then begin
          distanceMax = distance
          vEdgeRoot   = index
          vFlip       = 0b
        endif
      endif
    endfor
  endif

  oSkeletonGraph = obj_new('C_s3DGraphSkelDir')
  oSkeletonGraph->masterGraphCreation, vEdgeRoot = vEdgeRoot, vVertices = finalVertices, vAristas = vEdges, $
                                       xyzSizePerPixel = xyzSizePerPixel, vFlip = vFlip, fAllowLoops = fAllowLoops

  ; Now get the skeleton "curve" edges (full edges) between junctions/endpoints
  skeletonEdgePaths = oSkeletonGraph->createFullSkelEdges()
  obj_destroy, oSkeletonGraph

  vec = make_array(dim_edge, /float, value = 0.0)
  nEdges = n_elements(skeletonEdgePaths)
  edgeLengths = [-1]
  lindex  = 0L
  nSnakes = 0L
  pEdgeSnakes = ptrArr(1)

  while (lindex lt nEdges) do begin
    first   = lindex + 1L
    last    = lindex + skeletonEdgePaths[lindex]
    indexes = skeletonEdgePaths[first:last]

    nSegments = n_elements(indexes) - 2
    segmentsX = [-1]
    segmentsY = [-1]
    segmentsZ = [-1]

    dx = (vertices[0, indexes[nSnakes]] - vertices[0, indexes[nSnakes+1]])^2
    dy = (vertices[1, indexes[nSnakes]] - vertices[1, indexes[nSnakes+1]])^2
    dz = (vertices[2, indexes[nSnakes]] - vertices[2, indexes[nSnakes+1]])^2

    length       = computePathLength(finalVertices, indexes, xyzSizePerPixel)
    pEdgeSnakes[nSnakes] = ptr_new(finalVertices[indexes])
    pEdgeSnakes  = [pEdgeSnakes, ptr_new()]
    edgeLengths  = (edgeLengths[0] eq -1) ? [length] : [edgeLengths, length]
    vec[nSnakes] = length
    nSnakes += 1L
    lindex = lindex + skeletonEdgePaths[lindex] + 1L
  endwhile

  if (nSnakes eq 0) then begin
    return
  endif

  pEdgeSnakes = pEdgeSnakes[0: nSnakes-1]
  pEdgeSnakesFinal = ptrArr(nSnakes)

  ;print, '3D GraphSkel Curved Edge Length ', vec
  ;print, '3D GraphSkel Curved Edge Length  [x]', edgeLengths

  ; Active contour parameter setting section
  acAlpha = 0.01
  acBeta  = 1.0
  acGamma = 1.0
  acKappa = 1.0
  acIterations = 140
  acSamplingDistance = 1.0
  acMinContourPoint  = 20

  fDoVectorFieldCalculation = 1b
  acVFiterations = 500
  acVFtype = 2
  acVFconvergenceLimit = 0.01

  acVF_GVF_mu = 0.1
  acVF_EP_GVF_noiseFloor = 0.0
  acVF_EP_GVF_noiseRange = 0.0

    ; Active contour adjustment section
  if (fDoVectorFieldCalculation ne 0) then begin
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
  endif else image = bytArr(1, 1)

  oAAB = obj_new('C_sAABContainer', image, $
                                    alphaVal = acAlpha,$
                                    betaVal  = acBeta, $
                                    gammaVal = acGamma,$
                                    kappaVal = fDoVectorFieldCalculation ne 0 ? acKappa : 0.0,$
                                    contourMaxIterations    = acIterations,$
                                    contourSamplingDistance = acSamplingDistance,$
                                    minContourPointCount    = acMinContourPoint,$
                                    fVerbose                = fVerbose)

  if fDoVectorFieldCalculation then $
    fVFok = oAAB->calcVectorField(type = acVFtype, mu = aabmVFmu, iterations = acVFiterations, convergenceLimit = acVFconvergenceLimit)

  for i = 0L, nSnakes-1 do begin
    fAdjust = oAAB->adjustContour()
    pEdgeSnakesFinal[i] = ptr_new(oAAB->relaxSingleContour())
  endfor

    ; Add graphic objects and cleaning section
  obj_destroy, oAAB

  for i = 0L, nSnakes-1 do begin
    ;if ptr_valid(pEdgeSnakes[i]) then print, i, '-' *pEdgeSnakes[i]
    oPolyline = obj_new('IDLgrPolyline', data = *pEdgeSnakesFinal[i])
    oObjectModel->add, oPolyline,$
                       color = color, alpha_channel = 1.0, thick = 2.0,$
                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                       name = strCompress('2DacFromSkeleton:' + string(i), /rem), uValue = 'ObjInOriginalPosition'
                      ;uValue = {name:'BaseObject', number:i})
    ptr_free, pEdgeSnakes[i], pEdgeSnakesFinal[i]
  endfor
end


pro C_sROIGroupObj::getoTrackModel, oObjectModel, color = color, stack_tlb = stack_tlb

   if not(keyWord_set(color)) then color = [255,0,0]

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalZNum = totalZNum,clusPos = clusPos,selROIGroupObj = selROIGroupObj ; Susana por clusPos
 
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy

   whParam = (where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]
   if (whParam ne -1) then begin
      color = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]],$
      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]],$
      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]]  ] > [0,0,0] < [255,255,255]
   endif
   ;color = [0,255,0]

   whParam = (where(*(*self.pParamStruct).pNames eq 'Border PolygonStyle'))[0]
   if (whParam ne -1) then begin
      modelStyle = (*(*self.pParamStruct).pValues[whParam] > 0) < 2
      *(*self.pParamStruct).pValues[whParam] = modelStyle
   endif

   whParam = (where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]
   if (whParam ne -1) then begin
      borderThick = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] > 1. < 10.
                *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] = borderThick
   endif else borderThick = 1.

   whParam = (where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]
   if (whParam ne -1) then begin
      borderLineStyle = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] > 0 < 6
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] = borderLineStyle
   endif else borderLineStyle = 0

   zPos = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Position [real]'))[0]]
   zPos = 0.

   ;wTopBase viene de la ventana track
   restore, FILENAME = 'C:\RSI\trackWidgetNumber.sav'
   widget_control, wTopBase, get_uValue = trackState, /no_copy

   self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   pParamStruct = (*trackState.poCurrTrackGroup)->getpParamStruct()

   tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
   tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
   tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]

   ;TODO make it dynamic
   ;;;;;;;
   dt = 1.0
   imageSize = self->getXYZDim()
   realSize  = imageSize
   ;;;;;;;
   factor = realSize / imageSize
   roisCentroids = self->getGroupCenterXYZ()

   for i = 0L, (self->count())-1 do begin

      (self->get(position = i))->getProperty, color = color, alpha_channel = alpha
      drawTrajectoryFlag = 0b

      ;get ROI center of mass
      for j = 0L, (*trackState.poCurrTrackGroup)->getObjNum()-1 do begin
        trackStruct   = (*trackState.poCurrTrackGroup)->getTrackParam(paramName = 'Track Path', objNum = j)
        trackVect     = trackStruct.pXYZValues     
        trackVectTime = fix((*trackStruct.pTValues)/dt)
        N = size(*trackVect, /dim)
        insideInterval = where(trackVectTime eq tPos)

        if insideInterval[0] ne -1 then begin
            ;verify if this trajectory contains this ROI
            distance = sqrt(((*trackVect)[insideInterval[0],0] - roisCentroids[1,i]*factor[0])^2+$
                            ((*trackVect)[insideInterval[0],1] - roisCentroids[2,i]*factor[1])^2)
            ;print, 'distance = ' + string(distance)
            if (distance lt 1.0) then begin
              ;print, 'ROI-Trajectory association founded, distance= '+string(distance)
              drawTrajectoryFlag = 1b
              break
            endif
        endif
      endfor

      if drawTrajectoryFlag eq 1b then begin
        oObjectModel->add, obj_new('IDLgrPolyline', data = transpose(*trackVect),$
            color = color, alpha_channel = 1.0,$
            xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, thick = 2.0, $
            ;name = strCompress('2DTrackPath:' + string(i),/rem), uvalue = 'ObjInOriginalPosition')
            uValue = {name:'BaseObject', number:i})
      endif
      ;else begin
         ;print, 'ROI is not in any trajectory!'
      ;endelse
      if ((i mod 100) eq 0) then print, 'At ROI: ' + string(i) + ' of ' + string((self->count())-1)
   endfor
;   oObjectModel->add, obj_new('IDLgrPolygon', data=[[0.0, 50.0, 0.0],[5.0, 50.0, 0.0],[50.0, 0.0, 0.0]], polygons=[[2,0,1],[2,1,2],[2,2,0]], $
;                              color = [0,0,255], thick = 2.0, $
;                              xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
   widget_control, wTopBase, set_uValue = trackState, /no_copy        
end


; getoOFModel
;
; Computes OF (current and next time) and display the result
;
; History
;  2013.10.28 First version. MCerda
pro C_sROIGroupObj::getoOFModel, oObjectModel, color = color, stack_tlb = stack_tlb

  print, 'DEBUG: OF Model'
  nObj = self->count()
  if (nObj lt 1) then return
  
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
  imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, $
      stateObj_tlb = stateObj_tlb, totalTNum = totalTNum
  
  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, $
      yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  
  xyzDim = self->getxyzDim()

  if (tPos ge (totalTNum-1)) then begin
    print, 'unable to compute 2DOF for the last time frame, returning...'
    return
  endif

  image1 = make_array(xyzDim, /byte)
  image1[*,*] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
  
  image2 = make_array(xyzDim, /byte)
  image2[*,*] = imageStackInfoObject->getSelectedImage(tPos = tPos+1, chPos = chPos, zPos = zPos)

  print, 'OF 2D starting...'
  
  scales=4
  method=0  ; Method 0:HS 1:CLG 2:LK
  alpha=200
  
  opticalFlowMultiscale, image1, image2, scales, method, u, v, ALPHA =alpha, ITERATIONS = 200, verbose=1b
  
  print, 'OF 2D done.'
  
  x=findgen(xyzDim[0])
  y=findgen(xyzDim[1])
   
  xarr = x # Replicate(1, N_Elements(y))
  yarr = Replicate(1, N_Elements(x)) # y
  
  myseeds=make_array(3,xyzDim[0]*xyzDim[1])
  myseeds[0, *]=fix(xarr(*))
  myseeds[1, *]=fix(yarr(*))
  myseeds[2, *]=0
  
  w = make_array(xyzDim, /byte)
  
  opticalFlowShowOF3D, u, v, w, oObjectModel, 1, xCoord_conv = xCoord_conv, $
      yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, ZFACTOR=1.0, $
      /VECTOR, SEEDS = myseeds
   

end

; getoAACOFModel
;
; Computes OF (current and next time) and display the result
;
; History
;  2013.10.28 First version. MCerda
pro C_sROIGroupObj::getoAACOFModel, oObjectModel, color = color, stack_tlb = stack_tlb

  print, 'DEBUG: AAC+OF Model'
  nObj = self->count()
  if (nObj lt 1) then return

  widget_control, stack_tlb, get_uValue = stackState, /no_copy
  imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  
  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, $
      stateObj_tlb = stateObj_tlb, totalTNum = totalTNum
  
  self->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, $
      yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
  
  xyzDim = self->getxyzDim()

  if (tPos ge (totalTNum-1)) then begin
    print, 'unable to compute 2DOF for the last time frame, returning...'
    return
  endif

  image1 = make_array(xyzDim, /byte)
  image1[*,*] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
  
  image2 = make_array(xyzDim, /byte)
  image2[*,*] = imageStackInfoObject->getSelectedImage(tPos = tPos+1, chPos = chPos, zPos = zPos)

  print, 'OF 2D starting...'
  
  ;cambiar si el flujo es muy suave, o se estan perdiendo detalle
  alpha=200
  rho=10
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;de aqui hacia abajo, se puede mantener fijo los parametros
  iterations=500
  verbose=1b
  nScales=20
  scaleFactor=.65
  coupledMode=1b
  wFactor=1.9
  
  opticalFlowCLG, image1, image2, u, v, ALPHA = alpha, RHO = rho, SIGMA = sigma,$
                  ITERATIONS = iterations, VERBOSE = verbose, NSCALES = nScales,$
                  SCALEFACTOR = scaleFactor, COUPLEDMODE = coupledMode, WFACTOR = wFactor
                                          
  
  print, 'OF 2D done.'
  
  ;should be directly parameters
  color = [255,255,255]
  modelStyle =0
  borderThick = 1.
  borderLineStyle = 0
  zPos = 0.
  
  for i = 0L, nObj-1 do $
    (self->get(position = i))->addObjectBorderPolygon, color = color, oModel = oObjectModel, modelStyle = modelStyle, objNumber = i,$
                                                       borderThick = borderThick, borderLineStyle = borderLineStyle, zPos = zPos
                                                       
  AABModelData, oObjectModel, stack_tlb = stack_tlb, nObj=nObj, out_polygon=out_polygon
  
  ;muestra ROI (red), AAC (green), and OF projection (blue)
  for i = 0L, nObj-1 do begin
    (self->get(position = i))->getProperty, data = polygon
    ;poligono que viene del 2D Model
    pPolynomList = (self->get(position = i))->getpObjectBorderPolygonList() 
    print, 'adding object id='+string(i)
    ;poligono desde AAC
    vertices_aac=*out_polygon[i]
    
    ;TODO si el objeto tiene tuneles se debe iterar para mostrarlo
    ;mostrar poligono del ROI
    vertices=*pPolynomList[0]
    oObjectModel->add, obj_new('IDLgrPolyline', data = [[vertices[*,*]] , [vertices[*,0]]],$
        color = [255,0,0], alpha_channel = 1.0,$
        xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, thick = 2.0, $
        ;name = strCompress('2DTrackPath:' + string(i),/rem), uvalue = 'ObjInOriginalPosition')
        uValue = {name:'BaseObject', number:i})
    
    ;mostrar poligono AAC
    vertices=*pPolynomList[0]
    oObjectModel->add, obj_new('IDLgrPolyline', data = [[vertices_aac[*,*]] , [vertices_aac[*,0]]],$
        color = [0,255,0], alpha_channel = 1.0,$
        xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, thick = 2.0, $
        ;name = strCompress('2DTrackPath:' + string(i),/rem), uvalue = 'ObjInOriginalPosition')
        uValue = {name:'BaseObjectAAC', number:i})
        
    ;mover poligono de acuerdo al FO
    verx=vertices_aac[0,*]+u(fix(vertices_aac[0,*]), fix(vertices_aac[1,*]))
    very=vertices_aac[1,*]+v(fix(vertices_aac[0,*]), fix(vertices_aac[1,*]))
    verof = [verx, very]
            
    oObjectModel->add, obj_new('IDLgrPolyline', data = [[verof[*,*]] , [verof[*,0]]],$
        color = [0,0,255], alpha_channel = 1.0,$
        xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, thick = 2.0, $
        ;name = strCompress('2DTrackPath:' + string(i),/rem), uvalue = 'ObjInOriginalPosition')
        uValue = {name:'DeformedObject', number:i})
        
  endfor
  
end


; auxiliar function to get the AABModel only (no graphic output)
;
; Uses Active Adjacent Boundary Model a.k.a. Adjacent Active Contours
; See C_sAABContainer__Define.pro
;
; History
;  2013.10.28 First version. Mcerda (from JJara's version)
pro AABModelData, oObjectModel, stack_tlb = stack_tlb, nObj=nObj, out_polygon=out_polygon

  print, 'WARNING: THIS METHOD IS UNDER DEVELOPMENT!'
  if (nObj lt 1) then return

  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
    imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)

  fDoVectorFieldCalculation = 0b ; Can force or avoid vector field calculation
  fDoContourAdjustment      = 0b ; Option to avoid contour adjustment.
  fUseAABcontourColorCode   = 1b ; Option to use AABM color codes for contour vertices (see documentation in C_sAABContainer__Define).

  aabmAlpha = 0.05
  aabmBeta  = 0.05
  aabmGamma = 1.0
  aabmKappa = 1.0
  aabmContourMaxIterations    = 300
  aabmContourConvergenceLimit = 0.01
  aabmPerimeterFactor         = 2.0

  aabmVFtype = 'EP-GVF'; 'GVF', 'GGVF' or 'EP-GVF'
  aabmVFmaxIterations      = 300
  aabmVFconvergenceLimit   = 0.01
  aabmVFmu                 = 0.1
  aabmEPGVFnoiseCut        = 0.1
  aabmEPGVFnoiseRange      = 0.2
  ;aabmEPGVFnumericalStep   = 1 ; optional parameter, could be left undefined
  aambEPGVFsigmaGaussianBlur = 0 ; 0-> no blur before vector field computation
  aabmFverbose               = 1b ; Set verbose mode ON while doing my PhD thesis ;)
  aabmFnoInterpolateAtFirst  = 0b ; 1-> Avoid interpolation while setting contours
  aabmMinContourPointCount  = 16
  aabmPointSamplingDistance = 1.0
  aabmProximityDistance     = 2.0 ; In pixels

  oAABM = obj_new('C_sAABContainer',$
                  image,$
                  alphaVal = aabmAlpha,$
                  betaVal  = aabmBeta,$
                  gammaVal = aabmGamma,$
                  kappaVal = aabmKappa,$
                  proximityDist           = aabmProximityDistance,$
                  contourMaxIterations    = aabmContourMaxIterations,$
                  vfMaxIterations         = aabmVFmaxIterations,$
                  contourSamplingDistance = aabmPointSamplingDistance,$
                  fVerbose                = aabmFverbose)
  if ~obj_valid(oAABM) then return

    ; Perform image vector field calculation (if fDoVectorFieldCalculation is non-0).
  if fDoVectorFieldCalculation then $
  fVFok = oAABM->calcVectorField(type = aabmVFtype, mu = aabmVFmu, iterations = aabmVFmaxIterations, convergenceLimit = aabmVFconvergenceLimit)

  ; Someday a special case for processing a different number of contours could be used. So...
  nObj2 = oObjectModel->Count()
  if (nObj2 lt 1) then begin
    obj_destroy, oAABM
    return
  endif

  fSet = oAABM->preSetNumContours(nObj2)
  for i = 0L, nObj2-1 do begin
    (oObjectModel->get(position = i))->getProperty, data = polygon
    fContourAdded = oAABM->addContour(transpose(polygon[0,*]), transpose(polygon[1,*]), i, FNOINTERPOLATE = aabmFnoInterpolateAtFirst)
  endfor

    ; Perform contour adjustment (if fDoContourAdjustment is non-0).
  fAABMsuccess = fDoContourAdjustment ? oAABM->adjustContour() : 1b
  if (fAABMsuccess ne 1) then begin
    print, 'Something went wrong while adjusting the contours. Nothing to show :('
    obj_destroy, oAABM
    return
  endif

    ; Finally, get adjusted contours and add graphic objects.
  if fUseAABcontourColorCode then begin
    fCalc        = oAABM->computeAABcontourVertexTypes()
    aabmContours = oAABM->getContours(nContours = nObjOut, pAABMcontourColors = pAABMcontourColors)
  endif else begin
    aabmContours = oAABM->getContours(nContours = nObjOut)
  endelse

  fUseAABcontourColorCode = fUseAABcontourColorCode and (n_elements(pAABMcontourColors) gt 0)
  if fUseAABcontourColorCode then print, 'Using AABM contour colors for rendering'

  if (nObjOut lt 1) then print, 'No contours were retrieved from oAABM!' $
  else begin
    out_polygon=ptrarr(nObjOut)
    for i = 0L, nObjOut-1 do begin
        out_polygon[i]=ptr_new(0)
        *out_polygon[i]=transpose([[*((aabmContours.px)[i])], [*((aabmContours.py)[i])]])
    end
  endelse

  obj_destroy, oAABM

end

;_END_GET_MODEL_OBJECT_FUNCTIONS_____________________________________________________________________


;_BEGIN_OBJECT_PARAMETER_FUNCTIONS_____________________________________________________________________
function C_sROIGroupObj::getParamContainer
    return, self.oParamContainer
end

pro C_sROIGroupObj::setParamContainer, object = object
    if obj_valid(self.oParamContainer) then begin
       oParams = (self.oParamContainer)->get(/all)
       (self.oParamContainer)->remove, /all
       if obj_valid(oParams[0]) then for i = 0, n_elements(oParams)-1 do obj_destroy, oParams[i]

       if obj_valid(object) then begin
         oParams = object->get(/all)
         object->remove, /all
         obj_destroy, object
         if obj_valid(oParams[0]) then for i = 0, n_elements(oParams)-1 do (self.oParamContainer)->add, oParams[i], position = i
       endif
    endif else if obj_valid(object) then self.oParamContainer = object
end

function C_sROIGroupObj::getParamContainerObj, active = active
    if (active gt (self.oParamContainer->count()-1)) then active = self.oParamContainer->count() - 1
    return, self.oParamContainer->get(position = active)
end

pro C_sROIGroupObj::addParamContainerObj, object = object
    if keyWord_set(object) then self.oParamContainer->add, object
end

pro C_sROIGroupObj::deleteParamContainerObj, active = active
    if (active gt (self.oSegContainer->count())) then active = (self.oSegContainer->count())-1
    self.oParamContainer->remove, position = active
end

function C_sROIGroupObj::getSelectedROIObjFromParamName, selROIParamName = selROIParamName, position = position
    if (n_elements(selROIParamName) gt 0) then $
       for i = 0, self.oParamContainer->count()-1 do begin
         oROIParam = self.oParamContainer->get(position = i)
         if ((*(oROIParam->getpParamStruct())).name eq selROIParamName) then return, oROIParam
         position = (where( *(*(oROIParam->getpParamStruct())).pNames eq selROIParamName, count))[0]
         if (count gt 0) then return, oROIParam
       endfor
    return, -1
end

function C_sROIGroupObj::getGroupMaskInNumberOrder, selROIParamName = selROIParamName
    mask = intArr( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] )
    for i = 0, self->count()-1 do $
       mask[ *( (self->get(position = i))->getpWherePoints()) ] = i+1
    return, mask
end

function C_sROIGroupObj::getGroupMaskInParamValOrder, selROIParamName = selROIParamName
    mask = intArr( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] )
    oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = position)
    if obj_valid(oROIParam) then begin
       pValueStruct = oROIParam->getpValueStruct(position = position)
       maxParam = max(*(*pValueStruct).pROIParamVect, min = minParam)
       for i = 0, self->count()-1 do begin
         whObj = where (*(*(oROIParam->getpParamStruct())).pROINumberVect eq (self->get(position = i))->getNumber() )
         if (total(whObj) ge 0) then $
          mask[ *( (self->get(position = i))->getpWherePoints()) ] = ((*(*pValueStruct).pROIParamVect)[whObj] - minParam) * (205./(maxParam-minParam)) + 46
       endfor
    endif
    return, mask
end


function C_sROIGroupObj::getGroupMaskInExactParamValOrder, selROIParamName = selROIParamName
    oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = position)
    if obj_valid(oROIParam) then begin
       pValueStruct = oROIParam->getpValueStruct(position = position)
       maxParam = max(*(*pValueStruct).pROIParamVect, min = minParam)
       mask = make_array( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                          *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]], /int)
       for i = 0, self->count()-1 do begin
          whObj = where(*(*(oROIParam->getpParamStruct())).pROINumberVect eq (self->get(position = i))->getNumber(), count)
          if (count ge 0) then mask[ *((self->get(position = i))->getpWherePoints()) ] = (*(*pValueStruct).pROIParamVect)[whObj]
       endfor
    endif
    if (n_elements(mask) gt 0) then return, mask else return, bytArr( *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                                                                      *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] )
end


pro C_sROIGroupObj::setGroupObjColorsInParamValOrder, selROIParamName = selROIParamName,$
                                                      ROIObjParamPos = ROIObjParamPos,$
                                                      ROIObjSubParamPos = ROIObjSubParamPos,$
                                                      pWhereThreshArr = pWhereThreshArr,$
                                                      pColorArr = pColorArr

    if (n_elements(selROIParamName) gt 0) then $
       oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = ROIObjSubParamPos) else $
       oROIParam = self.oParamContainer->get(position = ROIObjParamPos)

    if obj_valid(oROIParam) then begin
       pValueStruct = oROIParam->getpValueStruct(position = ROIObjSubParamPos)
       maxParam = max(*(*pValueStruct).pROIParamVect, min = minParam)
       fParam = (minParam eq -1) and (maxParam eq -1)
       if (maxParam eq minParam) then begin
         minParam = 0
         maxParam = 1
       endif
       if (fParam) then begin
         for i = 0, self->count()-1 do begin
          objNumber = (self->get(position = i))->getNumber()
          whObj = (where (*(*(oROIParam->getpParamStruct())).pROINumberVect eq objNumber ))[0]
          if (whObj ne -1) then (self->get(position = i))->setProperty, color = [200, 200, 200]
         endfor
       endif else begin
         if (n_elements(pWhereThreshArr) le 0) then begin
          for i = 0, self->count()-1 do begin
              objNumber = (self->get(position = i))->getNumber()
              whObj = (where (*(*(oROIParam->getpParamStruct())).pROINumberVect eq objNumber ))[0]
              if (whObj ne -1) then $
                 (self->get(position = i))->setProperty, color = ([255, 255, 255] * (((*(*pValueStruct).pROIParamVect)[whObj] - minParam) * (.65/(maxParam-minParam)) + .35)[0])
          endfor
         endif else begin
          whObjInThresh_1 = -1
          whObjInThresh_2 = -1
          whObjInThresh_3 = -1
          whObjInThresh_4 = -1
          for i = 0, self->count()-1 do begin
              objNumber = (self->get(position = i))->getNumber()
              pParamStruct = *(oROIParam->getpParamStruct())
              whObj = (where (*(pParamStruct).pROINumberVect eq objNumber ))[0]
              if (whObj ne -1) then begin
                 if ((*(pWhereThreshArr)[0])[0] ne -1) then whObjInThresh_1 = ( (where ( (*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[0]] eq objNumber ))[0] ne -1)
                 if ((*(pWhereThreshArr)[1])[0] ne -1) then whObjInThresh_2 = ( (where ( (*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[1]] eq objNumber ))[0] ne -1)
                 if ((*(pWhereThreshArr)[2])[0] ne -1) then whObjInThresh_3 = ( (where ( (*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[2]] eq objNumber ))[0] ne -1)
                 if ((*(pWhereThreshArr)[3])[0] ne -1) then whObjInThresh_4 = ( (where ( (*(pParamStruct).pROINumberVect)[*(pWhereThreshArr)[3]] eq objNumber ))[0] ne -1)

                 case 1 of
                   whObjInThresh_4: (self->get(position = i))->setProperty, color = *(pColorArr)[3]
                   whObjInThresh_3: (self->get(position = i))->setProperty, color = *(pColorArr)[2]
                   whObjInThresh_2: (self->get(position = i))->setProperty, color = *(pColorArr)[1]
                   whObjInThresh_1: (self->get(position = i))->setProperty, color = *(pColorArr)[0]
                   else: (self->get(position = i))->setProperty, color = ([255, 255, 255] * (((*(*pValueStruct).pROIParamVect)[whObj] - minParam) * (.75/(maxParam-minParam)) + .25)[0])
                 endcase

              endif
          endfor
         endelse
       endelse
    endif
end


pro C_sROIGroupObj::selectObjectsInThresholdIntervals, selROIParamName = selROIParamName
   oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName, position = position)
   oROIParam->applyThresholdFilter, position = position

   pParamStruct = oROIParam->getpParamStruct()
   for i = self->count()-1,0,-1 do begin
      objNumber = (self->get(position = i))->getNumber()
      whObj = (where (*(*pParamStruct).pROINumberVect eq objNumber))[0]
      if (whObj eq -1) then obj_destroy, self->get(position = i)
   endfor
end


pro C_sROIGroupObj::complementGroupParameters, oParamContainerReference = oParamContainerReference
    self->setActiveGroupParameterNameList
    count = oParamContainerReference->count()-1
    if ptr_valid(self.pActiveGroupParameterNameList) then begin
       if (n_elements(*self.pActiveGroupParameterNameList) lt 1) then *self.pActiveGroupParameterNameList = 'XXX'
    endif else self.pActiveGroupParameterNameList = ptr_new('XXX', /no_copy)
    for i = 0, count do begin
       oROIParamReference = oParamContainerReference->get(position = i)
       whParam = (where(*self.pActiveGroupParameterNameList eq ((*(oROIParamReference->getpParamStruct())).Name)))[0]
       if (whParam eq -1) then begin
         save, oROIParamReference, filename = s_getPathForSystem()+'obj.tmp'
         restore, s_getPathForSystem()+'obj.tmp', restored_objects = oROIParam, /relaxed
         oROIParam = oROIParam[0]
         self.oParamContainer->add, oROIParam
         self->setActiveGroupParameterNameList
         self->calculateGroupParameters, selROIParamName = ((*(oROIParam->getpParamStruct())).Name)
       endif else begin
         oROIParam = self.oParamContainer->get(position = whParam)
         if (total((*(*(oROIParam->getpParamStruct())).pActive) ne (*(*(oROIParamReference->getpParamStruct())).pActive)) gt 0) then begin
          *(*(oROIParam->getpParamStruct())).pActive = (*(*(oROIParam->getpParamStruct())).pActive) > (*(*(oROIParamReference->getpParamStruct())).pActive)
          self->calculateGroupParameters, selROIParamName = ((*(oROIParam->getpParamStruct())).Name)
         endif
       endelse
    endfor
end


pro C_sROIGroupObj::setParamFilters, oParamContainerReference = oParamContainerReference
    self->setActiveGroupParameterNameList
    count = oParamContainerReference->count()-1
    if ptr_valid(self.pActiveGroupParameterNameList) then begin
       if (n_elements(*self.pActiveGroupParameterNameList) lt 1) then *self.pActiveGroupParameterNameList = 'XXX'
    endif else self.pActiveGroupParameterNameList = ptr_new('XXX', /no_copy)
    for i = 0, count do begin
       oROIParamReference = oParamContainerReference->get(position = i)
       if (i eq 0) then referenceNameList = (*(oROIParamReference->getpParamStruct())).Name else  referenceNameList = [referenceNameList, (*(oROIParamReference->getpParamStruct())).Name]
       if (ptr_valid(self.pActiveGroupParameterNameList)) then whParam = where(*self.pActiveGroupParameterNameList eq referenceNameList[i]) else whParam = -1
       if (whParam[0] ne -1) then begin
         if (n_elements(whParam) gt 1) then begin
          if (n_elements(where (referenceNameList[i] eq referenceNameList)) eq 1) then whParam = whParam[0] else whParam = whParam[n_elements(where (referenceNameList[i] eq referenceNameList))-1]
         endif
         (self.oParamContainer->get(position = whParam))->setThresholdFilters, oROIParamReference = oParamContainerReference->get(position = i)
       endif
    endfor
end


pro C_sROIGroupObj::calculateGroupParameters, mask = mask, selROIParamName = selROIParamName, position = position, stack_tlb = stack_tlb

    count = self.oParamContainer->count()-1
    if (n_elements(selROIParamName) gt 0) then begin
       oROIParam = self->getSelectedROIObjFromParamName(selROIParamName = selROIParamName)
       count = 0
    endif

    xySizePerPixel = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [real]'))[0]] * 1. / $
                       *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                       *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [real]'))[0]] * 1. / $
                       *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]]  ]

    for i = 0, count do begin
       if (i eq 0) then begin
          if not(obj_valid(oROIParam)) then oROIParam = self.oParamContainer->get(position = 0)
       endif else oROIParam = self.oParamContainer->get(position = i)
       case (*(oROIParam->getpParamStruct())).name of
         'Object Number in Cluster': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              if (n_elements(position) le 0) then oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, /group else $
                 oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, /group
          endcase
         'Object Perimeter': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              if (n_elements(position) le 0) then oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel else $
                 oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position
          endcase
         'Object Time': oROIParam->apply, time = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time After Start [s]'))[0]],$
                                          zSlize = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Position'))[0]],$
                                          C_sROI2DGroupObj = self, position = position
         'Group Object Number': oROIParam->apply, number = self->count()-1
         'Object Track Objects': oROIParam->apply, position = position, stack_tlb = stack_tlb, C_sROI2DGroupObj = self
         'Object Intensity Ratio': oROIParam->apply, position = position, stack_tlb = stack_tlb, C_sROIGroupObj = self
         'Inter-Object Center Distance': oROIParam->apply, centerCoordinates = self->C_sROIGroupObj::getGroupCenterXYZ(), xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = self
         'Inter-Object Border Distance': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, C_sROIGroupObj = self
          endcase
         'Inter-Object Border Neighbour': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              oROIParam->apply, stack_tlb = stack_tlb, mask = mask, xySizePerPixel = xySizePerPixel, C_sROIGroupObj = self
          endcase
         'Inter-Object Dipole Interactions': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, C_sROIGroupObj = self, stack_tlb = stack_tlb
          endcase
         'Group Phase': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              if (n_elements(position) le 0) then oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel else $
                 oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position
          endcase
         'Object Colocalization': begin
              if (n_elements(position) le 0) then oROIParam->apply, C_sROIGroupObj = self, stack_tlb = stack_tlb else $
                 oROIParam->apply, C_sROIGroupObj = self, position = position, stack_tlb = stack_tlb
          endcase
         'Object AC-Parameters': begin
              if (n_elements(position) le 0) then oROIParam->apply, C_sROIGroupObj = self, stack_tlb = stack_tlb else $
                 oROIParam->apply, C_sROIGroupObj = self, position = position, stack_tlb = stack_tlb
          endcase
         'AABM Descriptors': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
              widget_control, stateObj_tlb, get_uValue = state, /no_copy
                roiGroupFilePathName = state.currROI2DGroupFileName
              widget_control, stateObj_tlb, set_uValue = state, /no_copy
              pathSep = path_sep()
              roiGroupFilePathNameBase = strMid(roiGroupFilePathName, 0, strPos(roiGroupFilePathName, '.', /reverse_search))
              if (n_elements(position) le 0) $
              then oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = self, stack_tlb = stack_tlb, baseFileName = roiGroupFilePathNameBase $
              else oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, C_sROIGroupObj = self, stack_tlb = stack_tlb, baseFileName = roiGroupFilePathNameBase
         endcase
         'AABM Quality Indicators': begin
              if (n_elements(mask) eq 0) then mask = self->C_sROIGroupObj::getGroupMask()
              s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
              widget_control, stateObj_tlb, get_uValue = state, /no_copy
                roiGroupFilePathName = state.currROI2DGroupFileName
              widget_control, stateObj_tlb, set_uValue = state, /no_copy
              pathSep = path_sep()
              roiGroupFilePathNameBase = strMid(roiGroupFilePathName, 0, strPos(roiGroupFilePathName, '.', /reverse_search))

              if (n_elements(position) le 0) $
              then oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = self, stack_tlb = stack_tlb, baseFileName = roiGroupFilePathNameBase $
              else oROIParam->apply, mask = mask, xySizePerPixel = xySizePerPixel, C_sROIGroupObj = self, stack_tlb = stack_tlb, baseFileName = roiGroupFilePathNameBase
         endcase
         'Object OF-AC Test': begin
              if (n_elements(position) le 0) then oROIParam->apply, C_sROIGroupObj = self, stack_tlb = stack_tlb else $
                 oROIParam->apply, C_sROIGroupObj = self, position = position, stack_tlb = stack_tlb
          endcase
         'Object Optical Flow': begin
              if (n_elements(position) le 0) then oROIParam->apply, C_sROIGroupObj = self, stack_tlb = stack_tlb else $
                 oROIParam->apply, C_sROIGroupObj = self, position = position, stack_tlb = stack_tlb
          endcase
          'Object Track Parameters': begin
              if (n_elements(position) le 0) then oROIParam->apply, C_sROIGroupObj = self, stack_tlb = stack_tlb else $
                 oROIParam->apply, C_sROIGroupObj = self, position = position, stack_tlb = stack_tlb
          endcase
         else: if (n_elements(position) le 0) then oROIParam->apply, C_sROIGroupObj = self else $
                  oROIParam->apply, C_sROIGroupObj = self, position = position
       endcase
    endfor
end


function C_sROIGroupObj::getParameterNameList
   return, ['Name',$
             'Time Position',$
             'Channel Position',$
             'z-Slice Position',$
             'Total Number of z-Slices',$
             'z-Interval [real]',$
             'z-Position [real]',$
             'Time After Start [s]',$
             'x-Size [pixel]',$
             'y-Size [pixel]',$
             'x-Size [real]',$
             'y-Size [real]',$
             'ShowBox in Original Picture [x0]',$
             'ShowBox in Original Picture [x1]',$
             'ShowBox in Original Picture [y0]',$
             'ShowBox in Original Picture [y1]',$
             'ShowBox in Original Picture [z0]',$
             'ShowBox in Original Picture [z1]',$
             'ROI-Group ShowBox xSize [pixel]',$
             'ROI-Group ShowBox ySize [pixel]',$
             'ROI-Group ShowBox xSize [real]',$
             'ROI-Group ShowBox ySize [real]',$
             'Comments',$
             'Hide On/Off',$
             'Image Object',$
             'Image Color',$
             'Image Opacity',$
             'Border Object',$
             'Border PolygonStyle',$
             'Border Color_r',$
             'Border Color_g',$
             'Border Color_b',$
             'Border Thick',$
             'Border LineStyle',$
             'ROI Object',$
             'ROI Color_r',$
             'ROI Color_g',$
             'ROI Color_b',$
             'ROI Style',$
             'ROI Thick',$
             'ROI LineStyle',$
             'ROI Hide' ]
end


function C_sROIGroupObj::init, name = name,$
                                  tPos = tPos,$
                                  chPos = chPos,$
                                  zPos = zPos,$
                                  ZSliceInterval = ZSliceInterval,$
                                  ZSliceReal = ZSliceReal,$
                                  timeAfterStart = timeAfterStart,$
                                  xyzPixSize = xyzPixSize,$            ;    xyzPixSize: 2D- or 3D-embedding Object-Space
                                  frameRealSize = frameRealSize,$              ;  FramePixSize: 2D- or 3D-embedding Object-Space
                                  xyzShowBoxPosition = xyzShowBoxPosition,$
                                  ROIGroupShowBoxPixSize = ROIGroupShowBoxPixSize,$
                                  hideOnOff = hideOnOff

    parameterNameList = self->getParameterNameList()
    paramStruct = {ROIGroupParamStruct,$
                   pValues : ptrArr(n_elements(parameterNameList), /allocate),$       ; Pointer on Filter Parameter Values.
                   pNames : ptr_new(parameterNameList, /no_copy)  }     ; Pointer on Filter Parameter Names.

    if (n_elements(name) eq 0) then name = '-NO NAME-'
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Name'))[0]] = name
    if (n_elements(tPos) eq 0) then tPos = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Time Position'))[0]] = tPos
    if (n_elements(chPos) eq 0) then chPos = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Channel Position'))[0]] = chPos
    if (n_elements(zPos) eq 0) then zPos = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Slice Position'))[0]] = zPos
    if (n_elements(ZSliceInterval) eq 0) then ZSliceInterval = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Interval [real]'))[0]] = ZSliceInterval
    if (n_elements(ZSliceReal) eq 0) then ZSliceReal = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Position [real]'))[0]] = ZSliceReal

    if (n_elements(timeAfterStart) eq 0) then timeAfterStart = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Time After Start [s]'))[0]] = timeAfterStart

    if (n_elements(xyzPixSize) eq 0) then xyzPixSize = [-1,-1,-1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [pixel]'))[0]] = xyzPixSize[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [pixel]'))[0]] = xyzPixSize[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Total Number of z-Slices'))[0]] = xyzPixSize[2]

    if (n_elements(frameRealSize) eq 0) then frameRealSize = xyzPixSize $
       else if ( (frameRealSize[0] le 0) or (frameRealSize[1] le 0) ) then frameRealSize = xyzPixSize
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [real]'))[0]] = float(frameRealSize[0])
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [real]'))[0]] = float(frameRealSize[1])

    if (n_elements(xyzShowBoxPosition) eq 0) then xyzShowBoxPosition = [0, xyzPixSize[0]-1, 0, xyzPixSize[1]-1, 0, xyzPixSize[2]-1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ShowBox in Original Picture [x0]'))[0]] = xyzShowBoxPosition[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ShowBox in Original Picture [x1]'))[0]] = xyzShowBoxPosition[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ShowBox in Original Picture [y0]'))[0]] = xyzShowBoxPosition[2]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ShowBox in Original Picture [y1]'))[0]] = xyzShowBoxPosition[3]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ShowBox in Original Picture [z0]'))[0]] = xyzShowBoxPosition[4]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ShowBox in Original Picture [z1]'))[0]] = xyzShowBoxPosition[5]
    if (n_elements(ROIGroupShowBoxPixSize) eq 0) then ROIGroupShowBoxPixSize = [xyzShowBoxPosition[1]-xyzShowBoxPosition[0]+1,xyzShowBoxPosition[3]-xyzShowBoxPosition[2]+1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]] = ROIGroupShowBoxPixSize[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] = ROIGroupShowBoxPixSize[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Comments'))[0]] = '007'
    if (n_elements(hideOnOff) eq 0) then hideOnOff = 0b
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Hide On/Off'))[0]] = hideOnOff

    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Image Object'))[0]] = 'CONTAINED'
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Image Color'))[0]] = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Image Opacity'))[0]] = 255

    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Object'))[0]] = 'CONTAINED'
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border PolygonStyle'))[0]] = 1
    if (n_elements(borderColor) eq 0) then borderColor = [255,0,0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_r'))[0]] = borderColor[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_g'))[0]] = borderColor[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_b'))[0]] = borderColor[2]
    if (n_elements(borderThick) eq 0) then borderThick = 1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Thick'))[0]] = borderThick
    if (n_elements(borderLineStyle) eq 0) then borderLineStyle = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border LineStyle'))[0]] = borderLineStyle
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Object'))[0]] = 'CONTAINED'
    if (n_elements(ROIColor) eq 0) then ROIColor = [255,0,0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Color_r'))[0]] = ROIColor[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Color_g'))[0]] = ROIColor[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Color_b'))[0]] = ROIColor[2]
    if (n_elements(ROIStyle) eq 0) then ROIStyle = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Style'))[0]] = ROIStyle
    if (n_elements(ROIThick) eq 0) then ROIThick = 1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Thick'))[0]] = ROIThick
    if (n_elements(ROILineStyle) eq 0) then ROILineStyle = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI LineStyle'))[0]] = ROILineStyle
    if (n_elements(ROIHide) eq 0) then ROIHide = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Hide'))[0]] = ROIHide

    self.pParamStruct = ptr_new(paramStruct, /no_copy)
    self->setRealROIGroupShowBoxSize
    self->setIDLgrROIGroupXYZCoord
    self->setProperty, name = name

    self.oParamContainer = obj_new('IDL_Container')
    return, 1
end


pro C_sROIGroupObj__define
   tmp = {C_sROIGroupObj, pParamStruct: ptr_new(),$
                          pActiveGroupParameterNameList: ptr_new(),$
                          oParamContainer:  obj_new(),$
                          inherits IDLgrROIGroup}
end
