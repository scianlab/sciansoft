;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjBoxModel
;
; PURPOSE:
;       - Calculation of 3D Object Box Model
;
; AUTHOR:
;     Dr. Steffen Härtel (2007)
;     e_mail:shartel@med.uchile.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjBoxModel' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_3DObjBoxModel::apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position
      ; check if surface models exist. If not then take original ROIs.
      fModelSelected = 0
      fSurf = 0
      s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
      widget_control, stateObj_tlb, get_uValue = stateObjWindow, /no_copy
        if(OBJ_VALID(stateObjWindow)) then begin
           oObjectModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface AC Model')
           if not(obj_valid(oObjectModel)) then begin
              oObjectModel = *(stateObjWindow.poCurrROIGraphicModel)->getByName('3D Surface Mesh Model')
              fModelSelected=3
           endif else fModelSelected = 1; AC model
         endif
      widget_control, stateObj_tlb, set_uValue = stateObjWindow, /no_copy
      if obj_valid(oObjectModel) then fSurf = 1b else fSurf = 0b
      if fSurf then begin
         for i = oObjectModel->count()-1,0,-1 do obj_destroy, oObjectModel->get(position=i)
         oObjectModel->remove, /all
         obj_destroy, oObjectModel
         
         if(fModelSelected eq 1) then begin
              oSurfModel = obj_new('IDLgrModel', uValue = '3D Surface AC Model', name = '3D Surface AC Model')
              C_sROI3DGroupObj->getoACModel, oSurfModel, stack_tlb = stack_tlb
         endif
         if(fModelSelected eq 2) then begin      
              print, '3D Surface Model is no longer supported'
         endif
         if(fModelSelected eq 3) then begin      
              oSurfModel = obj_new('IDLgrModel', uValue = '3D Surface Mesh Model', name = '3D Surface Mesh Model')
              C_sROI3DGroupObj->getoMeshModel, oSurfModel
         endif
      endif

   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do  whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

       ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1:if (position[0] eq -1) then return else  whParamActive[position] = 1
      else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase

      ; check Pointers
   wherePA = where(whParamActive eq 1)
   if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

   nObjects = C_sROI3DGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

        ; set Object Number Vector
      (*self.pParamStruct).pROINumberVect = ptr_new(C_sROI3DGroupObj->getObjectNumberVector(), /no_copy)

        ; set Object Parameter Vectors
      for i = 0, n_elements(whParam)-1 do if (whParamActive[i]) then *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float, value = -1.)

      fCalcBack = 0b
      if fSurf then begin
         oObj = oSurfModel->get(position = 0)
         if obj_valid(oObj) then begin
            oObj->getProperty, uvalue = uvalue
            if (uvalue eq 'ObjInGalleryPosition') then begin
               C_sROI3DGroupObj->calcObjPos, oSurfModel, sDirection = 'calcOriginalPosition'
               fCalcBack = 1b
            endif
         endif
      endif

      xyzSizePerPixel = C_sROI3DGroupObj->getxyzSizePerPixel()
      if (whParamActive[10] or whParamActive[11]) then begin
         xyzDIM = C_sROI3DGroupObj->getxyzDIM()
         xyzVol = make_array(xyzDIM, /byte)
         pBoxPos = ptrArr(nObjects)
      endif

      for i = 0, nObjects-1 do begin
         fCOOk = 0b
         if fSurf then begin
            oObj = oSurfModel->get(position = i)
            if obj_valid(oObj) then begin
               oObj->getProperty, data = xyzPoints, poly = polygons
               fCOOk = 1b
            endif
         endif

            ; get BOX Coordinates
         if fCOOk then evProj = (C_sROI3DGroupObj->get(position = i))->getEVSProj(xyzPoints = xyzPoints) else evProj = (C_sROI3DGroupObj->get(position = i))->getEVSProj()

         if (whParamActive[10] or whParamActive[11]) then begin
            vec = [(evProj.sqProj)[0,4]-(evProj.sqProj)[0,0],(evProj.sqProj)[1,4]-(evProj.sqProj)[1,0],(evProj.sqProj)[2,4]-(evProj.sqProj)[2,0]]
            szVecA = round((max(abs(vec))*3) > 5)
            vecA = make_array([3,szVecA], /float)
            stepA = vec / (szVecA-1)
            vecA[0,*] = make_array(szVecA, /float, /index) * stepA[0] + (evProj.sqProj)[0,0]
            vecA[1,*] = make_array(szVecA, /float, /index) * stepA[1] + (evProj.sqProj)[1,0]
            vecA[2,*] = make_array(szVecA, /float, /index) * stepA[2] + (evProj.sqProj)[2,0]

            vec = [(evProj.sqProj)[0,1]-(evProj.sqProj)[0,0],(evProj.sqProj)[1,1]-(evProj.sqProj)[1,0],(evProj.sqProj)[2,1]-(evProj.sqProj)[2,0]]
            szVecB = round((max(abs(vec))*3) > 5)
            vecB = make_array([3,szVecB], /float)
            stepB = vec / (szVecB-1)
            vecB[0,*] = make_array(szVecB, /float, /index) * stepB[0] + (evProj.sqProj)[0,0]
            vecB[1,*] = make_array(szVecB, /float, /index) * stepB[1] + (evProj.sqProj)[1,0]
            vecB[2,*] = make_array(szVecB, /float, /index) * stepB[2] + (evProj.sqProj)[2,0]

            vec = [(evProj.sqProj)[0,3]-(evProj.sqProj)[0,0],(evProj.sqProj)[1,3]-(evProj.sqProj)[1,0],(evProj.sqProj)[2,3]-(evProj.sqProj)[2,0]]
            szVecC = round((max(abs(vec))*3) > 5)
            vecC = make_array([3,szVecC], /float)
            stepC = vec / (szVecC-1)
            vecC[0,*] = make_array(szVecC, /float, /index) * stepC[0] + (evProj.sqProj)[0,0]
            vecC[1,*] = make_array(szVecC, /float, /index) * stepC[1] + (evProj.sqProj)[1,0]
            vecC[2,*] = make_array(szVecC, /float, /index) * stepC[2] + (evProj.sqProj)[2,0]

            for m = 0, (size(vecC, /dim))[1]-1 do begin
               for l = 0, (size(vecB, /dim))[1]-1 do begin
                  pos = round(vecA[0,*] + m * stepC[0] + l * stepB[0]) + $
                        round(vecA[1,*] + m * stepC[1] + l * stepB[1]) * xyzDIM[0] + $
                        round(vecA[2,*] + m * stepC[2] + l * stepB[2]) * xyzDIM[0] * xyzDIM[1]
                  xyzVol[pos] = 1
               endfor
            endfor
            pBoxPos[i] = ptr_new(where(xyzVol), /no_copy)
            xyzVol[*] = 0
         endif

            ; transform box and axis xyzcoordinates into real coordinate system
         evProj.evMinMax[0,*] *= xyzSizePerPixel[0]
         evProj.evMinMax[1,*] *= xyzSizePerPixel[1]
         evProj.evMinMax[2,*] *= xyzSizePerPixel[2]
         evProj.sqProj[0,*] *= xyzSizePerPixel[0]
         evProj.sqProj[1,*] *= xyzSizePerPixel[1]
         evProj.sqProj[2,*] *= xyzSizePerPixel[2]

         evLength = [sqrt( ((evProj.evMinMax)[0,1]-(evProj.evMinMax)[0,0])^2 + ((evProj.evMinMax)[1,1]-(evProj.evMinMax)[1,0])^2 + ((evProj.evMinMax)[2,1]-(evProj.evMinMax)[2,0])^2 ),$
                     sqrt( ((evProj.evMinMax)[0,3]-(evProj.evMinMax)[0,2])^2 + ((evProj.evMinMax)[1,3]-(evProj.evMinMax)[1,2])^2 + ((evProj.evMinMax)[2,3]-(evProj.evMinMax)[2,2])^2 ),$
                     sqrt( ((evProj.evMinMax)[0,5]-(evProj.evMinMax)[0,4])^2 + ((evProj.evMinMax)[1,5]-(evProj.evMinMax)[1,4])^2 + ((evProj.evMinMax)[2,5]-(evProj.evMinMax)[2,4])^2 ) ]

            ; '3D Box Volume [x³]'
         if whParamActive[0] then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[i] = evLength[0] * evLength[1] * evLength[2]

            ; '3D Vol div Box Vol'
         if whParamActive[1] then if ((evLength[0] * evLength[1] * evLength[2]) gt 0) then (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = $
            (n_elements(*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())) * (xyzSizePerPixel[0] * xyzSizePerPixel[1] * xyzSizePerPixel[2])) / (evLength[0] * evLength[1] * evLength[2]) $
            else (*(*(*self.pValueStruct)[whParam[1]]).pROIParamVect)[i] = 0.

            ; '3D Box Surface [x²]'
         if whParamActive[2] then (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect)[i] = 2*evLength[0]*evLength[1] + 2*evLength[1]*evLength[2] + 2*evLength[0]*evLength[2]

            ; '3D Box Sphericity' -> http://en.wikipedia.org/wiki/Sphericity
         if whParamActive[3] then begin
            V = evLength[0] * evLength[1] * evLength[2]
            S = 2*evLength[0]*evLength[1] + 2*evLength[1]*evLength[2] + 2*evLength[0]*evLength[2]
            if (S ne 0) then (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[i] = (!pi)^(1./3) * (6.*V)^(2./3) / S $
               else (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect)[i] = 0
         endif

            ; '3D Box Length [x]'
         if whParamActive[4] then (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)[i] = evLength[0]

            ; '3D Box Width [x]'
         if whParamActive[5] then (*(*(*self.pValueStruct)[whParam[5]]).pROIParamVect)[i] = evLength[1]

            ; '3D Box Height [x]'
         if whParamActive[6] then (*(*(*self.pValueStruct)[whParam[6]]).pROIParamVect)[i] = evLength[2]

            ; '3D Box Elongation'
         if whParamActive[7] then if (evLength[0] gt 0) then (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[i] = 1. - (evLength[1]/evLength[0]) else (*(*(*self.pValueStruct)[whParam[7]]).pROIParamVect)[i] = 1.

            ; '3D Relative Elongation'
         if whParamActive[8] then if (evLength[0] gt 0) then (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[i] = 1. - (evLength[2]/evLength[0]) else (*(*(*self.pValueStruct)[whParam[8]]).pROIParamVect)[i] = 1.

            ; '3D Box Flatness'
         if whParamActive[9] then if (evLength[1] gt 0) then (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[i] = 1. - (evLength[2]/evLength[1]) else (*(*(*self.pValueStruct)[whParam[9]]).pROIParamVect)[i] = 1.

            ; '3D Box Entropy'
         if whParamActive[10] then begin
            whereNE0 = where(evLength ne 0, count)
            case count of
            0: (*(*(*self.pValueStruct)[whParam[10]]).pROIParamVect)[i] = 1.
            else: begin
               p = evLength[whereNE0] / total(evLength[whereNE0])
               (*(*(*self.pValueStruct)[whParam[10]]).pROIParamVect)[i] = -1./alog10(3.) * total(p * alog10(p))
            endcase
            endcase
         endif
      endfor

         ; '3D Box Overlaps'
      if (whParamActive[11] or whParamActive[12]) then begin

            ; fill xyz with all Boxes
         for i = 0, nObjects-1 do if ((*pBoxPos[i])[0] ne -1) then xyzVol[(*pBoxPos[i])] += 1

            ; '3D Box Single Overlap'
         if whParamActive[11] then begin
            for i = 0, nObjects-1 do begin
               if ((*pBoxPos[i])[0] ne -1) then begin

                  nSing = total(xyzVol[(*pBoxPos[i])] gt 1)
                  (*(*(*self.pValueStruct)[whParam[11]]).pROIParamVect)[i] = nSing / n_elements(*pBoxPos[i])

               endif else (*(*(*self.pValueStruct)[whParam[11]]).pROIParamVect)[i] = -1
            endfor
         endif

            ; '3D Box All Overlap'
         if whParamActive[12] then begin
            result = total(xyzVol gt 1) / total(xyzVol ge 1)
            for i = 0, nObjects-1 do (*(*(*self.pValueStruct)[whParam[12]]).pROIParamVect)[i] = result
         endif

         xyzVol = 0
         for i = 0, nObjects-1 do if ptr_valid(pBoxPos[nObjects-i-1]) then ptr_free, pBoxPos[nObjects-i-1]
         ptr_free, pBoxPos
      endif
   endelse

  if fSurf then begin
     for i = oSurfModel->count()-1,0,-1 do obj_destroy, oSurfModel->get(position=i)
     oSurfModel->remove, /all
     obj_destroy, oSurfModel
  endif   
end


function C_sROIParam_3DObjBoxModel::init

   ROIParamStruct = {name:'3D Object Box',$     ;  ROI Name.
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    nParams = 13
    self.pValueStruct = ptr_new(ptrArr(nParams))
    ROIParamWidgetType = make_array(nParams, /string, value = 'widget_slider')
    ROIParamNames = ['3D Box Volume [x³]',$
                     '3D Vol div Box Vol',$
                     '3D Box Surface [x²]',$
                     '3D Box Sphericity',$ -> http://en.wikipedia.org/wiki/Sphericity
                     '3D Box Length [x]',$
                     '3D Box Width [x]',$
                     '3D Box Height [x]',$
                     '3D Box Elongation',$
                     '3D Box Relative Elongation',$
                     '3D Box Flatness',$
                     '3D Box Entropy',$
                     '3D Box Single Overlap',$
                     '3D Box All Overlap']

    ROIParamActive = make_array(nParams, /byte, value = 1b)
    ROIParamMin = make_array(nParams, /float, value = 0.)
    ROIParamMax = make_array(nParams, /float, value = 1.)
    ROIParamValues = make_array(nParams, /float, value = 0.)
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[0],$
                      type:'3D ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$
                      pNames:ptr_new(),$
                      pActive:ptr_new(),$
                      pMin:ptr_new(),$
                      pMax:ptr_new(),$
                      pValues:ptr_new(),$
                      pROIParamVect:ptr_new()}

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,0,0,0,0,0,0,0]
    ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.]
    ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1.]
    ROIValueValues =[0.,1.,0.,1.,0.,1.,0.,1.]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    for i = 1, nParams-2 do begin
       ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[i],$
                       type:'3D ROI-Parameter-Method',$
                       pWidgetType:ptr_new(),$
                       pNames:ptr_new(),$
                       pActive:ptr_new(),$
                       pMin:ptr_new(),$
                       pMax:ptr_new(),$
                       pValues:ptr_new(),$
                       pROIParamVect:ptr_new()}

       ROIValueWidgetType = [ROIValueWidgetType]
       ROIValueNames = [ROIValueNames]
       ROIValueActive = [ROIValueActive]
       ROIValueMin = [ROIValueMin]
       ROIValueMax = [ROIValueMax]
       ROIValueValues =[ROIValueValues]

       pROIParamVect = [-1]
       ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
       ROIValueStruct.pNames = ptr_new(ROIValueNames)
       ROIValueStruct.pActive = ptr_new(ROIValueActive)
       ROIValueStruct.pMin = ptr_new(ROIValueMin)
       ROIValueStruct.pMax = ptr_new(ROIValueMax)
       ROIValueStruct.pValues = ptr_new(ROIValueValues)
       ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

       (*self.pValueStruct)[i] = ptr_new(ROIValueStruct, /no_copy)
    endfor

    ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[nParams-1],$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$
                    pNames:ptr_new(),$
                    pActive:ptr_new(),$
                    pMin:ptr_new(),$
                    pMax:ptr_new(),$
                    pValues:ptr_new(),$
                    pROIParamVect:ptr_new()}

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[nParams-1] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end

pro C_sROIParam_3DObjBoxModel__define
   tmp = {C_sROIParam_3DObjBoxModel, pParamStruct:ptr_new(), pValueStruct:ptr_new(), inherits C_sROIParam}
end