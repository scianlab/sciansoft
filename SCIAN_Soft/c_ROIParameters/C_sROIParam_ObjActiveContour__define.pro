;_______________________________________IOISIOI_______________________________________
; NAME:
;     C_sROIParam_ActiveContour
;
; PURPOSE:
;     - Encapsulation for 2D contours and their derived parameters, as outputs from Active Contours
;
; AUTHOR:
;     Jorge Jara (2005)
;     e_mail:jjaraw@gmail.com
;
; CALLING SEQUENCE:
;     result = obj_new('C_sROIParam_ObjActiveContour' )
;
; METHOHDS:
;_______________________________________IOISIOI_______________________________________
; calcular en relación a las dim reales
; agregar 3 parám q dividen las 2 curvaturas y los inflexPts c/r al perímetro + Object AC-P²A
pro C_sROIParam_ObjActiveContour::apply, C_sROIGroupObj = C_sROIGroupObj, stack_tlb = stack_tlb

   whParam = [(where(*(*self.pParamStruct).pNames eq 'Object AC-Perimeter'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Curvature-Circular'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Curvature-Derivative'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Mean-Curv-Circular'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Mean-Curv-Derivative'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Inflection-Points'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Mean-Inflection-Points'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Average-Gradient-X'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Average-Gradient-Y'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Curv-Circular_Perimeter'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-Curv-Derivative_Perimeter'))[0],$
                 (where(*(*self.pParamStruct).pNames eq 'Object AC-P²A'))[0]]

      ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1:if (position[0] eq -1) then return else  whParamActive[position] = 1
      else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase

      ; check Pointers
   wherePA = where(whParamActive eq 1)
   if ~ptr_valid((*self.pParamStruct).pROINumberVect) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if ~ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

   nObjects = C_sROIGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

      pParamStruct = C_sROIGroupObj->getpParamStruct()
      whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [pixel]'))[0]
      pixelXsize = *(((*pParamStruct).pValues)[whereDim]) > 1
      whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [pixel]'))[0]
      pixelYsize = *(((*pParamStruct).pValues)[whereDim]) > 1
      whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [real]'))[0]
      realXsize = *(((*pParamStruct).pValues)[whereDim]) > 1
      whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [real]'))[0]
      realYsize = *(((*pParamStruct).pValues)[whereDim])

      xSizeRatio = realXsize / pixelXsize
      ySizeRatio = realYsize / pixelYsize

         ; set Object Number Vector
      *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

      if (total(whParamActive[*]) gt 0) then begin

         thisAlpha = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Alpha'))[0]]
         thisBeta = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Beta'))[0]]
         thisGamma = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Gamma'))[0]]
         thisKappa = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Kappa'))[0]]
         thisMu = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Mu'))[0]]
         thisSnakeIterations = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Snake_Iterations'))[0]]
         thisGVFiterations = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'GVF_Iterations'))[0]]
         thisPerimeterFactor = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Perimeter_Factor'))[0]]
         fKeepGGVF = (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Keep_GGVF'))[0]] eq 1

         if ~fKeepGGVF then begin
            if ptr_valid((*self.pParamStruct).pSnake) then begin
               if obj_valid(*(*self.pParamStruct).pSnake) then obj_destroy, *(*self.pParamStruct).pSnake
               ptr_free, (*self.pParamStruct).pSnake
            endif
         endif

            ; get original intensity image... or the mask intensity image
         s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos
         widget_control, stack_tlb, get_uValue = stateStack, /no_copy
            intImage = (*stateStack.pImageStackInfoObject)->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos)
         widget_control, stack_tlb, set_uValue = stateStack, /no_copy
         ;intImage = C_sROIGroupObj->getGroupMaskIntensity()

         if ~fKeepGGVF or (fKeepGGVF and ~ptr_valid((*self.pParamStruct).pSnake)) then begin

            (*self.pParamStruct).pSnake = ptr_new(obj_new('C_sActiveContour', intImage,$
                                                                              alpha = thisAlpha,$
                                                                              beta = thisBeta,$
                                                                              gamma = thisGamma,$
                                                                              kappa = thisKappa,$
                                                                              mu = thisMu,$
                                                                              iterations = thisSnakeIterations,$
                                                                              gvf_iterations = thisGVFiterations), /no_copy)
            (*(*self.pParamStruct).pSnake)->calcGGVF
         endif

         (*(*self.pParamStruct).pSnake)->setParams, alpha = thisAlpha, beta = thisBeta, gamma = thisGamma,$
                                                    kappa = thisKappa, mu = thisMu,$
                                                    iterations = thisSnakeIterations, gvf_iterations = thisGVFiterations
         dimI = size(intImage, /dim)
         xWinDim = dimI[0]
         yWinDim = dimI[1]
        ; window, 31, xsize = xWinDim, ysize = yWinDim
        ; tvscl, congrid(intImage, xWinDim, yWinDim, /interp, /center)

         paramMatrix = make_array(n_elements(whParam), nObjects>1, /float)
         for i = 0, nObjects-1 do begin

            pObjectBorderPolygon = (C_sROIGroupObj->get(position = i))->getpObjectBorderPolygonList()

            (*(*self.pParamStruct).pSnake)->setContour, transpose((*pObjectBorderPolygon[0])[0,*]), transpose((*pObjectBorderPolygon[0])[1,*])

            npts = round(((*(*self.pParamStruct).pSnake)->getPerimeter()) * thisPerimeterFactor) > 16
            (*(*self.pParamStruct).pSnake)->arcSample, points = npts
            fContour = (*(*self.pParamStruct).pSnake)->adjustContour()
            xcoords =  (*(*self.pParamStruct).pSnake)->getXcoords()
            ycoords =  (*(*self.pParamStruct).pSnake)->getYcoords()
          ;  plot, 0.5+xcoords, 0.5+ycoords, xstyle=1, ystyle=1, xrange=[0, dimI[0]], yrange=[0, dimI[1]], position = [0, 0, xWinDim, yWinDim], /noErase

               ; distance between i, i+1 along the snake
            pointDist = (*(*self.pParamStruct).pSnake)->getDistance(xyRes = [xSizeRatio, ySizeRatio])
            paramMatrix[0,i] = total(pointDist)
            minP = min(pointDist, max = maxP)
          ;  plots, 0.5+xcoords, 0.5+ycoords, color = (pointDist-minP)/((maxP-minP)/255.)

               ; curvature along the snake with triangles
            pointCir3 = (*(*self.pParamStruct).pSnake)->getCir3(xyRes = [xSizeRatio, ySizeRatio])
            pointCurv3 = pointCir3 * 0.
            whereN0 = where(pointCir3 ne 0)
            if (whereN0[0] ne -1) then pointCurv3[whereN0] = 1./pointCir3[whereN0]
          ;  plots, 0.5+xcoords, 0.5+ycoords, color = (pointCurv3-min(pointCurv3))/((max(pointCurv3)-min(pointCurv3))/255.), thick = 2
            paramMatrix[1,i] = total(pointCurv3)

               ; curvature along the snake with derivatives
            pointCurvD = abs((*(*self.pParamStruct).pSnake)->getCurvD(xyRes = [xSizeRatio, ySizeRatio]))
            paramMatrix[2,i] = total(pointCurvD)
 ;           plots, 0.5+xcoords, 0.5+ycoords, color = (pointCurvD-min(pointCurvD))/((max(pointCurvD)-min(pointCurvD))/255.), thick = 2

               ; normalized curvatures along the snake
            paramMatrix[3,i] = (moment(pointCurv3))[0]
            paramMatrix[4,i] = (moment(pointCurvD))[0]

               ; inflex points
            pointInflex = (*(*self.pParamStruct).pSnake)->getInflexPoints()
            whereGT0 = where(pointInflex gt 0)
            if (whereGT0[0] ne -1) then begin
                paramMatrix[5,i] = n_elements(whereGT0)
     ;           for j = 0, n_elements(whereGT0)-1 do $
     ;              plots, [0.5+(xcoords)[whereGT0[j]],0.5+(xcoords)[whereGT0[j]]] , [0.5+(ycoords)[whereGT0[j]], 0.5+(ycoords)[whereGT0[j]] ], psym = 1, color = [255,255,255], thick = 1.
            endif

            paramMatrix[6,i] = n_elements(whereGT0)/paramMatrix[0,i]
            paramMatrix[7,i] = 0.
            paramMatrix[8,i] = 0.
            paramMatrix[9,i] = paramMatrix[1,i]/paramMatrix[0,i]
            paramMatrix[10,i] = paramMatrix[2,i]/paramMatrix[0,i]

               ; object P²A
            oRoi = obj_new('IDLanROI',xcoords * xSizeRatio, ycoords * ySizeRatio)
            result = oRoi->computeGeometry(area = thisArea)
            obj_destroy, oRoi
            paramMatrix[11,i] = paramMatrix[0,i]^2 / thisArea
         endfor

         if ~fKeepGGVF then begin
            if ptr_valid((*self.pParamStruct).pSnake) then begin
               if obj_valid(*(*self.pParamStruct).pSnake) then obj_destroy, *(*self.pParamStruct).pSnake
               ptr_free, (*self.pParamStruct).pSnake
            endif
         endif

         for i = 0, n_elements(whParam)-1 do $
            if whParamActive[i] then *(*(*self.pValueStruct)[whParam[i]]).pROIParamVect = make_array(nObjects, /float) + paramMatrix[i, *]
      endif
   endelse
end


function C_sROIParam_ObjActiveContour::init

   ROIParamStruct = {name:'Object AC-Parameters',$   ;  ROI Name.
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$              ; Pointer on ROI-Obj Parameter Names.
                 pNames:ptr_new(),$                   ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$                  ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$                     ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$                     ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$                  ; Pointer on ROI-Obj Parameter Values.
                 pSnake:ptr_new(),$                   ; Pointer on Snake-Object.
                 pROINumberVect:ptr_new()}            ; Pointer on ROI-Obj Number Vector

   self.pValueStruct = ptr_new(ptrArr(12))
   ROIParamWidgetType = make_array(12, /string, value = 'widget_slider')
   ROIParamNames = ['Object AC-Perimeter',$
                    'Object AC-Curvature-Circular',$
                    'Object AC-Curvature-Derivative',$
                    'Object AC-Mean-Curv-Circular',$
                    'Object AC-Mean-Curv-Derivative',$
                    'Object AC-Inflection-Points',$
                    'Object AC-Mean-Inflection-Points',$
                    'Object AC-Average-Gradient-X',$
                    'Object AC-Average-Gradient-Y',$
                    'Object AC-Curv-Circular_Perimeter',$
                    'Object AC-Curv-Derivative_Perimeter',$
                    'Object AC-P²A']

   ROIParamActive = [1,1,1,1,1,1,1,1,1,1,1,1]
   ROIParamMin = [0,0,0,0,0,0,0,0,0,0,0,0]
   ROIParamMax = [0,0,0,0,0,0,0,0,0,0,0,0]
   ROIParamValues = [0,0,0,0,0,0,0,0,0,0,0,0]
   pROINumberVect = [-1]

   ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
   ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
   ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
   ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
   ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
   ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
   ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
   self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

   ROIValueStruct = {name:'Object AC-Perimeter',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(16, /string, value = 'widget_slider')
   ROIValueNames = ['Alpha',$
                    'Beta',$
                    'Gamma',$
                    'Kappa',$
                    'Mu',$
                    'Snake_Iterations',$
                    'GVF_Iterations',$
                    'Perimeter_Factor',$
                    'Keep_GGVF',$
                    'Threshold_1a','Threshold_1b',$
                    'Threshold_2a','Threshold_2b',$
                    'Threshold_3a','Threshold_3b',$
                    'Threshold_4a','Threshold_4b']
   ROIValueActive = [1b,1b,1b,1b,1b,1b,1b,1b,1b,1b,0b,0b,0b,0b,0b,0b,0b]
   ROIValueMin = make_array(17, /float)
   ROIValueMax    = [3.,3.,3.,3.,1. ,90.,90,5.,1.,1.,1.,1.,1.,1.,1.,1.]
   ROIValueValues = [.2,.2,1.,.1,.05,5  ,20,1.,0.,0.,1.,0.,1.,0.,1.,0.,1.]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
   ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
   ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
   ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
   ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
   ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Object AC-Curvature-Circular',$
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Object AC-Curvature-Derivative',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                    'Threshold_2a', 'Threshold_2b',$
                    'Threshold_3a', 'Threshold_3b',$
                    'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Object AC-Mean-Curv-Circular',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
    ROIValueActive = make_array(8, /byte, value = 0)
    ROIValueActive[0] = 1
    ROIValueMin = make_array(8, /float, value = 0)
    ROIValueMax = make_array(8, /float, value = 1)
    ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Object AC-Mean-Curv-Derivative',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Object AC-Inflection-Points',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                    'Threshold_2a', 'Threshold_2b',$
                    'Threshold_3a', 'Threshold_3b',$
                    'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[5] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Object AC-Mean-Inflection-Points',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[6] = ptr_new(ROIValueStruct, /no_copy)


   ROIValueStruct = {name:'Object AC-Average-Gradient-X',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[7] = ptr_new(ROIValueStruct, /no_copy)


   ROIValueStruct = {name:'Object AC-Average-Gradient-Y',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[8] = ptr_new(ROIValueStruct, /no_copy)


   ROIValueStruct = {name:'Object AC-Curv-Circular_Perimeter',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[9] = ptr_new(ROIValueStruct, /no_copy)


   ROIValueStruct = {name:'Object AC-Curv-Circular_Derivative',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                    'Threshold_2a', 'Threshold_2b',$
                    'Threshold_3a', 'Threshold_3b',$
                    'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
   ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
   ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
   ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
   ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
   ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[10] = ptr_new(ROIValueStruct, /no_copy)


   ROIValueStruct = {name:'Object AC-P²A',$
                 type:'Single ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$ ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$  ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$  ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$    ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}   ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                    'Threshold_2a', 'Threshold_2b',$
                    'Threshold_3a', 'Threshold_3b',$
                    'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1. ]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
   ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
   ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
   ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
   ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
   ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[11] = ptr_new(ROIValueStruct, /no_copy)

   return, 1
end

pro C_sROIParam_ObjActiveContour__define
   tmp = {C_sROIParam_ObjActiveContour,$
          pParamStruct:ptr_new(),$
          pValueStruct:ptr_new(),$
          inherits C_sROIParam}
end