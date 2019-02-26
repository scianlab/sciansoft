;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjOpticalFlow
;
; PURPOSE:
;       - Calculation of OF and report using 2D ROI's
;
; AUTHOR:
;     Dr. Mauricio CERDA (2012)
;     e_mail:mauriciocerda@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjOpticaFlow' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjOpticalFlow::apply, C_sROIGroupObj = C_sROIGroupObj, stack_tlb = stack_tlb, position = position

    whParam = [(where( *(*self.pParamStruct).pNames eq 'Object Speed [pixelsPframe]'))[0],$
               (where( *(*self.pParamStruct).pNames eq 'Object Angle [deg]'))[0],$
               (where( *(*self.pParamStruct).pNames eq 'Object Vx [pixelsPframe]'))[0],$
               (where( *(*self.pParamStruct).pNames eq 'Object Vy [pixelsPframe]'))[0],$
               (where( *(*self.pParamStruct).pNames eq 'Object Speed2 [pixelsPframe]'))[0] ]

    ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1:if (position[0] eq -1) then return else whParamActive[position] = 1
       else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase

    ; check Pointers
    wherePA = where(whParamActive eq 1)
    if ~ptr_valid((*self.pParamStruct).pROINumberVect) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if ~ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROIGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif
      
    ; set Object Number Vector
    *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

    if nObjects gt 0 then begin 
    ; Object-Loop for the determination of object size [pixel]
    paramVectSpeed = fltArr(nObjects)
    paramVectAngle = fltArr(nObjects)
    paramVx        = fltArr(nObjects)
    paramVy        = fltArr(nObjects)
    paramSpeed2    = fltArr(nObjects)
    
    self->CallOpticalFlow2D, u, v, indComputed, stack_tlb=stack_tlb, C_sROIGroupObj=C_sROIGroupObj
    
    speed=sqrt(u*u+v*v)
    angle=atan(v, u)
    for i = 0, nObjects-1 do begin
      ;Complete here
      indSeg=*((C_sROIGroupObj->get(position = i))->getpWherePoints())
      indSeg=SetIntersection(indSeg, indComputed)
      
      if indSeg[0] ne -1 then begin
        paramVectSpeed[i]=mean(speed(indSeg))
        paramVectAngle[i]=mean(angle(indSeg))
        paramVx[i]=mean(u(indSeg))
        paramVy[i]=mean(v(indSeg))
        paramSpeed2[i]=sqrt(paramVx[i]*paramVx[i]+paramVy[i]*paramVy[i])
      end
      
      if indSeg[0] eq -1 then begin
        paramVectSpeed[i]=0.0
        paramVectAngle[i]=0.0
        paramVx[i]=0.0
        paramVy[i]=0.0
        paramSpeed2[i]=0.0
      end
    endfor
    
    print, 'After N-Objects' 
        
    ; speed [pixels/frame]
    if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = paramVectSpeed
    ; angle [degrees]
    if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = paramVectAngle
    ; vx [pixels/frame]
    if whParamActive[2] then *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = paramVx
    ; vy [pixels/frame]
    if whParamActive[3] then *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = paramVy
    ; speed2 [pixels/frame]
    if whParamActive[4] then *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = paramSpeed2
    
    endif
end


pro C_sROIParam_ObjOpticalFlow::CallOpticalFlow2D, stack_tlb=stack_tlb, C_sROIGroupObj=C_sROIGroupObj, u, v, indComputed

  ; get 3d image, compute of, move ROI...
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
  imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy


  s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos=clusPos, totalTNum = totalTNum
  
  
  ;get segmentation
  segmentationCluster=0
  oImage = imageStackInfoObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)  
  segmentedImage = oImage->applyImageSegmentation(selectedStackObject = imageStackInfoObject ,$
                                          tPos = tPos , chPos = chPos , zPos = zPos , clusPos = segmentationCluster )  
  indSeg = where(segmentedImage ne 0)
  chPos=0 ; solo al juntar con segmentacion de erika el input viene del canal 0
  
  image1=imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos=zPos)
  image2=imageStackInfoObject->getSelectedImage(tPos = (tPos+1)<(totalTNum-1), chPos = chPos, zPos=zPos)
  getDerivatives, image1*1.0, image2*1.0, ex=ex, ey=ey, et=et
  
  scales=4
  method=0; Method 0:HS 1:CLG 2:LK
    
  print, 'OF 2D ROI starting...'
  ;auto alpha
  auto=1b ; 1b : computes alpha 9b: manual alpha
  
  if auto eq 1b then begin 
    gradient=ex*ex+ey*ey
        
    if method eq 0 then begin
      alpha=sqrt(mean(gradient(indSeg)))
      print, 'alpha,', alpha
    endif
    if method eq 1 then begin
      alpha=mean(gradient(indSeg))/3.0
      print, 'alpha,', alpha
    endif
    if method eq 2 then begin
      tau=0.0
      print, 'tau,', tau
    endif
    
  endif
  
  if auto eq 0b and (method eq 0 or method eq 1) then alpha=10.0 
  
  ;HS and CLG are computed for each pixel
  indComputed = where(image1 ge 0.0 )
  
  if method eq 0 or method eq 1 then opticalFlowMultiscale, image1, image2, scales, method, u, v, ALPHA =alpha, ITERATIONS = 200, verbose=0b
  if method eq 2 then opticalFlowMultiscale, image1, image2, scales, method, u, v,  verbose = 0b, tau=tau, indComputed=indComputed
            
  ;indSeg=SetIntersection(indSeg, indComputed)
        
  print, 'OF 2D ROI computed'
  
end


function C_sROIParam_ObjOpticalFlow::init

    ROIParamStruct = {name:'Object Optical Flow',$   ;  ROI Name.
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(5))
    ROIParamWidgetType = make_array(5, /string, value = 'widget_slider')
    ROIParamNames = ['Object Speed [pixelsPframe]', 'Object Angle [deg]', 'Object Vx [pixelsPframe]', 'Object Vy [pixelsPframe]', 'Object Speed2 [pixelsPframe]']
    ROIParamActive = [1,1,1,1,1]
    ROIParamMin = [0,0,0,0,0]
    ROIParamMax = [0,0,0,0,0]
    ROIParamValues = [0,0,0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ;;;;;;;;;;SUB-ROI 0
    ROIValueStruct = {name:'Object Speed [pixelsPframe]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
                     
    ROIValueActive = make_array(8, /byte, value = 0)
    ROIValueMin = make_array(8, /float, value = 0)
    ROIValueMax = make_array(8, /float, value = 1)
    ROIValueValues =[0.,1.,$
                     0.,1.,$
                     0.,1.,$
                     0.,1.]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ;;;;;;;;;;SUB-ROI 1
    ROIValueStruct = {name:'Object Angle [deg]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIValueWidgetType[4:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ;;;;;;;;;;SUB-ROI 2
    ROIValueStruct = {name:'Object Vx [pixelsPframe]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ;ROIValueWidgetType = [ROIValueWidgetType[4:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)


    ;;;;;;;;;;SUB-ROI 3
    ROIValueStruct = {name:'Object Vy [pixelsPframe]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ;ROIValueWidgetType = [ROIValueWidgetType[4:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

    ;;;;;;;;;;SUB-ROI 4
    ROIValueStruct = {name:'Object Speed2 [pixelsPframe]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ;ROIValueWidgetType = [ROIValueWidgetType[4:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)
    
    return, 1
end

pro C_sROIParam_ObjOpticalFlow__define
   tmp = {C_sROIParam_ObjOpticalFlow, $
          pParamStruct:ptr_new(),$
          pValueStruct:ptr_new(),$
          inherits C_sROIParam}
end
