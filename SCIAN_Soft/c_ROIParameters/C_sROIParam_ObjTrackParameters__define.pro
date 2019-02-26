;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjTrackParameters
;
; PURPOSE:
;       - Tracking of Objects.
;
; AUTHOR:
;     Mauricio Cerda (2013)
;     e_mail: mauriciocerda@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjTrackParameters')
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjTrackParameters::apply,  C_sROIGroupObj=C_sROIGroupObj, stack_tlb = stack_tlb, position = position
 
   whParam = [(where(*(*self.pParamStruct).pNames eq 'Track VCL'))[0],$
              (where(*(*self.pParamStruct).pNames eq 'Track VSL'))[0],$
              (where(*(*self.pParamStruct).pNames eq 'Track Length'))[0] ]
   
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, stateObj_tlb = stateObj_tlb
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalZNum = totalZNum,clusPos = clusPos,selROIGroupObj = selROIGroupObj ; Susana por clusPos
   
   
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
   imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   
              
    ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1:if (position[0] eq -1) then return else whParamActive[position] = 1
       else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase

    ; check Pointers
    wherePA = where(whParamActive eq 1)
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROIGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif
      
    ; set Object Number Vector
    *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

    if nObjects gt 0 then begin 
    
    
    ;wTopBase viene de la ventana track
   restore, FILENAME='C:\RSI\trackWidgetNumber.sav'
   widget_control, wTopBase, get_uValue = trackState, /no_copy
   
   C_sROIGroupObj->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
       
   pParamStruct = (*trackState.poCurrTrackGroup)->getpParamStruct()
   
       
   tStart = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Start'))[0]]
   tEnd = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T End'))[0]]
   tInt = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'T Interval'))[0]]
   ;TODO make it dynamic
   dt=1.0
   imageSize=C_sROIGroupObj->getXYZDim()
   realSize=imageSize
   factor=realSize/imageSize
   roisCentroids=C_sROIGroupObj->getGroupCenterXYZ()
   
   paramVCL    = fltArr(nObjects)
   paramVSL    = fltArr(nObjects)
   paramLength = fltArr(nObjects)
   
    ; Object-Loop for the determination of object size [pixel]
    for i = 0, nObjects-1 do begin

      drawTrajectoryFlag=0b
      drawTrajectoryindex=-1          
      ;get ROI center of mass
      
      for j = 0, (*trackState.poCurrTrackGroup)->getObjNum()-1 do begin
        trackStruct = (*trackState.poCurrTrackGroup)->getTrackParam(paramName = 'Track Path', objNum = j)
        trackVect     = trackStruct.pXYZValues     
        trackVectTime = fix((*trackStruct.pTValues)/dt)
      
        N= size(*trackVect, /dim)
        insideInterval=where(trackVectTime eq tPos)
        
        if insideInterval[0] ne -1 then begin
            ;verify if this trajectory contains this ROI
            distance=sqrt(  ((*trackVect)[insideInterval[0],0] - roisCentroids[1,i]*factor[0])^2+$
                            ((*trackVect)[insideInterval[0],1] - roisCentroids[2,i]*factor[1])^2 )
                            
            ;print, 'distance=' + string(distance)
            
            if distance lt 1.0 then begin
              ;print, 'ROI-Trajectory association founded, distance= '+string(distance)
              drawTrajectoryFlag = 1b
              drawTrajectoryindex = j
              break
            endif
        endif
                                                
      endfor     
      
      if drawTrajectoryFlag eq 1b then begin
      
          ;trajectory VCL
          trackStruct = (*trackState.poCurrTrackGroup)->getTrackParam(paramName = 'Track VCL', objNum = drawTrajectoryindex)
          dx=trackStruct.XYZDisplacement
          paramVCL[i]=total(dx/dt)
          
          ;trajectory length
          trackStruct = (*trackState.poCurrTrackGroup)->getTrackParam(paramName = 'Track Path', objNum = drawTrajectoryindex)
          trackVect     = trackStruct.pXYZValues 
          trackVectTime = fix((*trackStruct.pTValues)/dt)
                      
          N= size(*trackVect, /dim)
          paramLength[i]=N[0]
          
          ;trajectory VSL
          paramVSL[i]= sqrt( ( (*trackVect)[0,0]-(*trackVect)[N[0]-1,0])^2 + ( (*trackVect)[0,1]-(*trackVect)[N[0]-1,1] )^2 ) 
          paramVSL[i]= paramVSL[i] / (N[0]*dt)
          
      endif else begin
            print, 'ROI is not in any trajectory'
      endelse
      
    endfor
    
    print, 'After N-Objects' 
        
    ; speed [pixels/frame]
    if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = paramVCL
    ; angle [degrees]
    if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = paramVSL
    ; vx [pixels/frame]
    if whParamActive[2] then *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = paramLength
    
    endif
    
    widget_control, wTopBase, set_uValue = trackState, /no_copy
    
end


function C_sROIParam_ObjTrackParameters::init

   ROIParamStruct = {name:'Object Track Parameters',$   ;  ROI Name.
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROINumberVect:ptr_new(),$     ; Pointer on ROI-Obj Number Vector
                     pROIOverlapMatrix:ptr_new(),$    ; Pointer on ROI-Obj Overlap Matrix.
                     pROIDistanceMatrix:ptr_new(),$     ; Pointer on ROI-Obj Distance Matrix.
                     pROICandidateArray:ptr_new() }  ; Pointer on ROI-Obj Candidates Matrix.
                                                     ; each row (track#, 1,2,3, ...,N) contains a list of candidates
                    
   self.pValueStruct = ptr_new(ptrArr(3))
   ROIParamWidgetType = make_array(3, /string, value = 'widget_slider')
   ROIParamNames = [ 'Track VCL',$
                     'Track VSL',$
                     'Track Length' ]
   ROIParamActive = [1,1,1]
   ROIParamMin = [0,0,0]
   ROIParamMax = [1,1,1]
   ROIParamValues = [0,0,0]
   pROINumberVect = [-1]

   ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
   ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
   ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
   ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
   ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
   ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
   ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

   self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

   ROIValueStruct = {name:'Track VCL',$
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(9, /string, value = 'widget_slider')
   ROIValueNames = [ 'Highest Object Number',$
                     'Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
   ROIValueActive = [1,0,0,0,0,0,0,0,0]
   ROIValueMin = [1,0.,0.,0.,0.,0.,0.,0.,0.]
   ROIValueMax = [10000,1.,1.,1.,1.,1.,1.,1.,1.]
   ROIValueValues = [0,0.,1.,0.,1.,0.,1.,0.,1.]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
   (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)
   
   ROIValueStruct = {name:'Track VSL',$
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = [ 'Threshold_1a', 'Threshold_1b',$
                     'Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b',$
                     'Threshold_4a', 'Threshold_4b']
   ROIValueActive = [0,0,0,0,0,0,0,0]
   ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.]
   ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1.]
   ROIValueValues = [0.,1.,0.,1.,0.,1.,0.,1.]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
   (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:'Track Length',$
                     type:'Single ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                     pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax: ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                     pROIParamVect: ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

   ROIValueWidgetType = ['widget_slider','widget_slider', ROIValueWidgetType]
   ROIValueNames = ['x-Size per Pixel','y-Size per Pixel', ROIValueNames]
   ROIValueActive = [1, 1, ROIValueActive]
   ROIValueMin = [0., 0., ROIValueMin]
   ROIValueMax = [1000., 1000., ROIValueMax]
   ROIValueValues = [1., 1., ROIValueValues]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
   ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
   ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
   ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
   ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
   ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

   (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)
   return, 1
end

pro C_sROIParam_ObjTrackParameters__define
   tmp = {C_sROIParam_ObjTrackParameters, pParamStruct: ptr_new(), pValueStruct: ptr_new(), inherits C_sROIParam}
end