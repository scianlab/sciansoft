;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_SomitesLocatorOnBF
;
; PURPOSE:
;     define initial position for somites and other boundaries for NeuralCrest MIgration
; for use with BF images... we need to see the somite...
; the results will be saved in the folder for tracking information...
; it filter need some sphere_angleInformation :D too
;
; AUTHOR:
;     FASL 2012
;     e-mail: fsantibanez@med.uchile.cl
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_SomitesLocatorOnBF' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_SomitesLocatorOnBF::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
    return,  'FASL_NC_Release_Filter_Method'
end

function C_sImageFilter_SomitesLocatorOnBF::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_SomitesLocatorOnBF::apply, image = image,$
                                               selectedStackObject = selectedStackObject,$
                                               tPos = tPos,$
                                               chPos = chPos,$
                                               zPos = zPos,$
                                               clusPos = clusPos,$
                                               segPos = segPos,$
                                               cut_x = cut_x,$
                                               cut_y = cut_y,$
                                               stack_tlb = stack_tlb

; Recovery and assign paramas

   if (size(image, /n_dim) ne 2) then return, image

  image = DOUBLE(image)
  ; Limits for variables
   dimI = size(image, /dim)

   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalTNum =  vTotalTimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalZNum =  vTotalZimes
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, totalChNum = vTotalChNum


   whParam = (where((*(*self.pParamStruct).pNames) eq 'Use ZProjection for BF'))[0]
   uProjection = (*(*self.pParamStruct).pActive)[whParam] 
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Time BF'))[0]
   uTime = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = 0
   (*(*self.pParamStruct).pMax)[whParam] = vTotalTimes
   ; Get Value   
   vTime = (*(*self.pParamStruct).pValues)[whParam]    
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Channel BF'))[0]
   uChannel = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = 0
   (*(*self.pParamStruct).pMax)[whParam] = vTotalChNum
   ; Get Value   
   vChannel = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'zPosition BF'))[0]
   uZPos = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = 0
   (*(*self.pParamStruct).pMax)[whParam] = vTotalZimes
   ; Get Value   
   if((*(*self.pParamStruct).pValues)[whParam]  ge vTotalZimes) then (*(*self.pParamStruct).pValues)[whParam] = vTotalZimes-1
   vZPos = (*(*self.pParamStruct).pValues)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Use automatic rotation/Flip'))[0]
   uAutoRotateFlip = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite Aperture Angle'))[0]
   uAngle = (*(*self.pParamStruct).pActive)[whParam]
   vAngle = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite6_Init_X'))[0]
   uSom6x = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(0)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(0)
   ; Get Value   
   vSom6x = (*(*self.pParamStruct).pValues)[whParam]    
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite6_Init_Y'))[0]
   uSom6y = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(1)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(1)
   ; Get Value   
   vSom6y = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite7_Init_X'))[0]
   uSom7x = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(0)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(0)
   ; Get Value   
   vSom7x = (*(*self.pParamStruct).pValues)[whParam]    
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite7_Init_Y'))[0]
   uSom7y = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(1)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(1)
   ; Get Value   
   vSom7y = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite8_Init_X'))[0]
   uSom8x = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(0)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(0)
   ; Get Value   
   vSom8x = (*(*self.pParamStruct).pValues)[whParam]    
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite8_Init_Y'))[0]
   uSom8y = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(1)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(1)
   ; Get Value   
   vSom8y = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite9_Init_X'))[0]
   uSom9x = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(0)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(0)
   ; Get Value   
   vSom9x = (*(*self.pParamStruct).pValues)[whParam]    
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Somite9_Init_Y'))[0]
   uSom9y = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = -0.5*dimI(1)
   (*(*self.pParamStruct).pMax)[whParam] = 1.5*dimI(1)
   ; Get Value   
   vSom9y = (*(*self.pParamStruct).pValues)[whParam]    


   whParam = (where((*(*self.pParamStruct).pNames) eq 'D/V division 1 __ Y posisiton'))[0]
   uDivPosY1 = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = 0
   (*(*self.pParamStruct).pMax)[whParam] = dimI(1)
   ; Get Value   
   vDivPosY1 = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'D/V division 2 __ Y posisiton'))[0]
   uDivPosY2 = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = 0
   (*(*self.pParamStruct).pMax)[whParam] = dimI(1)
   ; Get Value   
   vDivPosY2 = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'D/V division 3 __ Y posisiton'))[0]
   uDivPosY3 = (*(*self.pParamStruct).pActive)[whParam]
   ; Check variables 
   (*(*self.pParamStruct).pMin)[whParam] = 0
   (*(*self.pParamStruct).pMax)[whParam] = dimI(1)
   ; Get Value   
   vDivPosY3 = (*(*self.pParamStruct).pValues)[whParam]    

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Save Reference¨s System'))[0]
   uSaveBoundaries = (*(*self.pParamStruct).pActive)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Try Automatic Positions'))[0]
   uTryAutomatic = (*(*self.pParamStruct).pActive)[whParam]
   ; Only try one time... then the user need optimize the values....
   ; if you reActive this function then you can restart to the initial estimated values....
   (*(*self.pParamStruct).pValues)[whParam] = 0

  ; Stack Data

   pParamStruct = selectedStackObject->getpParamStruct()
   totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]
   totalChannels = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]]
   totalzSlices = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of z-Slices'))[0]]

  ; Reorientate and FLip... based on saved file "PointsAndAngle" .... if uAutoRotateFlip is active but there are not
  ; saved files.. we can ignore it.. or try with automatic magic... XD
  if(uAutoRotateFlip) then begin
    ; Verify existence of saved file
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy

    vPath = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]

    filename = strCompress(vPath+strcompress('_Tracking'+ path_sep()+'_ParamSphere'+ path_sep()+'PointsAndAngle.txt', /rem))
    
    vCentroCorreccion = s_CenterCircleBy3PointfromFile(filename = filename, point1 = point1, point2 = point2, point3 = point3, drifted = vDrifted, xyzDim = dimI)
      
    if(vDrifted ne -1) then begin
        ; We are in conditions to Rotate and drift XD
        ; First Rotate all pixels related with the estimated center    
       
        ; Obtain Radius
        vRadius = sqrt(Total((point2 - vCentroCorreccion)*(point2 - vCentroCorreccion)))
        
        ;angle for rotation ... only performs operations at the same plane.... 
            posicionRelativa = point2 - vCentroCorreccion
            posicionRelativa[2] = 0.0
            radio = sqrt(Total(posicionRelativa*posicionRelativa))
            posicionRelativa = posicionRelativa / radio
            vAngle = acos(posicionRelativa[1])
            if(point2[0] gt vCentroCorreccion[0]) then begin
            ; Im not sure if ROT suport negative angles...
            ; otherwise use !PI + (!PI - vAngle ) = 2.0*!PI - vAngle  
              vAngle = -vAngle 
            endif
        
        uDrawElements = 0b
        vMaxToUse = max(image) + 50

        if(uDrawElements eq 1b) then begin
          ; Draw a big DOT XD--- for represent the position of the reference point
          image[(point2[0]-10)>0:(point2[0]+10)<dimI[0],(point2[1]-10)>0:(point2[1]+10)<dimI[1]] = vMaxToUse;  
        endif

       vSemiRadio     = ceil(1.2d * radio)
       vDimNewImage   = ceil(2* (vSemiRadio)) + 1
       vOrientedImage = MAKE_ARRAY(vDimNewImage,vDimNewImage,/DOUBLE, VALUE = mean(image))
       vInitX         = ceil(vSemiRadio + 1 - vCentroCorreccion[0])
       vInitY         = ceil(vSemiRadio + 1 - vCentroCorreccion[1])
       ;vEndX          = 
       ;vEndY          =     
       vOrientedImage[vInitX:vInitX+dimI[0]-1,vInitY:vInitY+dimI[1]-1] = image
       ;vOrientedImage =  ROT(image, vAngle*180.0/!PI,1.0,vCentroCorreccion[0],vCentroCorreccion[1], /INTERP, CUBIC= -0.5, MISSING  = min(image), /PIVOT)
       ;vOrientedImage =  ROT(vOrientedImage, vAngle*180.0/!PI,1.0,vSemiRadio+1,vSemiRadio+1, /INTERP, CUBIC= -0.5, MISSING  = min(image), /PIVOT)
       vOrientedImage =  ROT(vOrientedImage, vAngle*180.0/!PI,1.0,vSemiRadio,vSemiRadio, /INTERP, CUBIC= -0.5, /PIVOT)
    endif

    deltaXY = sqrt(total(1.0d*dimI*dimI))/2.0d
    ;deltaXY = max(dimI)/2.0d
        
    vInitX = vSemiRadio+1
    
    vUp    = vDimNewImage-1
    ;deltaY = vSemiRadio - ceil(radio)
    deltaY = ceil(vSemiRadio - radio) + deltaXY - 100 
    ;deltaY  = deltaXY + 0
    vInitY = vUp - deltaY
     
    ;image  = vOrientedImage[vInitX -floor(deltaXY):vInitX +ceil(deltaXY),vInitY-floor(deltaXY) : vInitY +ceil(deltaXY)]
    image  = vOrientedImage[vInitX -floor(deltaXY):vInitX +ceil(deltaXY),vInitY - floor(deltaXY) : vInitY + ceil(deltaXY)]
    vOrientedImage = 0
  endif 

  ; to save the information related with somites initial position for use in clasification .....  
  return, image
end

function C_sImageFilter_SomitesLocatorOnBF::init
    filterStruct = {Name: 'C_sImageFilter_SomitesLocatorOnBF',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of C_Threshold.
    filterParamWidgetType = make_array(18, /string, value = 'widget_slider')
    filterParamNames = ['Use ZProjection for BF', 'Time BF', 'Channel BF', 'zPosition BF',$
                        'Use automatic rotation/Flip',$
                        'Somite Aperture Angle',$
                        'Somite6_Init_X', 'Somite6_Init_Y',$
                        'Somite7_Init_X', 'Somite7_Init_Y',$
                        'Somite8_Init_X', 'Somite8_Init_Y',$
                        'Somite9_Init_X', 'Somite9_Init_Y',$
                        'D/V division 1 __ Y posisiton',$
                        'D/V division 2 __ Y posisiton',$
                        'D/V division 3 __ Y posisiton',$
                        'Save Reference¨s System',$
                        'Try Automatic Positions']
    filterParamActive = [0, 0, 0, 0,$
                        1,$
                        1,$
                        1, 1,$
                        1,1,$
                        1,1,$
                        1,1,$
                        1,$
                        1,$
                        1,$
                        0,$
                        1]
    filterParamMin = [0, 0, 0, 0,$
                        0,$
                        -90.,$
                        -1000, -1000,$
                        -1000, -1000,$
                        -1000, -1000,$
                        -1000, -1000,$
                        -1000,$
                        -1000,$
                        -1000,$
                        0,$
                        0]
    filterParamMax = [1, 100, 100, 100,$
                        1,$
                        90.,$
                        1000, 1000,$
                        1000, 1000,$
                        1000, 1000,$
                        1000, 1000,$
                        1000,$
                        1000,$
                        1000,$
                        1,$
                        1]
    filterParamValues = [0, 0, 2,0,$
                        1,$
                        45.,$
                        0, 500,$
                        250, 500,$
                        500, 500,$
                        750, 5000,$
                        750,$
                        500,$
                        2500,$
                        1,$
                        1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_SomitesLocatorOnBF__define
  tmp = {C_sImageFilter_SomitesLocatorOnBF, pParamStruct: ptr_new(), inherits C_sImageFilter}
end