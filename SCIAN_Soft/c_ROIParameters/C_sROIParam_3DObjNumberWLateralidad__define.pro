;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjNumberWLateralidad
;
; PURPOSE:
;       - Anota una buena descripcion Felipe!!!
;
; AUTHOR:
;     Felipe Santibañez (2011)
;     e_mail: fsantibanez@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjNumberWLateralidad' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_3DObjNumberWLateralidad::apply, C_sROI3DGroupObj = C_sROI3DGroupObj,  position = position

    whParam = [(where( *(*self.pParamStruct).pNames eq '3D Object Number With Lateralidad [N]'))[0] ]

       ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1: if (position[0] eq -1) then return else  whParamActive[position] = 1
       else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase
    wherePA = where(whParamActive eq 1)

       ; check Pointers
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROI3DGroupObj->count()
    if (nObjects lt 1) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin
             ; set Object Number Vector
           *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()
    
           if whParamActive[0] then $
             *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = C_sROI3DGroupObj->getObjectNumberVector()

          if( (*(*(*self.pValueStruct)[whParam[0]]).pValues)[8] eq 1)then begin
             (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[*] = 1
          endif
             
          originalCellsNumber = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[8]
           if((originalCellsNumber  gt 1) and (originalCellsNumber  lt nObjects)) then begin
              for f = 0, nObjects-1 do begin
                if(f lt originalCellsNumber) then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[f] = floor((f)/(2*originalCellsNumber)*(nObjects-2)) $
                else begin
                    if (nObjects eq (originalCellsNumber+1)) then (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[f] = nObjects-1 else $
                   (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect)[f] = floor((f-originalCellsNumber-1)*(nObjects-4)/(2*(nObjects-originalCellsNumber-1))+(nObjects+2)/2)
                endelse
              endfor                        
           endif   
    endelse
end


function C_sROIParam_3DObjNumberWLateralidad::init

    ROIParamStruct = {name:'3D Object Number With Lateralidad',$     ;  ROI Name.
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(1))
    ROIParamWidgetType = ['widget_slider']
    ROIParamNames = ['3D Object Number With Lateralidad [N]']
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

    ROIValueStruct = {name:'3D Object Number With Lateralidad',$
                      type:'3D ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType =make_array(9, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                          'Threshold_2a', 'Threshold_2b',$
                          'Threshold_3a', 'Threshold_3b',$
                          'Threshold_4a', 'Threshold_4b',$
                          'Initial Nodes']  
    ROIValueActive = [0,0,0,0,0,0,0,0,          1]
    ROIValueMin = [0.,0.,0.,0.,0.,0.,0.,0.,     0]
    ROIValueMax = [1.,1.,1.,1.,1.,1.,1.,1.,     100]
    ROIValueValues =[0.,1.,0.,1.,0.,1.,0.,1.,   0]
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

pro C_sROIParam_3DObjNumberWLateralidad__define
   tmp = {C_sROIParam_3DObjNumberWLateralidad, pParamStruct:ptr_new(),pValueStruct:ptr_new(),inherits C_sROIParam}
end