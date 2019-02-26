;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_GroupPhase
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_GroupPhase' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_GroupPhase::apply, whereXY = whereXY, mask = mask, position = position, xySizePerPixel = xySizePerPixel, stack_tlb = stack_tlb

    whParam = [ (where( *(*self.pParamStruct).pNames eq 'Phase Area [%]'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Phase Area [x²]'))[0] ,$
                 (where( *(*self.pParamStruct).pNames eq 'Phase Area Lipid [Mol%]'))[0] ]
    whParamActive = whParam * 0
    if (n_elements(position) gt 0) then begin
       if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[i] = 1
    endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

    if whParamActive[0] then begin
       if ptr_valid((*(*self.pValueStruct)[whParam[0]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[0]]).pROIParamVect
       whereSubPhase = where(mask eq 0, complement = wherePhase)
       if (total(whereSubPhase) eq -1) then parameter = 100.
       if (total(wherePhase) eq -1) then parameter = 0.
       if ((total(whereSubPhase) ne -1) and (total(wherePhase) ne -1)) then parameter = 100.*n_elements(wherePhase) / (n_elements(whereSubPhase)+n_elements(wherePhase))
       (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( parameter, /no_copy)
    endif
    if whParamActive[1] then begin
       if ptr_valid((*(*self.pValueStruct)[whParam[1]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[1]]).pROIParamVect
       (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
       (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]

       dimMask = size(mask, /dim)
       whereSubPhase = where(mask eq 0, complement = wherePhase)
       if (total(whereSubPhase) eq -1) then parameter = 100.
       if (total(wherePhase) eq -1) then parameter = 0.
       if ((total(whereSubPhase) ne -1) and (total(wherePhase) ne -1)) then parameter = 100.*n_elements(wherePhase) / (n_elements(whereSubPhase)+n_elements(wherePhase))

       totalArea = (dimMask[0] *xySizePerPixel[0]) * (dimMask[1] *xySizePerPixel[1])
       parameter = totalArea * parameter / 100.
       (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( parameter, /no_copy)
    endif
    if whParamActive[2] then begin
       if ptr_valid((*(*self.pValueStruct)[whParam[2]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[2]]).pROIParamVect
       (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
       (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]

       lipidArea1 = (*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'Lipid 1: Area/Lipid [Ang²]'))[0]]
       lipidArea2 = (*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'Lipid 2: Area/Lipid [Ang²]'))[0]]
       inverseActive = (*(*(*self.pValueStruct)[whParam[2]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'Inverse Phase'))[0]]

       dimMask = size(mask, /dim)
       whereSubPhase = where(mask eq 0, complement = wherePhase)
       if (total(whereSubPhase) eq -1) then parameter = 100.
       if (total(wherePhase) eq -1) then parameter = 0.
       if ((total(whereSubPhase) ne -1) and (total(wherePhase) ne -1)) then parameter = 1. * n_elements(wherePhase) / (n_elements(whereSubPhase)+n_elements(wherePhase))

       totalArea = (dimMask[0] *xySizePerPixel[0]) * (dimMask[1] *xySizePerPixel[1])
       totalArea1 = totalArea * parameter
       totalArea2 = totalArea * (1.-parameter)

       numLipid1 = 1. * totalArea1 / lipidArea1
       numLipid2 = 1. * totalArea2 / lipidArea2
       percLipid1 = 100. * numLipid1 / (numLipid1+numLipid2)

       if (inverseActive) then parameter = 100.-percLipid1 else parameter = percLipid1

       (*(*self.pValueStruct)[whParam[2]]).pROIParamVect = ptr_new( parameter, /no_copy)
    endif
end

pro C_sROIParam_GroupPhase::setpValueStruct, pValueStruct
    whereValueStructName = (where( *(*self.pParamStruct).pNames eq *pValueStruct.name))[0]
    if ptr_valid((*self.pValueStruct)[position]) then ptr_free, (*self.pValueStruct)[position]
    (*self.pValueStruct)[position] = ptr_new(*pValueStruct, /no_copy)
end
function C_sROIParam_GroupPhase::getpValueStruct, position = position
    if (ptr_valid((*self.pValueStruct)[position])) then return, (*self.pValueStruct)[position]  else return, -1
end
pro C_sROIParam_GroupPhase::setpParamStruct, pParamStruct
    if ptr_valid(self.pParamStruct) then ptr_free, self.pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
function C_sROIParam_GroupPhase::getpParamStruct
   return, self.pParamStruct
end

pro C_sROIParam_GroupPhase::cleanup
    for i = 0, n_tags((*self.pParamStruct))-1 do begin
        case size((*self.pParamStruct).(i), /tname) of
            'POINTER': ptr_free, (*self.pParamStruct).(i)
            'OBJREF': obj_destroy, (*self.pParamStruct).(i)
            else:
        endcase
     endfor
    for j = 0, n_elements(*self.pValueStruct)-1 do begin
       for i = 0, n_tags((*(*self.pValueStruct)[j]))-1 do begin
         case size((*(*self.pValueStruct)[j]).(i), /tname) of
               'POINTER': ptr_free, (*(*self.pValueStruct)[j]).(i)
               'OBJREF': obj_destroy, (*(*self.pValueStruct)[j]).(i)
               else:
         endcase
        endfor
        ptr_free, (*self.pValueStruct)[j]
    endfor
    ptr_free, self.pValueStruct
    ptr_free, self.pParamStruct
end


function C_sROIParam_GroupPhase::init

    ROIParamStruct = {name:'Group Phase',$   ;  ROI Name.
                    type:   'Group ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Group Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Group Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Group Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Group Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Group Parameter Max_Values.
                    pValues:ptr_new() }         ; Pointer on ROI-Group Parameter Values.


    self.pValueStruct = ptr_new(ptrArr(3))
    ROIParamWidgetType = ['widget_slider',$
                            'widget_slider',$
                            'widget_slider']
    ROIParamNames = [ 'Phase Area [%]',$
                    'Phase Area [x²]',$
                    'Phase Area Lipid [Mol%]' ]
    ROIParamActive = [1,0,0]
    ROIParamMin = [0,0,0]
    ROIParamMax = [0,0,0]
    ROIParamValues = [0,0,0]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name: 'Phase Area [%]',$
                    type: 'Group ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Group Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Group Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Group Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Group Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Group Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Group Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Group Parameter Vector.


    ROIThresValueWidgetType = ['widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider',$
                        'widget_slider', 'widget_slider']
    ROIThresValueNames = ['Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIThresValueActive = [0,0,$
                        0,0,$
                        0,0,$
                        0,0 ]
    ROIThresValueMin = [0., 0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
    ROIThresValueMax = [1., 1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    ROIThresValueValues =[0., 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIThresValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIThresValueNames)
    ROIValueStruct.pActive = ptr_new(ROIThresValueActive)
    ROIValueStruct.pMin = ptr_new(ROIThresValueMin)
    ROIValueStruct.pMax = ptr_new(ROIThresValueMax)
    ROIValueStruct.pValues = ptr_new(ROIThresValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Phase Area [x²]',$
                    type: 'Group ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Group Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Group Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Group Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Group Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Group Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Group Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Group Parameter Vector.

    ROIValueWidgetType = ['widget_slider', 'widget_slider', ROIThresValueWidgetType]
    ROIValueNames = ['x-Size per Pixel', 'y-Size per Pixel', ROIThresValueNames]
    ROIValueActive = [1, 1, ROIThresValueActive]
    ROIValueMin = [0., 0., ROIThresValueMin]
    ROIValueMax = [1000., 1000., ROIThresValueMax]
    ROIValueValues =[1., 1., ROIThresValueValues]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Phase Area Lipid [Mol%]',$
                    type: 'Group ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Group Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Group Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Group Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Group Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Group Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Group Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Group Parameter Vector.

    ROIValueWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', ROIThresValueWidgetType]
    ROIValueNames = ['Lipid 1: Area/Lipid [Ang²]', 'Lipid 2: Area/Lipid [Ang²]', 'Inverse Phase', ROIThresValueNames]
    ROIValueActive = [1, 1, 0, ROIThresValueActive]
    ROIValueMin = [0., 0., 0., ROIThresValueMin]
    ROIValueMax = [500, 500, 1., ROIThresValueMax]
    ROIValueValues =[51., 84., 0., ROIThresValueValues]
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

pro C_sROIParam_GroupPhase__define
   tmp = {C_sROIParam_GroupPhase, pParamStruct: ptr_new(),$
                                  pValueStruct: ptr_new()}
end