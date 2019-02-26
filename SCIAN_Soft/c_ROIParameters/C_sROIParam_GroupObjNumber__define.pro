;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_GroupObjNumber
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_GroupObjNumber' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_GroupObjNumber::apply, number = number
    whParam = [(where( *(*self.pParamStruct).pNames eq 'Group Object Number'))[0]]
    if (n_elements(number) gt 0) then begin
       if ptr_valid((*(*self.pValueStruct)[whParam[0]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[0]]).pROIParamVect
       (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( number, /no_copy)
    endif
end

pro C_sROIParam_GroupObjNumber::setpValueStruct, pValueStruct
    whereValueStructName = (where( *(*self.pParamStruct).pNames eq *pValueStruct.name))[0]
    if ptr_valid((*self.pValueStruct)[position]) then ptr_free, (*self.pValueStruct)[position]
    (*self.pValueStruct)[position] = ptr_new(*pValueStruct, /no_copy)
end
function C_sROIParam_GroupObjNumber::getpValueStruct, position = position
    if (ptr_valid((*self.pValueStruct)[position])) then return, (*self.pValueStruct)[position]  else return, -1
end
pro C_sROIParam_GroupObjNumber::setpParamStruct, pParamStruct
    if ptr_valid(self.pParamStruct) then ptr_free, self.pParamStruct
    self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
function C_sROIParam_GroupObjNumber::getpParamStruct
   return, self.pParamStruct
end

pro C_sROIParam_GroupObjNumber::cleanup
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


function C_sROIParam_GroupObjNumber::init

    ROIParamStruct = {name:'Group Object Number',$   ;  ROI Name.
                    type:   'Group ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Group Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Group Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Group Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Group Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Group Parameter Max_Values.
                    pValues:ptr_new()}       ; Pointer on ROI-Group Parameter Values.

    self.pValueStruct = ptr_new(ptrArr(1))

    ROIParamWidgetType = ['widget_slider']
    ROIParamNames = ['Group Object Number']
    ROIParamActive = [1]
    ROIParamMin = [0]
    ROIParamMax = [0]
    ROIParamValues = [0]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name: 'Group Object Number',$
                    type: 'Group ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Group Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Group Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Group Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Group Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Group Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Group Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Group Parameter Vector.

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueActive = [0,0,$
                        0,0,$
                        0,0,$
                        0,0 ]
    ROIValueMin = [0., 0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
    ROIValueMax = [1., 1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    ROIValueValues =[0., 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]
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

pro C_sROIParam_GroupObjNumber__define
   tmp = {C_sROIParam_GroupObjNumber, pParamStruct: ptr_new(),$
                                  pValueStruct: ptr_new()}
end