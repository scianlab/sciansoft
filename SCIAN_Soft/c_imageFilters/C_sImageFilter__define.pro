;_____________________________IOISIOI____________________
; NAME:
;     C_sImageFilter
;
; PURPOSE:
;     Superclass for sImageFilters
;
; AUTHOR:
;     Dr. Steffen Härtel (2005)
;     e_mail: shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;     result = obj_new('C_sImageFilter')
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter::getImageFilterDevelopmentalState
    return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

pro C_sImageFilter::set, pParamStruct = pParamStruct
  *self.pParamStruct = *pParamStruct
end


pro C_sImageFilter::get, pParamStruct = pParamStruct
  pParamStruct = self.pParamStruct
end


function C_sImageFilter::getpParamStruct
  return, self.pParamStruct
end


pro C_sImageFilter::cleanup
  for i = 0,n_tags((*self.pParamStruct))-1 do begin
    case size((*self.pParamStruct).(i), /tname) of
    'POINTER':ptr_free, (*self.pParamStruct).(i)
    'OBJREF':obj_destroy, (*self.pParamStruct).(i)
    else:
    endcase
  endfor
  ptr_free, self.pParamStruct
end


function C_sImageFilter::init
  return, 1
end


pro C_sImageFilter__define
  tmp = {C_sImageFilter, a: 1b}
end