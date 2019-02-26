;_____________________________IOISIOI_____________________________
; NAME:
;     C_sImageFilter_FillRemove
;
; PURPOSE:
;     Fill-Filter-Class. See Fill.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;     result = obj_new('C_sImageFilter_FillRemove')
;
; METHOHDS:
;_____________________________IOISIOI_____________________________

function C_sImageFilter_FillRemove::getImageFilterType
  return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_FillRemove::apply, image = image

  if (size(image, /n_dim) ne 2) then return, image

  image = temporary(image) ne 0
  alpha = s_apop_flaeche(image eq 0, setAreaAsPixelSize = 1)

  whereOmit = (where((*(*self.pParamStruct).pNames) eq 'Omit_Background'))[0]
  if ((*(*self.pParamStruct).pActive)[whereOmit]) then alpha = alpha * (alpha lt max(alpha))

  where8n = (where((*(*self.pParamStruct).pNames) eq '8_Neighbors'))[0]
  use8n   = (*(*self.pParamStruct).pValues)[where8n] gt 0

  wherePixelSize = (where((*(*self.pParamStruct).pNames) eq 'Fill_Pixel_Size'))[0]
  if (*(*self.pParamStruct).pActive)[wherePixelSize] then begin
    alpha = temporary(alpha) lt (round((*(*self.pParamStruct).pValues)[wherePixelSize])) + image
    alphaFilled = 1
  endif

  wherePixelSize = (where((*(*self.pParamStruct).pNames) eq 'Remove_Pixel_Size'))[0]
  if (*(*self.pParamStruct).pActive)[wherePixelSize] then begin
    if (n_elements(alphaFilled) ne 0) $
    then alpha = s_apop_flaeche(alpha ne 0, setAreaAsPixelSize = 1, fAllNeighbors = use8n) gt (round((*(*self.pParamStruct).pValues)[wherePixelSize])) $
    else alpha = s_apop_flaeche(alpha eq 0, setAreaAsPixelSize = 1, fAllNeighbors = use8n) gt (round((*(*self.pParamStruct).pValues)[wherePixelSize]))
  endif

  return, alpha
end


function C_sImageFilter_FillRemove::init

  filterStruct = {Name:'C_FillRemove',$    ; Filter Name.
                  pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pNames:      ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pActive:     ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                  pMin:        ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
                  pMax:        ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                  pValues:     ptr_new()}  ; Pointer on Filter Parameter Values.

  filterParamNames      = ['Fill_Pixel_Size', 'Remove_Pixel_Size', 'Omit_Background', '8_Neighbors']
  filterParamWidgetType = make_array(n_elements(filterParamNames), /string, value = 'widget_slider')
  filterParamActive     = [1, 1, 0, 1]
  filterParamMin        = [0L, 0L,0L, 0L]
  filterParamMax        = [2147483647L, 2147483647L, 1L, 1L]
  filterParamValues     = [50L, 0L, 0L, 0L]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames  = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin    = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax    = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct    = ptr_new(filterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_FillRemove__define
  tmp = {C_sImageFilter_FillRemove, pParamStruct:ptr_new(), inherits C_sImageFilter}
end
