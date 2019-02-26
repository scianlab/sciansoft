;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_SizeSelect
;
; PURPOSE:
;       - Fill-Filter-Class. See Fill.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2004)
;     e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_SizeSelect' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_v____________________________IOISIOI____________________

function C_sImageFilter_SizeSelect::getImageFilterType
   return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_SizeSelect::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    image = temporary(image) ne 0

    whfAll = (where((*(*self.pParamStruct).pNames) eq 'Use 8 neighbors'))[0]
    fAllNeighbors = whfAll ne -1 ? ((*(*self.pParamStruct).pActive)[whfAll]) : 0b

    alpha = s_apop_flaeche(image, setAreaAsPixelSize = 1, fAllNeighbors = fAllNeighbors)

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Exclusive'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then fExcl = 1b else fExcl = 0b

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Show Size Only'))[0]
    if (whParam ne -1) then if ((*(*self.pParamStruct).pActive)[whParam]) then return, alpha

    whLE = (where((*(*self.pParamStruct).pNames) eq 'Select_<='))[0]
    whGE = (where((*(*self.pParamStruct).pNames) eq 'Select_>='))[0]

    if ((*(*self.pParamStruct).pActive)[whLE]) or ((*(*self.pParamStruct).pActive)[whGE]) then begin
       ((*(*self.pParamStruct).pActive)[whLE]) = 1b
       ((*(*self.pParamStruct).pActive)[whGE]) = 1b
       srMin = (*(*self.pParamStruct).pValues)[whLE]
       srMax = (*(*self.pParamStruct).pValues)[whGE]
       if fExcl then return, (alpha gt 0) * ((alpha ge srMax) * (alpha le srMin)) $
          else return, ( (alpha gt 0) * ((alpha ge srMax) + (alpha le srMin)) ) gt 0
    endif

    return, alpha
end


function C_sImageFilter_SizeSelect::init

    filterStruct = {Name: 'C_SizeSelect',$          ; Filter Name.
                           pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$      ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$     ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$        ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$        ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}      ; Pointer on Filter Parameter Values.

    filterParamNames  = ['Select_<=', 'Select_>=', 'Exclusive', 'Show Size Only', 'Use 8 neighbors']
    filterParamActive = [1,     1,     0, 0, 0]
    filterParamMin    = [0,     0,     0, 0, 0]
    filterParamMax    = [50000, 50000, 1, 1, 1]
    filterParamValues = [0,     10000, 1, 0, 0]
    filterParamWidgetType = make_array(n_elements(filterParamNames), /string, value = 'widget_slider')

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_SizeSelect__define
  tmp = {C_sImageFilter_SizeSelect, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
