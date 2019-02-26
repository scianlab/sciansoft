;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_TouchBorder
;
; PURPOSE:
;       - TouchBorder-Filter-Class. See TouchBorder.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;	    modified by MOR - takes in pixel size of border to eliminate
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_TouchBorder' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct        ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct        ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_TouchBorder::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_TouchBorder::apply, image = image

    if (size(image, /n_dim) ne 2) then return, image
    whParam = (where((*(*self.pParamStruct).pNames) eq 'TouchBorder_OnOff'))[0]
   
    if ((*(*self.pParamStruct).pActive)[whParam]) then begin
       pixelBorderSize = (*(*self.pParamStruct).pValues)[whParam]
       dimI = size(image, /dim)
       image = image ne 0
       alpha = s_apop_flaeche(image, revIndizes = revIndizes)
       left = make_array(pixelBorderSize, /index, /integer)

       image[ [ left,  fix((dimI[0]-1) - left)], *] = 2
       image[*, [left, fix((dimI[1]-1) - left)]] = 2

       for i = 0, max(alpha)-1 do begin
         whereBorder = where(image[ revIndizes[ revIndizes[i] : revIndizes[i+1]-1 ] ] eq 2)
         if (whereBorder[0] ne -1) then image[ revIndizes[ revIndizes[i] : revIndizes[i+1]-1 ] ] = 0
       endfor

       image[ [ left,  fix((dimI[0]-1) - left)], *] = 0 
       image[*, [left, fix((dimI[1]-1) - left)]] = 0
    endif

    return, image
end


function C_sImageFilter_TouchBorder::init

    filterStruct = {Name: 'C_TouchBorder',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of TouchBorder.
    filterParamWidgetType = ['widget_slider']
    filterParamNames = ['TouchBorder_OnOff']
    filterParamActive = [1]
    filterParamMin = [0]
    filterParamMax = [10000]
    filterParamValues = [1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_TouchBorder__define
  tmp = {C_sImageFilter_TouchBorder, pParamStruct: ptr_new(), inherits C_sImageFilter}
end