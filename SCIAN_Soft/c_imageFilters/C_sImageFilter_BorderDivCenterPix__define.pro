;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_BorderDivCenterPix
;
; PURPOSE:
;       - BorderDivCenterPix-Filter-Class. See BorderDivCenterPix.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_BorderDivCenterPix' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_BorderDivCenterPix::getImageFilterType
  return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_BorderDivCenterPix::apply, image = image
  if (size(image, /n_dim) ne 2) then return, image
  if (max(image) eq 0) then return, image

  image = temporary(image) ne 0
  image *= 1.

  borderDepth = 3
  i = 2

  case ((*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Border_1->thin_2->thick'))[0]]) of

    1: begin
      whPeriMask = where(s_ObjBorder_4or8((image eq 1), /eight))
      while ((whPeriMask[0] ne -1) and (i lt borderDepth)) do begin
        image[whPeriMask] = i
        i += 1
        whPeriMask = where(s_ObjBorder_4or8((image eq 1), /eight))
      endwhile
    endcase

    else: begin
      whPeriMask = where(s_ObjBorder_4or8((image eq 1), /eight))
      while ((whPeriMask[0] ne -1) and (i lt borderDepth)) do begin
        image[whPeriMask] = i
        whPeriMask = where(s_ObjBorder_4or8((image eq 1), /four))
        if (whPeriMask[0] ne -1) then begin
          image[whPeriMask] = i
          i += 1
          whPeriMask = where(s_ObjBorder_4or8((image eq 1), /eight))
        endif
      endwhile
    endcase

  endcase

  whPeriMask = where(image eq 1, count)
  if (count gt 0) then image[whPeriMask] = i

    ; calculate areas in pixel
  alpha = s_apop_flaeche(image, setAreaAsPixelSize = 1, revIndizes = revIndizes, nObj = nObj)
    ; calculate perimeters in pixel
  alphaII = s_apop_flaeche(image eq 2, setAreaAsPixelSize = 1)
    ; square perimeters
  alphaII *= alphaII
    ; normalize by areas
  alphaII /= alpha

    ; set entire region to values
  for i = 0l, nObj-1 do image[[revIndizes[revIndizes[i]:revIndizes[i+1]-1]]] = alphaII[[revIndizes[revIndizes[i]]]]

  return, image
end


function C_sImageFilter_BorderDivCenterPix::init

    filterStruct = {Name: 'C_BorderDivCenterPix',$  ; Filter Name.
                           pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$      ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$     ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$        ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$        ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}      ; Pointer on Filter Parameter Values.

       ; Parameters of BorderDivCenterPix.
    filterParamWidgetType = make_array(1, /string, value = 'widget_slider')
    filterParamNames = ['Border_1->thin_2->thick']
    filterParamActive = [1]
    filterParamMin = [1]
    filterParamMax = [2]
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

pro C_sImageFilter_BorderDivCenterPix__define
  tmp = {C_sImageFilter_BorderDivCenterPix, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
