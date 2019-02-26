;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_BorderDistance
;
; PURPOSE:
;       - BorderDistance-Filter-Class. See BorderDistance.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_BorderDistance' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_BorderDistance::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_BorderDistance::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Border_Depth'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then borderDepth = (*(*self.pParamStruct).pValues)[whParam] + 1 $
       else borderDepth = max(size(image, /dim))

    image gt= 0
    i = 2.
    case ((*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Connectivity_4_8_or_else'))[0]]) of
       4: begin
          whPeriMask = where( s_ObjBorder_4or8((image eq 1), /four))
          while ((whPeriMask[0] ne -1) and (i lt borderDepth)) do begin
              image[whPeriMask] = i
              i += 1
              whPeriMask = where( s_ObjBorder_4or8((image eq 1), /four))
          endwhile
         endcase
       8: begin
          whPeriMask = where( s_ObjBorder_4or8((image eq 1), /eight))
          while ((whPeriMask[0] ne -1) and (i lt borderDepth)) do begin
              image[whPeriMask] = i
              i += 1
              whPeriMask = where( s_ObjBorder_4or8((image eq 1), /eight))
          endwhile
         endcase
       else: begin
          whPeriMask = where( s_ObjBorder_4or8((image eq 1), /eight))
          while ((whPeriMask[0] ne -1) and (i lt borderDepth)) do begin
              image[whPeriMask] = i
              whPeriMask = where( s_ObjBorder_4or8((image eq 1), /four))
              if (whPeriMask[0] ne -1) then begin
                 image[whPeriMask] = i
                 i += 1
                 whPeriMask = where( s_ObjBorder_4or8((image eq 1), /eight))
              endif
          endwhile
       endcase
    endcase

    whPeriMask = where(image eq 1, count)
    if (count gt 0) then image[whPeriMask] = i
    return, image
end


function C_sImageFilter_BorderDistance::init

    filterStruct = {Name: 'C_BorderDistance',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of BorderDistance.
    filterParamWidgetType = make_array(2, /string, value = 'widget_slider')
    filterParamNames = ['Connectivity_4_8_or_else', 'Border_Depth']
    filterParamActive = [1,0]
    filterParamMin = [4,0]
    filterParamMax = [8,1000]
    filterParamValues = [8,10]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_BorderDistance__define
  tmp = {C_sImageFilter_BorderDistance, pParamStruct: ptr_new(), inherits C_sImageFilter}
end