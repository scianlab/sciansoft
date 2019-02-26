;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_SkeletonMedialAxis
;
; PURPOSE:
;    Medial axis skeleton filter class.
;
; AUTHOR:
;    Dr. Steffen Härtel (2001)
;    e_mail: shaertel@physik.uni-bremen.de
;    Jorge Jara (2018)
;
; CALLING SEQUENCE:
;   result = obj_new('C_sImageFilter_SkeletonMedialAxis')
;
; METHOHDS:
;   function  ->apply, image = image        ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct  ; pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct  ; pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_SkeletonMedialAxis::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_SkeletonMedialAxis::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

   dimImage  = size(image, /dim) ; dim2[0] x|cols, dim2[1] rows, y|dim2[n] leads to go up in dims... ToDo
   tempImage = image
   roiPoints = where(image gt 0)

   f8conn = 1
   repeat begin
      oldImage = tempImage
      for i = 0l, (n_elements(roiPoints)-1) do begin
         if f8conn $
         then searchPoints = [roiPoints[i]+dimImage[0]-1, roiPoints[i]+dimImage[0], roiPoints[i]+dimImage[0]+1, roiPoints[i]-dimImage[0]-1, roiPoints[i]-dimImage[0], roiPoints[i]-dimImage[0]+1, roiPoints[i]+1, roiPoints[i]-1]$
         else searchPoints = [roiPoints[i]+dimImage[0], roiPoints[i]-dimImage[0], roiPoints[i]+1, roiPoints[i]-1]
         tempImage[roiPoints[i]] = min(tempImage[searchPoints]) + 1
      endfor
   endrep until (min(tempImage eq oldImage) eq 1)

   skelImage = bytArr(dimImage[0], dimImage[1])
   fFlat = 0
   for i = 0l, (n_elements(roiPoints)-1) do begin
      if f8conn $
      then searchPoints = [roiPoints[i]+dimImage[0]-1, roiPoints[i]+dimImage[0], roiPoints[i]+dimImage[0]+1, roiPoints[i]-dimImage[0]-1, roiPoints[i]-dimImage[0], roiPoints[i]-dimImage[0]+1, roiPoints[i]+1, roiPoints[i]-1]$
      else searchPoints = [roiPoints[i]+dimImage[0], roiPoints[i]-dimImage[0], roiPoints[i]+1, roiPoints[i]-1]
      if (tempImage[roiPoints[i]] eq max(tempImage[searchPoints])) then begin
        if fFlat $
        then skelImage[roiPoints[i]] = 1 $
        else skelImage[roiPoints[i]] = tempImage[roiPoints[i]]
      endif
   endfor

   return, skelImage
end


function C_sImageFilter_SkeletonMedialAxis::init

  filterStruct = { $
    Name       : 'C_SkeletonMedialAxis',$ ; Filter Name.
    pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
    pNames     : ptr_new(),$ ; Pointer on Filter Parameter Names.
    pActive    : ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
    pMin       : ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
    pMax       : ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
    pValues    : ptr_new()}  ; Pointer on Filter Parameter Values.

    ; Filter parameters.
  filterParamWidgetType = ['widget_slider']
  filterParamNames      = ['Max_Value_Norm'] ; Normalize Sobel Values -> [0 'no gradient', 1 'perfect']
  filterParamActive     = [1]           ; (active)
  filterParamMin        = [1.]
  filterParamMax        = [ 2.*255.]
  filterParamValues     = [ 2.*255.]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames      = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive     = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin        = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax        = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues     = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct = ptr_new(filterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_SkeletonMedialAxis__define
  tmp = {C_sImageFilter_SkeletonMedialAxis, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
