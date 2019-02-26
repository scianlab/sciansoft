;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Cut
;
; PURPOSE:
;       - Cut-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Cut' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________


function C_sImageFilter_Cut::getImageFilterType
   return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_Cut::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   dimI = size(image, /dim)
   whParam = (where((*(*self.pParamStruct).pNames) eq 'xMin_Position'))[0]
      xMin_Position = (0 > (*(*self.pParamStruct).pValues)[whParam]) < ((*(*self.pParamStruct).pValues)[whParam] + (dimI[0]-1) - 1)
      (*(*self.pParamStruct).pValues)[whParam] = xMin_Position
   whParam = (where((*(*self.pParamStruct).pNames) eq 'xMax_Position'))[0]
      xMax_Position = ( ((*(*self.pParamStruct).pValues)[whParam] + (dimI[0]-1) - 1) < (*(*self.pParamStruct).pValues)[whParam]) > (xMin_Position)
      if ( (dimI[0]-1) gt (*(*self.pParamStruct).pValues)[whParam]) then (*(*self.pParamStruct).pValues)[whParam] = xMax_Position
   whParam = (where((*(*self.pParamStruct).pNames) eq 'yMin_Position'))[0]
      yMin_Position = (0 > (*(*self.pParamStruct).pValues)[whParam]) < ((*(*self.pParamStruct).pValues)[whParam] + (dimI[1]-1) - 1)
      (*(*self.pParamStruct).pValues)[whParam] = yMin_Position
   whParam = (where((*(*self.pParamStruct).pNames) eq 'yMax_Position'))[0]
      yMax_Position = ((*(*self.pParamStruct).pValues)[whParam] + (dimI[1]-1) - 1) < (*(*self.pParamStruct).pValues)[whParam] > (yMin_Position)
      if ( (dimI[1]-1) gt (*(*self.pParamStruct).pValues)[whParam]) then  (*(*self.pParamStruct).pValues)[whParam] = yMax_Position

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Real Cut on off'))[0]
      if ((*(*self.pParamStruct).pActive)[whParam]) then fCut = 1b else fCut = 0b

   if fCut then return, image[xMin_Position < (dimI[0]-2): xMax_Position < (dimI[0]-1), yMin_Position < (dimI[1]-2): yMax_Position < (dimI[1]-1)]

   image[0: xMin_Position < (dimI[0]-2), *] = 0
   image[xMax_Position < (dimI[0]-1): *, *] = 0

   image[*, 0: yMin_Position < (dimI[1]-2)] = 0
   image[*, yMax_Position < (dimI[1]-1): *] = 0

   return, image
end


function C_sImageFilter_Cut::init

    filterStruct = {Name: 'C_Cut',$   ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pImageFilterDataIn:ptr_new(),$       ; Pointer on original Data.
                           pImageFilterDataOut:ptr_new(),$     ; Pointer on filtered Data.
                           pNames:ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}     ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(5, /string, value = 'widget_slider')
    filterParamNames = ['xMin_Position',$   ;   Pixel position of the left cut position.
                        'xMax_Position',$   ;   Pixel position of the right cut position.
                        'yMin_Position',$   ;  Pixel position of the up position.
                        'yMax_Position',$   ;   Pixel position of the down cut position.
                        'Real Cut on off']    ;   Pixel position of the down cut position.

    filterParamActive = [1,1,1,1,1]
    filterParamMin = [0.,1.,0.,1.,0.]
    filterParamMax = [10000, 10000, 10000, 10000,1]
    filterParamValues = [0.,1.,0,1.,1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Cut__define
   tmp = {C_sImageFilter_Cut, pParamStruct: ptr_new(), inherits C_sImageFilter}
end