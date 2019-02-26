;_____________________________IOISIOI_____________________________
; NAME:
;      C_sImageFilter_OpenCloseAdvanced
;
; PURPOSE:
;      Performs morphological operations (dilate, erode, open, close) with disk-like structuring element of desired size.
;      Can be used on binary images and on gray-scale images.
;       - OpenCloseAdvanced-Filter-Class.
;
; AUTHOR:
;     Jan Scheer (2011)
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_OpenClose')
;
; METHOHDS:
;   function ->apply, pImageData = pImageData      ;pImageData   Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI_____________________________

function C_sImageFilter_OpenCloseAdvanced::getImageFilterType
  return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_OpenCloseAdvanced::apply, image = image

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Pixel_Neighbor'))[0]
  (*(*self.pParamStruct).pValues)[whParam] = (*(*self.pParamStruct).pValues)[whParam] > 1
  pixelNeighbor = (*(*self.pParamStruct).pValues)[whParam]

  fErode  = 0
  fDilate = 0
  fOpen   = 0
  fClose  = 0
  fGrey   = 0
  structure = 0

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Erode'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) then fErode = 1

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Dilate'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) then fDilate = 1

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Open'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) then fOpen = 1

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Close'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) then fClose = 1

  if (fErode eq 0) and (fDilate eq 0) and (fOpen eq 0) and (fClose eq 0) then return, image

  whParam = (where((*(*self.pParamStruct).pNames) eq 'Disk->0_Square->1_vLine->2_hLine->3_diagLine->4'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) then structure = (*(*self.pParamStruct).pValues)[whParam]

  whParam = (where((*(*self.pParamStruct).pNames) eq 'GreyScale'))[0]
  if ((*(*self.pParamStruct).pActive)[whParam]) then fGrey = 1

  dims      = size(image, /DIMENSION)
  radius    = pixelNeighbor
  structure = round(structure)

  case structure of
    0: strucElem = shift(dist(2*radius+1), radius, radius) le radius ; disk
    1: strucElem = dist(radius) le radius ; square
    2: strucElem = bytArr(radius, radius) ; vertical line
    3: strucElem = bytArr(radius, radius) ; horizontal line
    4: strucElem = byte(identity(radius)) ; det line
  endcase

  if (structure eq 2) then strucElem[0,*] = 1
  if (structure eq 3) then strucElem[*,0] = 1

  print, strucElem
  morph = image

  if (fErode) then begin
    if (fGrey eq 0) $
    then erodeImg = replicate(1B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor) $
    else erodeImg = replicate(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)
    erodeImg[pixelNeighbor, pixelNeighbor] = image
    padDims = size(erodeImg, /DIMENSIONS)
    morph   = erode(erodeImg, strucElem, /GRAY)
  endif

  if (fDilate) then begin
    if (fGrey eq 0) $
    then dilateImg = replicate(0B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor) $
    else dilateImg = replicate(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)
    dilateImg[pixelNeighbor,pixelNeighbor] = image
    padDims = size(dilateImg, /DIMENSIONS)
    morph   = dilate(dilateImg, strucElem, /GRAY)
  endif

  if (fOpen) then begin
    if (fGrey eq 0) $
    then openImg = replicate(1B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor) $
    else openImg = replicate(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)
    openImg[pixelNeighbor,pixelNeighbor] = image
    padDims = size(openImg, /DIMENSIONS)
    morph   = morph_open(openImg, strucElem, /GRAY)
  endif

  if (fClose) then begin
    if (fGrey eq 0) $
    then closeImg = replicate(1B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor) $
    else dilateImg = replicate(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)
    closeImg[pixelNeighbor, pixelNeighbor] = image
    padDims = size(closeImg, /DIMENSIONS)
    morph   = morph_close(closeImg, strucElem, /GRAY)
  endif

  morph = morph([pixelNeighbor] : [dims[0] - 1 + pixelNeighbor],$
                [pixelNeighbor] : [dims[1] - 1 + pixelNeighbor])
  return, morph

end


pro C_sImageFilter_OpenCloseAdvanced::set, pParamStruct = pParamStruct
  self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end


pro C_sImageFilter_OpenCloseAdvanced::get, pParamStruct = pParamStruct
  pParamStruct = self.pParamStruct
end


function C_sImageFilter_OpenCloseAdvanced::getpParamStruct
  return, self.pParamStruct
end


pro C_sImageFilter_OpenCloseAdvanced::cleanup

  for i = 0, n_tags((*self.pParamStruct))-1 do begin
    case size((*self.pParamStruct).(i), /tname) of
      'POINTER': ptr_free, (*self.pParamStruct).(i)
      'OBJREF' : obj_destroy, (*self.pParamStruct).(i)
    else:
    endcase
  endfor
  ptr_free, self.pParamStruct

end


function C_sImageFilter_OpenCloseAdvanced::init
  filterStruct = {Name       : 'C_OpenCloseAdvanced',$ ; Filter Name.
                  pWidgetType: ptr_new(),$  ; Pointer on Filter Parameter Names.
                  pNames     : ptr_new(),$  ; Pointer on Filter Parameter Names.
                  pActive    : ptr_new(),$  ; Pointer on Filter Parameter Active Bool.
                  pMin       : ptr_new(),$  ; Pointer on Filter Parameter Min_Values.
                  pMax       : ptr_new(),$  ; Pointer on Filter Parameter Max_Values.
                  pValues    : ptr_new()}   ; Pointer on Filter Parameter Values.

    ; Parameters of OpenCloseAdvanced.
  filterParamNames = ['Erode',$
                      'Dilate',$
                      'Open',$
                      'Close',$
                      'Pixel_Neighbor',$
                      'Disk->0_Square->1_vLine->2_hLine->3_diagLine->4',$
                      'GreyScale']

  filterParamWidgetType = make_array(n_elements(filterParamNames), /string, value = 'widget_slider')

  filterParamActive = [1 ,0,0,0,1  ,1,0]
  filterParamMin    = [0 ,0,0,0,1  ,0,0]
  filterParamMax    = [1 ,1,1,1,100,4,1]
  filterParamValues = [1.,0,0,0,1  ,0,0]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames      = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive     = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin        = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax        = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues     = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct = ptr_new(filterStruct, /no_copy)
  return, 1
end

pro C_sImageFilter_OpenCloseAdvanced__define
  tmp = {C_sImageFilter_OpenCloseAdvanced, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
