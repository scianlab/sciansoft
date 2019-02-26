;_____________________________IOISIOI____________________
; NAME:
;      NC_C_sImageFilter_OpenCloseAdvanced
;      Performs morphological operations (dilate, erode, open, close) with disk-like structuring element of desired size
;      Can be used on Binary Images and on GrayScale Images
; PURPOSE:
;       - OpenClose-Filter-Class. See OpenClose.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_OpenClose' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData             ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function NC_C_sImageFilter_OpenCloseAdvanced::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function NC_C_sImageFilter_OpenCloseAdvanced::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
    return,  'FASL_NC_Release_Filter_Method'
end

function NC_C_sImageFilter_OpenCloseAdvanced::apply, image = image
    
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Pixel_Neighbor'))[0]
    (*(*self.pParamStruct).pValues)[whParam] = (*(*self.pParamStruct).pValues)[whParam] > 1
    pixelNeighbor = (*(*self.pParamStruct).pValues)[whParam]
    erode=0
    dilate=0
    open = 0
    close = 0
    grey = 0
    structure = 0
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Erode'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then erode = 1

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Dilate'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then dilate = 1
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Open'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then open = 1

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Close'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then close = 1
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Disk->0_Square->1_vLine->2_hLine->3_diagLine->4'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then structure = (*(*self.pParamStruct).pValues)[whParam]
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'GreyScale'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then grey = 1
    
    
    dims = SIZE(image, /DIMENSION)  
    radius = pixelNeighbor  
    structure = round(structure)
    case structure of
      0: strucElem = SHIFT(DIST(2*radius+1), radius, radius) LE radius  ; disk
      1: strucElem = DIST(radius) LE radius  ; square
      2: strucElem = BYTARR(radius, radius) ; vertical line
      3: strucElem = BYTARR(radius, radius) ; horizontal line
      4: strucElem = BYTE(IDENTITY(radius)) ; det line
   endcase
    if (structure eq 2) then strucElem[0,*] = 1  
    if (structure eq 3) then strucElem[*,0] = 1  
    
    print, strucElem
    morph = image
    
    
    if(erode) then begin
         if(grey eq 0) then begin
            erodeImg = REPLICATE(1B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)  
         endif else begin
            erodeImg = REPLICATE(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)       
        endelse
            erodeImg[pixelNeighbor, pixelNeighbor] = image  
            padDims = SIZE(erodeImg, /DIMENSIONS)  
            morph = ERODE(erodeImg, strucElem, /GRAY )  
    endif
   
    if(dilate) then begin
         if(grey eq 0) then begin
            dilateImg = REPLICATE(0B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)   
         endif else begin
            dilateImg = REPLICATE(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)  
         endelse
            dilateImg[pixelNeighbor,pixelNeighbor] = image  
            padDims = SIZE(dilateImg, /DIMENSIONS)  
            morph = DILATE(dilateImg, strucElem, /GRAY)    
    endif
    
     if(open) then begin
         if(grey eq 0) then begin
            openImg = REPLICATE(1B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)  
         endif else begin
            openImg = REPLICATE(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)        
         endelse
            openImg[pixelNeighbor,pixelNeighbor] = image  
            padDims = SIZE(openImg, /DIMENSIONS)  
            morph = MORPH_OPEN(openImg, strucElem, /GRAY)  
    endif
    
    if(close) then begin
         if(grey eq 0) then begin
            closeImg = REPLICATE(1B, dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)   
         endif else begin
            dilateImg = REPLICATE(max(image), dims[0]+2*pixelNeighbor, dims[1]+2*pixelNeighbor)   
         endelse
            closeImg[pixelNeighbor, pixelNeighbor] = image  
            padDims = SIZE(closeImg, /DIMENSIONS)  
            morph = MORPH_CLOSE(closeImg, strucElem, /GRAY) 
    endif
  
    
    morph = morph([pixelNeighbor]:[dims[0]-1+pixelNeighbor],[pixelNeighbor]:[dims[1]-1+pixelNeighbor])
   
    return, morph
end

pro NC_C_sImageFilter_OpenCloseAdvanced::set, pParamStruct = pParamStruct
   self.pParamStruct = ptr_new(*pParamStruct, /no_copy)
end
pro NC_C_sImageFilter_OpenCloseAdvanced::get, pParamStruct = pParamStruct
   pParamStruct = self.pParamStruct
end
function NC_C_sImageFilter_OpenCloseAdvanced::getpParamStruct
   return, self.pParamStruct
end

pro NC_C_sImageFilter_OpenCloseAdvanced::cleanup
   for i = 0,n_tags((*self.pParamStruct))-1 do begin
      case size((*self.pParamStruct).(i), /tname) of
      'POINTER':ptr_free, (*self.pParamStruct).(i)
      'OBJREF':obj_destroy, (*self.pParamStruct).(i)
      else:
      endcase
   endfor
   ptr_free, self.pParamStruct
end


function NC_C_sImageFilter_OpenCloseAdvanced::init
   filterStruct = {Name:'C_OpenCloseAdvanced',$   ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

      ; Parameters of OpenClose.
   filterParamWidgetType = make_array(7, /string, value = 'widget_slider')

   filterParamNames = ['Erode',$
                      'Dilate',$
                      'Open',$
                      'Close',$
                      'Pixel_Neighbor', $
                      'Disk->0_Square->1_vLine->2_hLine->3_diagLine->4', $
                      'GreyScale']

   filterParamActive = [1,0,0,0,1,1,0]
   filterParamMin = [0,0,0,0,1,0,0]
   filterParamMax = [1,1,1,1,100,4,1]
   filterParamValues = [1.,0,0,0,1,0,0]

   filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro NC_C_sImageFilter_OpenCloseAdvanced__define
   tmp = {NC_C_sImageFilter_OpenCloseAdvanced, pParamStruct:ptr_new(), inherits C_sImageFilter}
end