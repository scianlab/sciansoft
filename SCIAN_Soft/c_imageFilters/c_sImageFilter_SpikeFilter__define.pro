;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_SpikeFilter
;
; PURPOSE:
;       - 1stDeviation-Filter-Class. See 1stDeviation.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_SpikeFilter' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_SpikeFilter::getImageFilterType
   return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_SpikeFilter::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image

   dimI = size (image, /dim)
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Pixel_Radius'))[0]
   pixRad = round((*(*self.pParamStruct).pValues)[whParam])

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Spike_Method'))[0]
   spikeMet = round((*(*self.pParamStruct).pValues)[whParam])

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Spike_Number'))[0]
   spikeNum = round((*(*self.pParamStruct).pValues)[whParam])

   case spikeMet of
      -1:
      else: begin
         kernel = make_array(2*pixRad+1, 2*pixRad+1, /float)
         for j = 0, 2*pixRad do for i = 0, 2*pixRad do kernel[i,j] = sqrt( (i-pixRad)^2 + (j-pixRad)^2 )

         kernel = -1. * (kernel lt (pixRad+1)) * (kernel ge pixRad)
         kernel[pixRad, pixRad] = -1. * total(kernel)
         print, 'kernel', kernel

         spikeImage = convol(1.*image, kernel, center = 1, /edge_wrap)
         
         whOne = where(kernel eq -1.)
         kernel[*] = 0.
         kernel[whOne] = 1.
         kernel /= total(kernel)
         print, 'meanKernel', kernel

         meanImage = convol(1.*image, kernel, center = 1, /edge_wrap)         
         
      endcase
   endcase
   
   if (spikeNum gt 0) then begin
      sortInd = reverse(sort(spikeImage))
      for i = 0, spikeNum-1 do begin
         image[sortInd[i]] = meanImage[sortInd[i]]
      endfor 
   endif 

   if (spikeMet eq 0) then begin
      image[*] = 0.
      for i = 1, spikeNum do begin
         image[sortInd[i]] = 255
      endfor 
   endif

   if (spikeMet eq 2) then begin
      return, spikeImage
   endif

   return, image
end


function C_sImageFilter_SpikeFilter::init

    filterStruct = {Name: 'C_SpikeFilter',$     ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of 1stDeviation.
    filterParamWidgetType = make_array(3, /string, value = 'widget_slider')

    filterParamNames = ['Pixel_Radius', 'Spike_Method', 'Spike_Number']

    filterParamActive = [1,1,1]
    filterParamMin = [1.,0,0]
    filterParamMax = [100,2,1000]
    filterParamValues = [1.,1,5]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_SpikeFilter__define
  tmp = {C_sImageFilter_SpikeFilter, pParamStruct: ptr_new(), inherits C_sImageFilter}
end