;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Convol
;
; PURPOSE:
;       - Convol-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_Convol' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_Convol::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_Convol::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size(image, /dim)
    typeI = size(image, /type)

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Kernel_Dim'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then $
       kernel_dim = (*(*self.pParamStruct).pValues)[whParam] < (floor(min(dimI)/2.) > 3)

    if ((min(dimI) le (kernel_dim*2+1)) or (kernel_dim eq 0)) then return, image

    kernelArray = indgen(kernel_dim*2+1, type = typeI)
    kernelArray = (1-cos(!pi*kernelArray/kernel_dim))
    kernelMatrix = make_array(kernel_dim*2+1, kernel_dim*2+1, type = size(kernelArray, /type))

    dimKernel = size(kernelMatrix, /dim)
    for i = 1, kernel_dim-1 do begin
       kernelMatrix[i, i : dimKernel[1]-1-i] = kernelArray[i]
       kernelMatrix[dimKernel[0]-1-i, i : dimKernel[1]-1-i] = kernelArray[i]
       kernelMatrix[i : dimKernel[0]-1-i, i] = kernelArray[i]
       kernelMatrix[i : dimKernel[0]-1-i, dimKernel[1]-1-i] = kernelArray[i]
    endfor
    kernelMatrix[kernel_dim,kernel_dim] = kernelArray[kernel_dim]
    whParam = (where((*(*self.pParamStruct).pNames) eq 'y-Factor'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then $
       kernelMatrix = congrid(kernelMatrix, dimKernel[0], dimKernel[1]*(*(*self.pParamStruct).pValues)[whParam])
    tvscl, kernelMatrix

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Center'))[0]
    if ((*(*self.pParamStruct).pActive)[whParam]) then begin
       whParam = (where((*(*self.pParamStruct).pNames) eq 'edge_wrap'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then return, convol(image, kernelMatrix, total(kernelMatrix), center = 1, /edge_wrap)
       whParam = (where((*(*self.pParamStruct).pNames) eq 'edge_truncate'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then return, convol(image, kernelMatrix, total(kernelMatrix), center = 1, /edge_truncate)
       return, convol(image, kernelMatrix, total(kernelMatrix), center = 1)
    endif else begin
       whParam = (where((*(*self.pParamStruct).pNames) eq 'edge_wrap'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then return, convol(image, kernelMatrix, total(kernelMatrix), center = 0, /edge_wrap)
       whParam = (where((*(*self.pParamStruct).pNames) eq 'edge_truncate'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then return, convol(image, kernelMatrix, total(kernelMatrix), center = 0, /edge_truncate)
       return, convol(image, kernelMatrix, total(kernelMatrix), center = 0)
    endelse
end


function C_sImageFilter_Convol::init

    filterStruct = {Name:'C_Convol',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pImagefilterParamType: ptr_new(),$      ; Pointer on Filter Parameter Type (Bool, Byte, Int, Long, Float, Double).
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$       ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$   ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of sMeanImage.
    filterParamWidgetType = make_array(5, /string, value = 'widget_slider')

    filterParamNames = ['Kernel_Dim',$       ;     Kernel-Dimension
                    'Center',$        ;     New Y-Dimension
                    'edge_wrap',$   ;   Forces  nearest-neighbor sampling (DEFAULT, 0 OFF/1 ON).
                    'edge_truncate',$   ;   Forces  nearest-neighbor sampling (DEFAULT, 0 OFF/1 ON).
                    'y-Factor']

    filterParamType = ['Int',$
                        'Bool',$
                        'Bool',$
                        'Bool',$
                        'Int' ]

    filterParamActive = [   1,$
                              1,$
                              0,$
                              0,$
                              0]

    filterParamMin = [         0.,$
                              0.,$
                              0.,$
                              0.,$
                              0]

    filterParamMax = [        1000,$
                              1,$
                              1,$
                              1,$
                              100]

    filterParamValues = [  1,$
                              1,$
                              0,$
                              0,$
                              1]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pImagefilterParamType = ptr_new(filterParamType, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_Convol__define
  tmp = {C_sImageFilter_Convol, pParamStruct: ptr_new(), inherits C_sImageFilter}
end