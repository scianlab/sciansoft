;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_1stDeviation
;
; PURPOSE:
;       - 1stDeviation-Filter-Class. See 1stDeviation.pro
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;.
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_1stDeviation' )
;
; METHOHDS:
;   function ->apply, pImageData = pImageData                     ;pImageData          Pointer on Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_1stDeviation::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_1stDeviation::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    dimI = size (image, /dim)
    wherePixRad = (where((*(*self.pParamStruct).pNames) eq 'Pixel_Radius'))[0]
    pixRad = round((*(*self.pParamStruct).pValues)[wherePixRad])

    expandImage = 1.*s_Expand_Mirror(image, pixRad)
    m_alpha = fltArr(8, dimI[0], dimI[1])
    alpha = fltArr(dimI[0], dimI[1])

    whereSubPixRad = (where((*(*self.pParamStruct).pNames) eq 'Sub_Pixel_Radius'))[0]
    if ( (*(*self.pParamStruct).pActive)[whereSubPixRad] ne 1) then begin
       k = pixRad
       m_alpha[0,*,*] = image - expandImage[pixRad+k : pixRad+k+dimI[0]-1, pixRad : pixRad+dimI[1]-1 ]
       m_alpha[1,*,*] = image - expandImage[pixRad+k : pixRad+k+dimI[0]-1, pixRad+k : pixRad+k+dimI[1]-1 ]
       m_alpha[2,*,*] = image - expandImage[pixRad : pixRad+dimI[0]-1    , pixRad+k : pixRad+k+dimI[1]-1 ]
       m_alpha[3,*,*] = image - expandImage[pixRad-k : pixRad-k+dimI[0]-1, pixRad+k : pixRad+k+dimI[1]-1 ]
       m_alpha[4,*,*] = image - expandImage[pixRad-k : pixRad-k+dimI[0]-1, pixRad : pixRad+dimI[1]-1 ]
       m_alpha[5,*,*] = image - expandImage[pixRad-k : pixRad-k+dimI[0]-1, pixRad-k : pixRad-k+dimI[1]-1 ]
       m_alpha[6,*,*] = image - expandImage[pixRad : pixRad+dimI[0]-1    , pixRad-k : pixRad-k+dimI[1]-1 ]
       m_alpha[7,*,*] = image - expandImage[pixRad+k : pixRad+k+dimI[0]-1, pixRad-k : pixRad-k+dimI[1]-1 ]
    endif else begin
       subPixRad = (*(*self.pParamStruct).pValues)[whereSubPixRad]
         for k = 0, pixRad do begin
          if (k lt subPixRad) then a = -1. else a = 1.
              m_alpha[0,*,*] += a*(image - expandImage[pixRad+k : pixRad+k+dimI[0]-1, pixRad : pixRad+dimI[1]-1 ])
              m_alpha[1,*,*] += a*(image - expandImage[pixRad+k : pixRad+k+dimI[0]-1, pixRad+k : pixRad+k+dimI[1]-1 ])
              m_alpha[2,*,*] += a*(image - expandImage[pixRad : pixRad+dimI[0]-1    , pixRad+k : pixRad+k+dimI[1]-1 ] )
              m_alpha[3,*,*] += a*(image - expandImage[pixRad-k : pixRad-k+dimI[0]-1, pixRad+k : pixRad+k+dimI[1]-1 ])
              m_alpha[4,*,*] += a*(image - expandImage[pixRad-k : pixRad-k+dimI[0]-1, pixRad : pixRad+dimI[1]-1 ] )
              m_alpha[5,*,*] += a*(image - expandImage[pixRad-k : pixRad-k+dimI[0]-1, pixRad-k : pixRad-k+dimI[1]-1 ])
              m_alpha[6,*,*] += a*(image - expandImage[pixRad : pixRad+dimI[0]-1    , pixRad-k : pixRad-k+dimI[1]-1 ] )
              m_alpha[7,*,*] += a*(image - expandImage[pixRad+k : pixRad+k+dimI[0]-1, pixRad-k : pixRad-k+dimI[1]-1 ])
         endfor
    endelse

    whereMinMax = (where((*(*self.pParamStruct).pNames) eq 'Min_Values'))[0]
    if (*(*self.pParamStruct).pActive)[whereMinMax] then $
       alpha[*,*] = m_alpha[0,*,*] < m_alpha[1,*,*] < m_alpha[2,*,*] < m_alpha[3,*,*] < m_alpha[4,*,*] < m_alpha[5,*,*] < m_alpha[6,*,*] < m_alpha[7,*,*]
    whereMinMax = (where((*(*self.pParamStruct).pNames) eq 'Max_Values'))[0]
    if (*(*self.pParamStruct).pActive)[whereMinMax] then $
       alpha[*,*] = m_alpha[0,*,*] > m_alpha[1,*,*] > m_alpha[2,*,*] > m_alpha[3,*,*] > m_alpha[4,*,*] > m_alpha[5,*,*] > m_alpha[6,*,*] > m_alpha[7,*,*]
    whereMinMax = (where((*(*self.pParamStruct).pNames) eq 'Mean_Values'))[0]
    if (*(*self.pParamStruct).pActive)[whereMinMax] then $
       alpha[*,*] = (m_alpha[0,*,*] + m_alpha[1,*,*] + m_alpha[2,*,*] + m_alpha[3,*,*] + m_alpha[4,*,*] + m_alpha[5,*,*] + m_alpha[6,*,*] + m_alpha[7,*,*]) / 8.

       ; free memory
    m_alpha = 0.

    whereMinMax = (where((*(*self.pParamStruct).pNames) eq 'Image_*_Balance'))[0]
    if ((*(*self.pParamStruct).pActive)[whereMinMax] eq 1) then begin
       balanceFactor = (*(*self.pParamStruct).pValues)[whereMinMax]
       minA = min(alpha, max = maxA)
       minI = min(image, max = maxI)
       alpha = ((1./balanceFactor)>.001) * (1.*alpha - minA) / ((maxA - minA) > (1/256.))
       image = (1./((1.-balanceFactor)>.001)) * (1.*image - minI) / ((maxI - minI) > (1/256.))
       alpha *= image
    endif

    whereMinMax = (where((*(*self.pParamStruct).pNames) eq 'Image_+_Balance'))[0]
    if ((*(*self.pParamStruct).pActive)[whereMinMax] eq 1) then begin
       balanceFactor = (*(*self.pParamStruct).pValues)[whereMinMax]
       minA = min(alpha, max = maxA)
       minI = min(image, max = maxI)
       alpha = (1.*balanceFactor) * (1.*alpha - minA) / ((maxA - minA) > (1/256.))
       image = (1.-balanceFactor) * (1.*image - minI) / ((maxI - minI) > (1/256.))
       alpha += image
    endif
    return, alpha
end


function C_sImageFilter_1stDeviation::init

    filterStruct = {Name: 'C_1stDeviation',$     ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of 1stDeviation.
    filterParamWidgetType = make_array(7, /string, value = 'widget_slider')

    filterParamNames = ['Pixel_Radius',$
                        'Sub_Pixel_Radius',$
                        'Mean_Values',$
                        'Min_Values',$
                        'Max_Values',$
                        'Image_*_Balance',$
                        'Image_+_Balance']

    filterParamActive = [1,0,1,0,0,0,0]
    filterParamMin = [1.,0,0,0,0,0,0]
    filterParamMax = [100.,99.,1,1,1,1,1]
    filterParamValues = [1.,1.,0,0,0,0,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_1stDeviation__define
  tmp = {C_sImageFilter_1stDeviation, pParamStruct: ptr_new(), inherits C_sImageFilter}
end