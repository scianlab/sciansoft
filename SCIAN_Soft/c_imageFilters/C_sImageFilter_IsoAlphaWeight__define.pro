;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_IsoAlphaWeight  (Steffen\s_Surface\sImageFilters_apop_iso_weight.pro)
;
; PURPOSE:
;       - IsoAlphaWeight-Filter-Class.
;
; AUTHOR:
;     loyarzo (2003)
;     e_mail: loyarzo@gmx.net
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_IsoAlphaWeight' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_IsoAlphaWeight::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_IsoAlphaWeight::apply, image = image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'PixRad1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         PixRad1 = round((*(*self.pParamStruct).pValues)[whParam]) > 0 else PixRad1 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'PixRad2'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         PixRad2 = round((*(*self.pParamStruct).pValues)[whParam]) > (PixRad1+1) else PixRad2 = 1 > (PixRad1+1)
    whParam = (where((*(*self.pParamStruct).pNames) eq 'IntensityRad1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         IntensityRad1 = round((*(*self.pParamStruct).pValues)[whParam]) > 0 else IntensityRad1 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'IntensityRad2'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then $
         IntensityRad2 = round((*(*self.pParamStruct).pValues)[whParam]) > IntensityRad1 else IntensityRad2 = 0 > IntensityRad1

    whParam = (where((*(*self.pParamStruct).pNames) eq 'weightForm1'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then weightForm1 = 1 else weightForm1 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'weightForm2'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then weightForm2 = 1 else weightForm2 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'weightForm3'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then weightForm3 = 1 else weightForm3 = 0
    whParam = (where((*(*self.pParamStruct).pNames) eq 'weightForm4'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then weightForm4 = 1 else weightForm4 = 0

    dimOri = size(image, /dim)
    normOri = 1.0 * image
    distance = fltArr(dimOri[0],dimOri[1])     ; Distance Matrix
    alphan1 = distance + 1   ; Matrix for rad1
    alphan2 = alphan1     ; Matrix for rad2

    expImage = s_Expand_Mirror(image, PixRad2)
    r1max = sqrt(PixRad1^2+IntensityRad1^2)
    r2max = sqrt(PixRad2^2+IntensityRad2^2)
    for k=-PixRad2,PixRad2 do for l=-PixRad2,PixRad2 do begin
           pixelAbst = 1.0 * (k*k+l*l)
           if ( pixelAbst le (PixRad2)^2 ) and ( pixelAbst ne 0) then begin
            distance = sqrt( ((expImage( PixRad2+k : PixRad2+k+dimOri[0]-1,  PixRad2+l : PixRad2+l+dimOri[1]-1 ) - normOri )^2) + pixelAbst)
;             intensityAbst = (expImage( PixRad2+k : PixRad2+k+dimOri[0]-1,  PixRad2+l : PixRad2+l+dimOri[1]-1 ) - normOri )^2
;          distance = sqrt(intensityAbst + pixelAbst)
          if (weightForm1 eq 1) then begin
              alphan1 = temporary(alphan1) + (distance le r1max ) * ( ((r1max>1)) / (distance>1) )
              alphan2 = temporary(alphan2) + (distance le r2max ) *( (distance gt r1max ) * ( r2max/(distance>1) ) +  (distance le r1max ) * ( ((r1max>1))/(distance>1) ) )
          endif
          if (weightForm2 eq 1) then begin
              alphan1 = temporary(alphan1) + (distance le r1max ) * ( ((r1max>1)) / (distance>1) )
              alphan2 = temporary(alphan2) + (distance le r2max ) *( (distance gt r1max ) * ( (r2max-r1max)/((distance-r1max)>1) ) +  (distance le r1max ) * ( ((r1max>1))/(distance>1) ) )
          endif
          if (weightForm3 eq 1) then begin
              alphan1 = temporary(alphan1) + (distance le r1max ) * ( 1. - distance/((r1max>1)) )
              alphan2 = temporary(alphan2) + (distance le r2max ) *( (distance gt r1max ) * ( 1 - distance/r2max )  +  (distance le r1max ) * ( 1 - distance/((r1max>1)) ) )
          endif
          if (weightForm4 eq 1) then begin
              alphan1 = temporary(alphan1) + (distance le r1max ) * ( 1. - distance/((r1max>1)) )
              alphan2 = temporary(alphan2) + (distance le r2max ) *( (distance gt r1max ) * ( 1 - (distance-r1max)/(r2max-r1max) )  +  (distance le r1max ) * ( 1 - distance/((r1max>1)) ) )
          endif
          if ((weightForm1 + weightForm2 + weightForm3 + weightForm4) eq 0) then begin
              alphan1 = temporary(alphan1) + (distance le r1Max )
              alphan2 = temporary(alphan2) + (distance le r2Max )
          endif
         endif
    endfor
    return,  alog(alphan2/alphan1) / alog((r2max>2)/((r1max>1)))
end


function C_sImageFilter_IsoAlphaWeight::init
    filterStruct = {Name: 'C_IsoAlphaWeight',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$      ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

       ; Parameters of C_IsoAlphaWeight.
    filterParamWidgetType = ['widget_slider',$
                             'widget_slider',$
                             'widget_slider',$
                             'widget_slider',$
                             'widget_slider',$
                             'widget_slider',$
                             'widget_slider',$
                             'widget_slider']
    filterParamNames = ['PixRad1' ,$       ;  PixRad1 for Pixel in Pixel rad1
                 'PixRad2' ,$         ;  PixRad2 for Pixel in Pixel rad2
                 'IntensityRad1' ,$     ;    IntensityRad1 for Colore in Units rad1
                 'IntensityRad2' ,$     ;    IntensityRad2 for Colore in Units rad2
                 'weightForm1' ,$     ;      IntensityRad for Colore in Units rad1
                 'weightForm2' ,$     ;      IntensityRad for Colore in Units rad1
                 'weightForm3' ,$     ;      IntensityRad for Colore in Units rad1
                 'weightForm4']       ;     IntensityRad for Colore in Units rad1
    filterParamActive = [1,$       ;   (always active)
                  1,$        ;   (always active)
                  0,$        ;   (always inactive)
                  0,$        ;   (always inactive)
                  1,$        ;   (always inactive)
                  0,$        ;   (always inactive)
                  0,$        ;   (always inactive)
                  0]          ;   (always inactive)
    filterParamMin = [0 ,$
                     1.,$
                   0,$
                   1.,$
                   0,$
                   0,$
                   0,$
                   0]
    filterParamMax = [19.,20.,49.,50., 1, 1, 1, 1]
    filterParamValues = [1. ,2. ,0 ,0 , 0, 0, 0, 0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_IsoAlphaWeight__define
  tmp = {C_sImageFilter_IsoAlphaWeight, pParamStruct: ptr_new(), inherits C_sImageFilter}
end