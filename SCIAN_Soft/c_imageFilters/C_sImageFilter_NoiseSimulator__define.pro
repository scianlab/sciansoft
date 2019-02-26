;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_NoiseSimulator  (Steffen\s_Surface\sImageFilters_apop_iso.pro)
;
; PURPOSE:
;       - NoiseSimulator-Filter-Class.
;
; AUTHOR:
;     Dr. Steffen Härtel (2005)
;     e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_NoiseSimulator' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_NoiseSimulator::getImageFilterType
    return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_NoiseSimulator::apply, image = image

    case size(image, /type) of
        4: xVect = make_array(4096, /float, /index)
        else: xVect = make_array(256, /float, /index)
    endCase

       ; define noiseImage
    n_image = n_elements(image)
    noiseImage = make_array(n_image, /float, value = -1.)
    whereVect = make_array(n_image, /int, /index)

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Add Noise to Signal'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then fAddNoiseToSignal = 1b else fAddNoiseToSignal = 0b

    whereShiftMeanValueTo = (where((*(*self.pParamStruct).pNames) eq 'Shift Mean-Value to ...'))[0]
       if ((*(*self.pParamStruct).pActive)[whereShiftMeanValueTo]) then fShiftMeanValueTo = 1b else fShiftMeanValueTo = 0b

    whParam = (where((*(*self.pParamStruct).pNames) eq 'Freeze Random Prozess ...'))[0]
       if ((*(*self.pParamStruct).pActive)[whParam]) then fFreezeRandom = 1b else fFreezeRandom = 0b

    whereGauss = (where((*(*self.pParamStruct).pNames) eq 'Gauss-Noise'))[0]
    wherePoisson = (where((*(*self.pParamStruct).pNames) eq 'Poisson-Noise'))[0]
    whereRayleigh = (where((*(*self.pParamStruct).pNames) eq 'Rayleigh-Noise'))[0]
    whereGamma = (where((*(*self.pParamStruct).pNames) eq 'Gamma-Noise'))[0]
    whereExponential = (where((*(*self.pParamStruct).pNames) eq 'Exponential-Noise'))[0]
    whereUniform = (where((*(*self.pParamStruct).pNames) eq 'Uniform-Noise'))[0]

    whParamMeanNoise = (where((*(*self.pParamStruct).pNames) eq 'Mean Noise'))[0]
    whParamSigmaNoise = (where((*(*self.pParamStruct).pNames) eq 'Variance of Noise'))[0]
    whParamA = (where((*(*self.pParamStruct).pNames) eq 'Parameter a'))[0]
    whParamB = (where((*(*self.pParamStruct).pNames) eq 'Parameter b'))[0]

       ; Gauss-Noise
    case 1 of
       ((*(*self.pParamStruct).pActive)[whereGauss]):  begin
         case 1 of
          ((*(*self.pParamStruct).pActive)[whParamMeanNoise]): begin
              paramA = (*(*self.pParamStruct).pValues)[whParamMeanNoise]
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
          endCase
          ((*(*self.pParamStruct).pActive)[whParamA]): begin
              paramA = (*(*self.pParamStruct).pValues)[whParamA]
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
          else: begin
              paramA = 2.
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 1b
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
          endCase
         endCase

         case 1 of
          ((*(*self.pParamStruct).pActive)[whParamSigmaNoise]): begin
              paramB = (*(*self.pParamStruct).pValues)[whParamSigmaNoise]
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
          endCase
          ((*(*self.pParamStruct).pActive)[whParamB]): begin
              paramB = (*(*self.pParamStruct).pValues)[whParamB]
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramB
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
          endCase
          else: begin
              paramB = 1.
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramB
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 1b
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
          endCase
         endCase

          ; calculate Probability Density Function (PDF)
         PDF = s_Gauss(xVect = xVect, a = paramA, b = paramB)
       endCase

       ; Poisson-Noise
       ((*(*self.pParamStruct).pActive)[wherePoisson]): begin
         case 1 of
         ((*(*self.pParamStruct).pActive)[whParamMeanNoise]): begin
              paramA = (*(*self.pParamStruct).pValues)[whParamMeanNoise]
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pValues)[whParamB] = paramA
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamSigmaNoise]): begin
              paramA = (*(*self.pParamStruct).pValues)[whParamSigmaNoise]
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pValues)[whParamB] = paramA
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamA]): begin
              paramA = (*(*self.pParamStruct).pValues)[whParamA]
              (*(*self.pParamStruct).pValues)[whParamB] = paramA
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramA
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamB]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamB]
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
          endCase
         else: begin
              paramA = 2.
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramA
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pValues)[whParamB] = paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 1b
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
          endCase
         endCase

          ; calculate Probability Density Function (PDF)
         PDF = s_Poisson(xVect = xVect, a = paramA)
       endCase

         ; Rayleigh-Noise
       ((*(*self.pParamStruct).pActive)[whereRayleigh]): begin
         case 1 of
         ((*(*self.pParamStruct).pActive)[whParamB]):  begin
              paramB = (*(*self.pParamStruct).pValues)[whParamB]
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramB * (4. - !pi) / 4.
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamSigmaNoise]):  begin
              paramB = (*(*self.pParamStruct).pValues)[whParamSigmaNoise] * 4. / (4. - !pi)
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
          endCase
         else: begin
              paramB = 5.
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamB] = 1b
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramB * (4. - !pi) / 4.
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
          endCase
         endCase

         case 1 of
         ((*(*self.pParamStruct).pActive)[whParamA]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamA]
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA + sqrt(paramB * !pi / 4.)
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamMeanNoise]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamMeanNoise]  - sqrt((*(*self.pParamStruct).pValues)[whParamSigmaNoise] * !pi / (4 - !pi) )
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
          endCase
         else: begin
              paramA = 5.
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 1b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramA + sqrt(paramB * !pi / 4.)
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         endCase

          ; calculate Probability Density Function (PDF)
         PDF = s_Rayleigh(xVect = xVect, a = paramA, b = paramB)
       endCase

       ; Gamma-Erlang-Noise
       ((*(*self.pParamStruct).pActive)[whereGamma]): begin
         case 1 of
         ((*(*self.pParamStruct).pActive)[whParamB]) and ((*(*self.pParamStruct).pActive)[whParamA]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamA]
              paramB = (*(*self.pParamStruct).pValues)[whParamB]
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramB / paramA^2
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramB / paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamSigmaNoise]) and ((*(*self.pParamStruct).pActive)[whParamMeanNoise]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamMeanNoise] / (*(*self.pParamStruct).pValues)[whParamSigmaNoise]
              paramB = (*(*self.pParamStruct).pValues)[whParamMeanNoise]^2 / (*(*self.pParamStruct).pValues)[whParamSigmaNoise]
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
          endCase
         else: begin
              paramA = 5.
              paramB = 5.
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 1b
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamB] = 1b
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = paramB / paramA^2
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = paramB / paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         endCase

          ; calculate Probability Density Function (PDF)
         PDF = s_GammaErlang(xVect = xVect, a = paramA, b = paramB)
       endCase

         ; Exponential-Noise
       ((*(*self.pParamStruct).pActive)[whereExponential]): begin
         case 1 of
         ((*(*self.pParamStruct).pActive)[whParamA]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamA]
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = 1. / paramA^2
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = 1.  / paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamMeanNoise]):  begin
              paramA = 1. / (*(*self.pParamStruct).pValues)[whParamMeanNoise]
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = 1. / paramA^2
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamSigmaNoise]):  begin
              paramA = sqrt(1. / (*(*self.pParamStruct).pValues)[whParamSigmaNoise])
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = 1. / paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         else: begin
              paramA = 5.
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 1b
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = 1. / paramA^2
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = 1. / paramA
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         endCase

          ; calculate Probability Density Function (PDF)
         PDF = s_Exponential(xVect = xVect, a = paramA)
       endCase

         ; Uniform-Noise
       else:  begin
         ((*(*self.pParamStruct).pActive)[whereUniform]) = 1b
         case 1 of
         ((*(*self.pParamStruct).pActive)[whParamB]) and ((*(*self.pParamStruct).pActive)[whParamA]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamA] < (*(*self.pParamStruct).pValues)[whParamB]
              paramB = (*(*self.pParamStruct).pValues)[whParamB] > (*(*self.pParamStruct).pValues)[whParamB]
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = (paramB - paramA)^2 / 12.
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = (paramB + paramA) / 2.
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         ((*(*self.pParamStruct).pActive)[whParamSigmaNoise]) and ((*(*self.pParamStruct).pActive)[whParamMeanNoise]):  begin
              paramA = (*(*self.pParamStruct).pValues)[whParamMeanNoise] - sqrt(3. * (*(*self.pParamStruct).pValues)[whParamSigmaNoise])
              paramB = (*(*self.pParamStruct).pValues)[whParamMeanNoise] + sqrt(3. * (*(*self.pParamStruct).pValues)[whParamSigmaNoise])
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 0b
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamB] = 0b
          endCase
         else: begin
              paramA = 5.
              paramB = 10.
              (*(*self.pParamStruct).pValues)[whParamA] = paramA
              (*(*self.pParamStruct).pActive)[whParamA] = 1b
              (*(*self.pParamStruct).pValues)[whParamB] = paramB
              (*(*self.pParamStruct).pActive)[whParamB] = 1b
              (*(*self.pParamStruct).pValues)[whParamSigmaNoise] = (paramB - paramA)^2 / 12.
              (*(*self.pParamStruct).pActive)[whParamSigmaNoise] = 0b
              (*(*self.pParamStruct).pValues)[whParamMeanNoise] = (paramB + paramA) / 2.
              (*(*self.pParamStruct).pActive)[whParamMeanNoise] = 0b
          endCase
         endCase

          ; calculate Probability Density Function (PDF)
         PDF = s_Uniform(xVect = xVect, a = paramA, b = paramB)
       endCase
    endCase

       ; remove NAN-values from PDF
    wherePDF = where(finite(PDF))
    if (wherePDF[0] ne -1) then PDF = PDF[wherePDF]

       ; calculate Probability Density Function (PDF) for imageMatrix
    PDFn = PDF * n_image
    whereGEOne = where(PDFn ge 1.)

    if (fFreezeRandom) then seedVar = 1l else seedVar = long( sysTime(1) + memory(/current) )
    if (whereGEOne[0] ne -1) then begin
       repeat begin
         randomCount = 0l
         for i = 0, n_elements(whereGEOne)-1 do begin
          j = 0
          repeat begin
              randomPos = round(n_elements(whereVect)*randomU(seedVar, floor(PDFn[whereGEOne[i]])-randomCount, /uniform))
              noiseImage[whereVect[randomPos]] = whereGEOne[i]
              whereVect2 = where(noiseImage eq -1)
              randomCount = randomCount + n_elements(whereVect) - n_elements(whereVect2)
              whereVect = whereVect2
              j = j + 1
          endRep until (floor(PDFn[whereGEOne[i]]) eq randomCount) or (j eq 10) or (whereVect2[0] eq -1)
          randomCount = 0l
         endfor
       endRep until (whereVect2[0] eq -1)
    endif

    if (fShiftMeanValueTo) then noiseImage = temporary(noiseImage) - ((*(*self.pParamStruct).pValues)[whParamMeanNoise] - (*(*self.pParamStruct).pValues)[whereShiftMeanValueTo])
    if (fAddNoiseToSignal) then return, image + noiseImage else  return, image*0 + noiseImage
end


function C_sImageFilter_NoiseSimulator::init
    ImageFilterStruct = {Name: 'C_NoiseSimulator',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$     ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$       ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}        ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(13, /string, value = 'widget_slider')
    filterParamNames = ['Add Noise to Signal',$
                           'Gauss-Noise' ,$
                           'Poisson-Noise' ,$
                           'Rayleigh-Noise' ,$
                           'Gamma-Noise' ,$
                           'Exponential-Noise' ,$
                           'Uniform-Noise' ,$
                           'Mean Noise',$
                           'Variance of Noise',$
                           'Parameter a' ,$
                           'Parameter b' ,$
                           'Shift Mean-Value to ...' ,$
                           'Freeze Random Prozess ...' ]
    filterParamActive = [1,1,0,0,0,0,0,1,1,0,0,0,0]
    filterParamMin = [0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,-255.,0]
    filterParamMax = [1.,1.,1.,1.,1.,1.,1.,255.,255.,255.,255.,255.,1]
    filterParamValues = [1.,1.,0.,0.,0.,0.,0.,10.,5.,10.,5.,0.,0.]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_NoiseSimulator__define
  tmp = {C_sImageFilter_NoiseSimulator, pParamStruct: ptr_new(), inherits C_sImageFilter}
end