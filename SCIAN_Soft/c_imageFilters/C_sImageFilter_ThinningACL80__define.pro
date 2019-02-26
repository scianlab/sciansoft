;_____________________________IOISIOI____________________ (This is art, a friend of mine use to say ;)
; NAME:
;     C_sImageFilter_ThinningACL80
;
; PURPOSE:
;     - Thinning filter for binary images. THIS IS A DEVELOPMENT VERSION
;
; AUTHOR:
;     Jorge Jara (2008)
;     e_mail: jjaraw@gmail.com
;
; CALLING SEQUENCE:
;     result = obj_new('C_sImageFilter_ThinningACL80')
;
; METHODS:
;     getImageFilterType   A "KIND OF" GETTER FOR THE IMAGE FILTER TYPE
;     checkParamsApplied   NOT VERY WELL USED... YET
;     apply                APPLIES THE FILTER
;     init                 CLASS CONSTRUCTOR
;
; REFERENCES:
;     Arcelli C, Cordella LP, Levialdi S.
;     "More About a Thinning Algorithm"
;     Electronics Letters 16(2): 51 - 53
;     1980
;_____________________________IOISIOI____________________ (This is art, a friend of mine use to say ;)

function C_sImageFilter_ThinningACL80::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_ThinningACL80::checkParamsApplied
    ; Compares parameter values between the current values and the values applied in the last segmentation.
    if ptr_valid(self.pParamApplied) then begin
        if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
            *self.pParamApplied = *(*self.pParamStruct).pValues
            return, 1
        endif
    endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
    return, 0
end


function C_sImageFilter_ThinningACL80::apply, image = image

    if (size(image, /n_dim) ne 2) then return, image
;    exp_image = s_Expand_Mirror(image, 1)

    ; If the parameters/input image unchanged since the last segmentation appliance of this filter,
    ; there's no need to spend time computing the contours again,
    ; and the filter returns the previosly segmented image
    if ptr_valid(self.pImage) then begin
        if (min(*self.pImage eq image) eq 0) $
        then *self.pImage = image $
        else if (not(self->checkParamsApplied()) and ptr_valid(self.pSegImage)) then return, *self.pSegImage
    endif else self.pImage = ptr_new(image)

    whFrap = (where((*(*self.pParamStruct).pNames) eq 'To FRAP'))[0]
    Frap   = (*(*self.pParamStruct).pValues)[whFrap]

    ; Normalize to 1
    image /= max(image)
    dimImage = size(image, /dim) ; dim2[0] rows, dim2[1] columns, dim2[n] for more dims... ToDo

    ; Touch border for now... avoid border-touching ROIs
    image[0, *] = 0
    image[*, 0] = 0
    image[dimImage[0]-1, *] = 0
    image[*, dimImage[1]-1] = 0

    thinnedImage = image
    iter = 0

    print, "C_ThinningACL80l: Begin of the thinning repeat cycle"

    repeat begin
      iter += 1
      oldImage      = thinnedImage
      roiPositions  = where(thinnedImage eq 1)
      points2delete = [-1]

      for i = 0l, n_elements(roiPositions)-1 do begin
          ; M1
          if ((thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0]]     eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0]]     eq 1) and $
              (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 1)) $
             then points2delete = [points2delete, roiPositions[i]]

          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

          ; N1
          if ((thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0]]     eq 0) and $
              (thinnedImage[roiPositions[i] + 1]               eq 0) and $
              (thinnedImage[roiPositions[i] - 1]               eq 1) and $
              (thinnedImage[roiPositions[i] + dimImage[0]]     eq 1)) $
             then points2delete = [points2delete, roiPositions[i]]

          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

          ; M2
          if ((thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 0) and $
              (thinnedImage[roiPositions[i] + 1]               eq 0) and $
              (thinnedImage[roiPositions[i] - 1]               eq 1) and $
              (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 1)) $
             then points2delete = [points2delete, roiPositions[i]]

          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

          ; N2
          if ((thinnedImage[roiPositions[i] + dimImage[0]]     eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 0) and $
              (thinnedImage[roiPositions[i] + 1]               eq 0) and $
              (thinnedImage[roiPositions[i] - 1]               eq 1) and $
              (thinnedImage[roiPositions[i] - dimImage[0]]     eq 1)) $
             then points2delete = [points2delete, roiPositions[i]]

          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

          ; M3
          if ((thinnedImage[roiPositions[i] + dimImage[0]]     eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0]]     eq 1) and $
              (thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 1)) $
             then points2delete = [points2delete, roiPositions[i]]

          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

          ; N3
          if ((thinnedImage[roiPositions[i] - 1]               eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0]]     eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0]]     eq 1) and $
              (thinnedImage[roiPositions[i] + 1]               eq 1)) $
             then points2delete = [points2delete, roiPositions[i]]

          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

          ; M4
          if ((thinnedImage[roiPositions[i] - 1]               eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 0) and $
              (thinnedImage[roiPositions[i] + dimImage[0] - 1] eq 0) and $
              (thinnedImage[roiPositions[i] + 1]               eq 1) and $
              (thinnedImage[roiPositions[i] - dimImage[0] + 1] eq 1)) $
             then points2delete = [points2delete, roiPositions[i]]

          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

          ; N4
          if ((thinnedImage[roiPositions[i] - 1]               eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0]]     eq 0) and $
              (thinnedImage[roiPositions[i] - dimImage[0] - 1] eq 0) and $
              (thinnedImage[roiPositions[i] + 1]               eq 1) and $
              (thinnedImage[roiPositions[i] + dimImage[0] + 1] eq 1)) $

             then points2delete = [points2delete, roiPositions[i]]
          if (n_elements(points2delete) ne 1) then begin
              thinnedImage[points2delete[1:*]] = 0
              points2delete = [-1]
          endif

      endfor

      if (n_elements(points2delete) ne 1) then thinnedImage[points2delete[1:*]] = 0
      ;print, "thinned image after iteration ", iter
      ;print, thinnedImage

    ; Check if more changes can be made
    endrep until (min(thinnedImage eq oldImage) eq 1)

    print, "C_ThinningACL80l: Thinning completed at iteration ", iter
;    window, 0, xsize = dimImage[0], ysize = dimImage[1], title='original image'
;    tvscl, congrid(image, dimImage[0], dimImage[1])
;    window, 1, xsize = dimImage[0], ysize = dimImage[1], title='thinned image'
;    tvscl, congrid(thinnedImage, dimImage[0], dimImage[1])
;    window, 2, xsize = dimImage[0], ysize = dimImage[1], title='diff. image'
;    tvscl, congrid(image-(thinnedImage)*.5, dimImage[0], dimImage[1])

    if (ptr_valid(self.pSegImage)) $
    then *self.pSegImage = bytArr(dimImage[0], dimImage[1]) $
    else self.pSegImage = ptr_new(bytArr(dimImage[0], dimImage[1]), /no_copy)

    if Frap eq 0 $
    then *self.pSegImage = image - (thinnedImage * 0.5) $
    else *self.pSegImage = thinnedImage

    return, *self.pSegImage
end


function C_sImageFilter_ThinningACL80::init
  filterStruct = { Name       : 'C_ThinningACL80l',$ ; Filter Name.
                   pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                   pNames     : ptr_new(),$ ; Pointer on Filter Parameter Names.
                   pActive    : ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                   pMin       : ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
                   pMax       : ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                   pValues    : ptr_new()}  ; Pointer on Filter Parameter Values.

  ; Parameters of C_ThinningACL80.
  filterParamWidgetType = ['widget_slider']
  filterParamNames      = ['To FRAP']            ; Not used yet
  filterParamActive     = [1]
  filterParamMin        = [0.]
  filterParamMax        = [1.]
  filterParamValues     = [0.]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames      = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive     = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin        = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax        = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues     = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct = ptr_new(filterStruct, /no_copy)
  return, 1
end

pro C_sImageFilter_ThinningACL80__define
  tmp = { C_sImageFilter_ThinningACL80,$
          pParamStruct : ptr_new(),$ ; Pointers to keep the last processed image, saves some time by avoiding re-thinning
          pParamApplied: ptr_new(),$
          pSegImage    : ptr_new(),$
          pIntImage    : ptr_new(),$
          pImage       : ptr_new(),$
          inherits C_sImageFilter }
end
