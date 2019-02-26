;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjPerimeter
;
; PURPOSE:
;       - Calculation of Object Perimeters and Derived Parameters in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjPerimeter' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjPerimeter::apply, mask = mask, position = position, xySizePerPixel = xySizePerPixel

    whParam = [(where( *(*self.pParamStruct).pNames eq 'Object Perimeter [pixel]'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Perimeter [pixel]'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Delta-Direction'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Curvature'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Perimeter [x]'))[0],$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Delta-Direction div P'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Delta-Direction div A'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Curvature div P'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object Chain Curvature div A'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object P²A [pixel]'))[0] ,$
                  (where( *(*self.pParamStruct).pNames eq 'Object P²A [x]'))[0] ]

       ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1: if (position[0] eq -1) then return else  whParamActive[position] = 1
       else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase

    if whParamActive[5] then begin    $  ; if 'Object Chain Delta-Direction div P' then activate
       whParamActive[2] = 1               ;'Object Chain Delta-Direction'
       (*(*self.pParamStruct).pActive)[whParam[2]] = 1
       whParamActive[4] = 1               ;'Object Chain Perimeter [x]'
       (*(*self.pParamStruct).pActive)[whParam[4]] = 1
    endif
    if whParamActive[7] then begin    $  ; if 'Object Chain Curvature div P' then activate
       whParamActive[3] = 1               ;'Object Chain Curvature'
       (*(*self.pParamStruct).pActive)[whParam[3]] = 1
       whParamActive[4] = 1               ;'Object Chain Perimeter [x]'
       (*(*self.pParamStruct).pActive)[whParam[4]] = 1
    endif
    if whParamActive[9] then begin    $  ; if 'Object P²A [pixel]' then activate
       whParamActive[0] = 1               ;'Object Chain Curvature'
       (*(*self.pParamStruct).pActive)[whParam[0]] = 1
    endif
    if whParamActive[10] then begin   $ ; if 'Object P²A [x]' then activate
       whParamActive[4] = 1               ;'Object Chain Perimeter [x]'
       (*(*self.pParamStruct).pActive)[whParam[4]] = 1
    endif

       ; check Pointers
    wherePA = where(whParamActive eq 1)
    if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new(-1, /no_copy)
    if (wherePA[0] eq -1) then return
    for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

    if whParamActive[4] then begin    ;Object Chain Perimeter [x]
       (*(*(*self.pValueStruct)[whParam[4]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[4]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
       (*(*(*self.pValueStruct)[whParam[4]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[4]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]
    endif
    if whParamActive[6] then begin $  ;Object Chain Delta-Direction div A
       (*(*(*self.pValueStruct)[whParam[6]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[6]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
       (*(*(*self.pValueStruct)[whParam[6]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[6]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]
       factor = 1.* xySizePerPixel[0] * xySizePerPixel[1]
    endif
    if whParamActive[8] then begin $  ;Object Chain Curvature div A
       (*(*(*self.pValueStruct)[whParam[8]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[8]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
       (*(*(*self.pValueStruct)[whParam[8]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[8]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]
       factor = 1.* xySizePerPixel[0] * xySizePerPixel[1]
    endif
    if whParamActive[10] then begin   $ ;Object P²A [x]
       (*(*(*self.pValueStruct)[whParam[10]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[10]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
       (*(*(*self.pValueStruct)[whParam[10]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[10]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]
       factor = 1.* xySizePerPixel[0] * xySizePerPixel[1]
    endif

    hmax = max(mask)
    if (hmax eq 0) then begin
       *(*self.pParamStruct).pROINumberVect = -1
       for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
    endif else begin
         ;Object Perimeter [pixel]
       if whParamActive[0] then begin
         if ((*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Four Neigbor'))[0]])  then begin
            peri4Mask = s_ObjBorder_4or8(mask, /four) * mask
            hmin = min(peri4Mask[where(peri4Mask ne 0)])
            hist = histogram(peri4Mask, min = hmin, max = hmax)
         endif else begin
            peri8Mask = s_ObjBorder_4or8(mask, /eight) * mask
            hmin = min(peri8Mask[where(peri8Mask ne 0)])
            hist = histogram(peri8Mask, min = hmin, max = hmax)
         endelse
         whereHist = where(hist ne 0)
         *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = hist[whereHist]
       endif
         ;Object Chain Perimeter [pixel]
       if whParamActive[1] then begin
         if ((*(*(*self.pValueStruct)[whParam[1]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'Four Neigbor'))[0]])  then begin
          if (n_elements(peri4Mask) eq 0) then peri4Mask = s_ObjBorder_4or8(mask, /four)  * mask
          chain4Code = s_objBorder_chainCode(peri4Mask)               ; Generate  Chain-Code
          *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = s_objBorder_chainCodeLength(chain4Code)        ; Calculate  Chain-Length
         endif else begin
          if (n_elements(peri8Mask) eq 0) then peri8Mask = s_ObjBorder_4or8(mask, /eight)  * mask
          chain8Code = s_objBorder_chainCode(peri8Mask)               ; Generate  Chain-Code
          *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = s_objBorder_chainCodeLength(chain8Code)        ; Calculate  Chain-Length
         endelse
       endif
         ;Object Chain Delta-Direction or Object Chain Curvature
       if ((whParamActive[2]) or (whParamActive[3])) then begin

         if ( ((*(*(*self.pValueStruct)[whParam[2]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'Four Neigbor'))[0]])  or $
             ((*(*(*self.pValueStruct)[whParam[3]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[3]]).pNames eq 'Four Neigbor'))[0]]) ) then begin
          if (n_elements(peri4Mask) eq 0) then peri4Mask = s_ObjBorder_4or8(mask, /four)  * mask
          if (n_elements(chain4Code) eq 0) then chain4Code = s_objBorder_chainCode(peri4Mask)               ; Generate  Chain-Code

          peri4Res = chain4Code[*,0:1]
          chain4Code = chain4Code[*,2:*]

          dimCode = size(chain4Code, /dim)
          curve = intArr(dimCode[0], dimCode[1])
          curve[ *, 0:dimCode[1]-2] = chain4Code[*, 1:dimCode[1]-1] - chain4Code[*, 0:dimCode[1]-2]

          for i = 0,dimCode[0]-1 do begin
              j = where(chain4Code[i,*] eq -1)
              if (n_elements(j) ne dimCode[1]) then if (total(j) ne -1) then curve[i,j[0]-1] = chain4Code[i,0] - chain4Code[i,j[0]-1] else $
                 curve[i,dimCode[0]-1] = chain4Code[i,0] - chain4Code[i,dimCode[0]-1]
          endfor

          j = where(chain4Code eq 0)
          if (total(j) ne -1) then curve[j]= ([0,1,2,3,4,-3,-2,-1])[curve[j]]
          j = where(chain4Code eq 1)
          if (total(j) ne -1) then curve[j]= ([-1,0,1,2,3,4,-3,-2])[curve[j]+1]
          j = where(chain4Code eq 2)
          if (total(j) ne -1) then curve[j]= ([-2,-1,0,1,2,3,4,-3])[curve[j]+2]
          j = where(chain4Code eq 3)
          if (total(j) ne -1) then curve[j]= ([-3,-2,-1,0,1,2,3,4])[curve[j]+3]
          j = where(chain4Code eq 4)
          if (total(j) ne -1) then curve[j]= ([4,-3,-2,-1,0,1,2,3])[curve[j]+4]
          j = where(chain4Code eq 5)
          if (total(j) ne -1) then curve[j]= ([3,4,-3,-2,-1,0,1,2])[curve[j]+5]
          j = where(chain4Code eq 6)
          if (total(j) ne -1) then curve[j]= ([2,3,4,-3,-2,-1,0,1])[curve[j]+6]
          j = where(chain4Code eq 7)
          if (total(j) ne -1) then curve[j]= ([1,2,3,4,-3,-2,-1,0])[curve[j]+7]

          dummy = fltArr(dimCode[0])
          if ((whParamActive[3]) and ((*(*(*self.pValueStruct)[whParam[3]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[3]]).pNames eq 'Four Neigbor'))[0]]))  then begin
              for i = 0,dimCode[0]-1 do begin
                 j = where(chain4Code[i,*] ne -1)
                 if (n_elements(j) ne dimCode[1]) then begin
                   if (total(j) ne -1) then dummy[i] = total(abs(curve[i,j]))
                 endif else begin
                   dummy[i] = 0.0
                 endelse
              endfor
              *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = dummy
          endif

          j = where(curve eq 0)
          if (total(j) ne -1) then curve(j) = 1
          dummy = fltArr(dimCode[0])
          if ((whParamActive[2]) and ((*(*(*self.pValueStruct)[whParam[2]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'Four Neigbor'))[0]]))  then begin
              curve(*,0:dimCode[1]-2) = curve[*,1:dimCode[1]-1] * curve[*,0:dimCode[1]-2]
              for i = 0,dimCode[0]-1 do begin
                 j = where(chain4Code[i,*] eq -1)
                 if (n_elements(j) ne dimCode[1]) then $
                   if (total(j) ne -1) then curve[i,j[0]-1] = curve[i,0] * curve[i,j[0]-1] else $
                   curve[i,dimCode[0]-1] = curve[i,0] * curve[i,dimCode[0]-1]
                 dummy[i] = total(curve[i,*] lt 0)
              endfor
              *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = dummy
          endif

          chain4Code = transpose([transpose(peri4Res), transpose(chain4Code)])
         endif

         if ( not((*(*(*self.pValueStruct)[whParam[2]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'Four Neigbor'))[0]])  or $
             not((*(*(*self.pValueStruct)[whParam[3]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'Four Neigbor'))[0]]) ) then begin
          if (n_elements(peri8Mask) eq 0) then peri8Mask = s_ObjBorder_4or8(mask, /eight)  * mask
          if (n_elements(chain8Code) eq 0) then chain8Code = s_objBorder_chainCode(peri8Mask)               ; Generate  Chain-Code

          peri8Res = chain8Code[*,0:1]
          chain8Code = chain8Code[*,2:*]

          dimCode = size(chain8Code, /dim)
          curve = intArr(dimCode[0], dimCode[1])
          curve[ *, 0:dimCode[1]-2] = chain8Code[*, 1:dimCode[1]-1] - chain8Code[*, 0:dimCode[1]-2]

          for i = 0,dimCode[0]-1 do begin
              j = where(chain8Code[i,*] eq -1)
              if (n_elements(j) ne dimCode[1]) then if (total(j) ne -1) then curve[i,j[0]-1] = chain8Code[i,0] - chain8Code[i,j[0]-1] else $
                 curve[i,dimCode[0]-1] = chain8Code[i,0] - chain8Code[i,dimCode[0]-1]
          endfor

          j = where(chain8Code eq 0)
          if (total(j) ne -1) then curve[j]= ([0,1,2,3,4,-3,-2,-1])(curve[j])
          j = where(chain8Code eq 1)
          if (total(j) ne -1) then curve[j]= ([-1,0,1,2,3,4,-3,-2])[curve[j]+1]
          j = where(chain8Code eq 2)
          if (total(j) ne -1) then curve[j]= ([-2,-1,0,1,2,3,4,-3])[curve[j]+2]
          j = where(chain8Code eq 3)
          if (total(j) ne -1) then curve[j]= ([-3,-2,-1,0,1,2,3,4])[curve[j]+3]
          j = where(chain8Code eq 4)
          if (total(j) ne -1) then curve[j]= ([4,-3,-2,-1,0,1,2,3])[curve[j]+4]
          j = where(chain8Code eq 5)
          if (total(j) ne -1) then curve[j]= ([3,4,-3,-2,-1,0,1,2])[curve[j]+5]
          j = where(chain8Code eq 6)
          if (total(j) ne -1) then curve[j]= ([2,3,4,-3,-2,-1,0,1])[curve[j]+6]
          j = where(chain8Code eq 7)
          if (total(j) ne -1) then curve[j]= ([1,2,3,4,-3,-2,-1,0])[curve[j]+7]

          dummy = fltArr(dimCode[0])
          if ((whParamActive[3]) and not((*(*(*self.pValueStruct)[whParam[3]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[3]]).pNames eq 'Four Neigbor'))[0]]))  then begin
              for i = 0,dimCode[0]-1 do begin
                 j = where(chain8Code[i,*] ne -1)
                 if (n_elements(j) ne dimCode[1]) then begin
                   if (total(j) ne -1) then dummy[i] = total(abs(curve[i,j]))
                 endif else begin
                   dummy[i] = 0.0
                 endelse
              endfor
              *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = dummy
          endif

          j = where(curve eq 0)
          if (total(j) ne -1) then curve(j) = 1
          dummy = fltArr(dimCode[0])
          if ((whParamActive[2]) and not((*(*(*self.pValueStruct)[whParam[2]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[2]]).pNames eq 'Four Neigbor'))[0]]))  then begin
              curve(*,0:dimCode[1]-2) = curve[*,1:dimCode[1]-1] * curve[*,0:dimCode[1]-2]
              for i = 0,dimCode[0]-1 do begin
                 j = where(chain8Code[i,*] eq -1)
                 if (n_elements(j) ne dimCode[1]) then $
                   if (total(j) ne -1) then curve[i,j[0]-1] = curve[i,0] * curve[i,j[0]-1] else $
                   curve[i,dimCode[0]-1] = curve[i,0] * curve[i,dimCode[0]-1]
                 dummy[i] = total(curve[i,*] lt 0)
              endfor
              *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = dummy
          endif
          chain8Code = transpose([transpose(peri8Res), transpose(chain8Code)])
         endif
       endif
         ; Object Chain Perimeter [x]
       if whParamActive[4] then begin
         if ((*(*(*self.pValueStruct)[whParam[4]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[4]]).pNames eq 'Four Neigbor'))[0]])  then begin
          if (n_elements(peri4Mask) eq 0) then peri4Mask = s_ObjBorder_4or8(mask, /four)  * mask
          if (n_elements(chain4Code) eq 0) then chain4Code = s_objBorder_chainCode(peri4Mask)               ; Generate  Chain-Code
          *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = s_objBorder_chainCodeLength(chain4Code, xySizePerPixel = xySizePerPixel)         ; Calculate  Chain-Length
         endif else begin
          if (n_elements(peri8Mask) eq 0) then peri8Mask = s_ObjBorder_4or8(mask, /eight)  * mask
          if (n_elements(chain8Code) eq 0) then chain8Code = s_objBorder_chainCode(peri8Mask)               ; Generate  Chain-Code
          *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = s_objBorder_chainCodeLength(chain8Code, xySizePerPixel = xySizePerPixel)         ; Calculate  Chain-Length
         endelse
       endif
         ;Object Chain Delta-Direction div P
       if whParamActive[5] then $
         *(*(*self.pValueStruct)[whParam[5]]).pROIParamVect = $
          (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect) / (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)
         ;Object Chain Delta-Direction div A
       if whParamActive[6] then begin
         hmin = min(mask[where(mask ne 0)])     ; Calculate Object Sizes
         areaHist = 1. * histogram(mask, min = hmin, max = hmax)
         whereAreaHist = where(areaHist ne 0)
         *(*(*self.pValueStruct)[whParam[6]]).pROIParamVect = $
          (*(*(*self.pValueStruct)[whParam[2]]).pROIParamVect) / (areaHist[whereAreaHist]*factor)
       endif
         ;Object Chain Curvature div P
       if whParamActive[7] then $
         *(*(*self.pValueStruct)[whParam[7]]).pROIParamVect = $
          (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect) / (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect)
         ;Object Chain Curvature div A
       if whParamActive[8] then begin
         if (n_elements(whereAreaHist) le 0) then begin
          hmin = min(mask[where(mask ne 0)])   ; Calculate Object Sizes
          areaHist = 1. * histogram(mask, min = hmin, max = hmax)
          whereAreaHist = where(areaHist ne 0)
         endif
         *(*(*self.pValueStruct)[whParam[8]]).pROIParamVect = $
          (*(*(*self.pValueStruct)[whParam[3]]).pROIParamVect) / (areaHist[whereAreaHist]*factor)
       endif
         ;Object P²A [pixel]
       if whParamActive[9] then begin
         if (n_elements(whereAreaHist) le 0) then begin
          hmin = min(mask[where(mask ne 0)])   ; Calculate Object Sizes
          areaHist = 1. * histogram(mask, min = hmin, max = hmax)
          whereAreaHist = where(areaHist ne 0)
         endif
         *(*(*self.pValueStruct)[whParam[9]]).pROIParamVect = $
          (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect) * (*(*(*self.pValueStruct)[whParam[0]]).pROIParamVect) / (areaHist[whereAreaHist])
       endif
         ;Object P²A [x]
       if whParamActive[10] then begin
         if (n_elements(whereAreaHist) le 0) then begin
          hmin = min(mask[where(mask ne 0)])   ; Calculate Object Sizes
          areaHist = 1. * histogram(mask, min = hmin, max = hmax)
          whereAreaHist = where(areaHist ne 0)
         endif
         *(*(*self.pValueStruct)[whParam[10]]).pROIParamVect = $
          (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect) * (*(*(*self.pValueStruct)[whParam[4]]).pROIParamVect) / (areaHist[whereAreaHist]*factor)
       endif

       if (n_elements(whereHist) eq 0) then begin
         case (n_elements(peri4Mask) ne 0) of
          1: begin
                 if (n_elements(peri4Mask) eq 0) then peri4Mask = s_ObjBorder_4or8(mask, /four)  * mask
                 hmin = min(peri4Mask[where(peri4Mask ne 0)])
                 hist = histogram(peri4Mask, min = hmin, max = hmax)
              endcase
          else: begin
                 if (n_elements(peri8Mask) eq 0) then peri8Mask = s_ObjBorder_4or8(mask, /eight)  * mask
                 hmin = min(peri8Mask[where(peri8Mask ne 0)])
                 hist = histogram(peri8Mask, min = hmin, max = hmax)
              endcase
         endcase
         whereHist = where(hist ne 0)
       endif
       (*self.pParamStruct).pROINumberVect = ptr_new( (whereHist + hmin), /no_copy)
    endelse
end


function C_sROIParam_ObjPerimeter::init

    ROIParamStruct = {name:'Object Perimeter',$     ;  ROI Name.
                    type:   'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect: ptr_new()   }     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(11))
    ROIParamWidgetType = make_array(11, /string, value = 'widget_slider')
    ROIParamNames = [ 'Object Perimeter [pixel]',$
                           'Object Chain Perimeter [pixel]',$
                           'Object Chain Delta-Direction',$
                           'Object Chain Curvature',$
                           'Object Chain Perimeter [x]',$
                           'Object Chain Delta-Direction div P',$
                           'Object Chain Delta-Direction div A',$
                           'Object Chain Curvature div P',$
                           'Object Chain Curvature div A',$
                           'Object P²A [pixel]',$
                           'Object P²A [x]']

    ROIParamActive = [0,0,1,1,1,1,1,1,1,1,1]
    ROIParamMin = [0,0,0,0,0,0,0,0,0,0,0]
    ROIParamMax = [0,0,0,0,0,0,0,0,0,0,0]
    ROIParamValues = [0,0,0,0,0,0,0,0,0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name: 'Object Perimeter [pixel]',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(9, /string, value = 'widget_slider')
    ROIValueNames = ['Four Neigbor',$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueActive = make_array(10, /byte, value = 0)
    ROIValueActive[0] = 1
    ROIValueMin = make_array(10, /float, value = 0)
    ROIValueMax = make_array(10, /float, value = 1)
    ROIValueValues =[0., 0., 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]

    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Object Chain Perimeter [pixel]',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIValueWidgetType]
    ROIValueNames = [ROIValueNames]
    ROIValueActive = [ROIValueActive]
    ROIValueMin = [ROIValueMin]
    ROIValueMax = [ROIValueMax]
    ROIValueValues =[ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Object Chain Delta-Direction',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ ROIValueWidgetType]
    ROIValueNames = [ ROIValueNames]
    ROIValueActive = [ ROIValueActive]
    ROIValueMin = [ ROIValueMin]
    ROIValueMax = [ ROIValueMax]
    ROIValueValues =[ ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Object Chain Curvature',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ ROIValueWidgetType]
    ROIValueNames = [ ROIValueNames]
    ROIValueActive = [ ROIValueActive]
    ROIValueMin = [ ROIValueMin]
    ROIValueMax = [ ROIValueMax]
    ROIValueValues =[ ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Object Chain Perimeter [x]',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ 'widget_slider', 'widget_slider', ROIValueWidgetType]
    ROIValueNames = [ 'x-Size per Pixel', 'y-Size per Pixel', ROIValueNames]
    ROIValueActive = [ 1, 1, ROIValueActive]
    ROIValueMin = [ 0., 0., ROIValueMin]
    ROIValueMax = [ 1000., 1000., ROIValueMax]
    ROIValueValues =[ 1., 1., ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name: 'Object Chain Delta-Direction div P',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ ROIValueWidgetType[3:*]]
    ROIValueNames = [ ROIValueNames[3:*]]
    ROIValueActive = [ ROIValueActive[3:*]]
    ROIValueMin = [ ROIValueMin[3:*]]
    ROIValueMax = [ ROIValueMax[3:*]]
    ROIValueValues =[ ROIValueValues[3:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[5] = ptr_new(ROIValueStruct, /no_copy)


    ROIValueStruct = {name: 'Object Chain Delta-Direction div A',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ 'widget_slider', 'widget_slider', ROIValueWidgetType]
    ROIValueNames = [ 'x-Size per Pixel', 'y-Size per Pixel', ROIValueNames]
    ROIValueActive = [ 1, 1, ROIValueActive]
    ROIValueMin = [ 0., 0., ROIValueMin]
    ROIValueMax = [ 1000., 1000., ROIValueMax]
    ROIValueValues =[ 1., 1., ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[6] = ptr_new(ROIValueStruct, /no_copy)


    ROIValueStruct = {name: 'Object Chain Curvature div P',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ ROIValueWidgetType[2:*]]
    ROIValueNames = [ ROIValueNames[2:*]]
    ROIValueActive = [ ROIValueActive[2:*]]
    ROIValueMin = [ ROIValueMin[2:*]]
    ROIValueMax = [ ROIValueMax[2:*]]
    ROIValueValues =[ ROIValueValues[2:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[7] = ptr_new(ROIValueStruct, /no_copy)


    ROIValueStruct = {name: 'Object Chain Curvature div A',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ 'widget_slider', 'widget_slider', ROIValueWidgetType]
    ROIValueNames = [ 'x-Size per Pixel', 'y-Size per Pixel', ROIValueNames]
    ROIValueActive = [ 1, 1, ROIValueActive]
    ROIValueMin = [ 0., 0., ROIValueMin]
    ROIValueMax = [ 1000., 1000., ROIValueMax]
    ROIValueValues =[ 1., 1., ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[8] = ptr_new(ROIValueStruct, /no_copy)


    ROIValueStruct = {name: 'Object P²A [pixel]',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ ROIValueWidgetType[2:*]]
    ROIValueNames = [ ROIValueNames[2:*]]
    ROIValueActive = [ ROIValueActive[2:*]]
    ROIValueMin = [ ROIValueMin[2:*]]
    ROIValueMax = [ ROIValueMax[2:*]]
    ROIValueValues =[ ROIValueValues[2:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[9] = ptr_new(ROIValueStruct, /no_copy)


    ROIValueStruct = {name: 'Object P²A [x]',$
                    type: 'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect: ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ 'widget_slider', 'widget_slider', ROIValueWidgetType]
    ROIValueNames = [ 'x-Size per Pixel', 'y-Size per Pixel', ROIValueNames]
    ROIValueActive = [ 1, 1, ROIValueActive]
    ROIValueMin = [ 0., 0., ROIValueMin]
    ROIValueMax = [ 1000., 1000., ROIValueMax]
    ROIValueValues =[ 1., 1., ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[10] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end

pro C_sROIParam_ObjPerimeter__define
   tmp = {C_sROIParam_ObjPerimeter, pParamStruct: ptr_new(),$
                                  pValueStruct: ptr_new(),$
                                  inherits C_sROIParam }
end