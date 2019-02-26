;_____________________________IOISIOI_____________________________
; FUNCTION_NAME:
;   s_expand_mirror
;
; PURPOSE:
;   Expands 2D-Image Data by mirroring image edges.
;
; AUTHOR:
;   Dr. Steffen Härtel (2001)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;   result = s_Expand_Mirror( image, pixRad )
;   image : 2D-Array of Image Data.
;   pixRad: Radius (PixelSize) to expand the image.
;
; RETURNS:
;   Image data of dimension [x+pixRad, y+pixRad]
;
;   Case mirrorType of
;     1:   New border is filled with mirrored border values (default)
;     2:   New border is filled with repeated border values FROM THE CONTRARY BORDER
;     3:   New border is not filled
;     4:   New border is filled with repeated border values FROM THE ADJACENT BORDER
;
;   When mirrorType is set to 4 of the output image are set according to the cornerRule:
;
;   Case cornerRule of
;     0:  Corner values set to 0
;     1:  Corner values set to the minimum value in the surrounding pixels from the input image
;     2:  Corner values set to the maximum value in the surrounding pixels from the input image
;     3:  Corner values set to the average value in the surrounding pixels from the input image
;   Default cornerRule value: 2
;
;_____________________________IOISIOI_____________________________

function s_expand_mirror, image, pixRad, mirrorType = mirrorType, cornerRule = cornerRule

  if (n_elements(mirrorType) eq 0) then mirrorType = 1
  if (n_elements(pixRad) eq 1) $
  then pixR = [pixRad, pixRad] $
  else pixR = pixRad

  dimI = size(image, /dim)

  case size(image, /type) of
    0: begin
      message, 'Undefined data type of image in s_expand_mirror'
      return, image
    endcase
    1: outI = bytArr(dimI[0] + 2*pixR[0], dimI[1] + 2*pixR[1])
    2: outI = intArr(dimI[0] + 2*pixR[0], dimI[1] + 2*pixR[1])
    12:outI = intArr(dimI[0] + 2*pixR[0], dimI[1] + 2*pixR[1])
    3: outI = lonArr(dimI[0] + 2*pixR[0], dimI[1] + 2*pixR[1])
    4: outI = fltArr(dimI[0] + 2*pixR[0], dimI[1] + 2*pixR[1])
    5: outI = dblArr(dimI[0] + 2*pixR[0], dimI[1] + 2*pixR[1])
  endcase
  dimM = size(outI, /dim)

    ; set centre of outI to image
  outI[pixR[0]:pixR[0]+dimI[0]-1, pixR[1]:pixR[1]+dimI[1]-1] = image[*,*]

  case mirrorType of
    2: begin  ; create continuous image values from contrary border
      outI[*, 0:pixR[1]-1] = outI[*, dimI[1]:dimI[1]+pixR[1]-1]
      outI[*, pixR[1]+dimI[1] : 2*pixR[1]+dimI[1]-1] = outI[*, pixR[1] : 2*pixR[1]-1]
      outI[0:pixR[0]-1, *] = outI[dimI[0]:dimI[0]+pixR[0]-1, *]
      outI[pixR[0]+dimI[0] : 2*pixR[0]+dimI[0]-1, *] = outI[pixR[0] : 2*pixR[1]-1, *]
    endcase
    3:
    4: begin
        ; first, repeat adjacent borders
      outI[*, pixRad-1] = outI[*, pixRad]
      outI[*, pixRad+dimI[1]] = outI[*, pixRad+dimI[1]-1]
      outI[pixRad-1, *] = outI[pixRad, *]
      outI[pixRad+dimI[0], *] = outI[pixRad+dimI[0]-1, *]
        ; now handle corners
      if (n_elements(cornerRule) eq 0) then cornerRule = 2
      c00 = [image[0,1], image[1,1], image[1,0]]
      c01 = [image[0,dimI[1]-1], image[1,dimI[1]-1], image[1,dimI[1]-1]]
      c10 = [image[dimI[0]-1,0], image[dimI[0]-1,1], image[dimI[0]-1,1]]
      c11 = [image[dimI[0]-1,dimI[1]-1], image[dimI[0]-1,dimI[1]-1], image[dimI[0]-1,dimI[1]-1]]
      case cornerRule of
      0: begin
         outI[pixRad-1      , pixRad-1] = 0
         outI[pixRad+dimI[0], pixRad-1] = 0
         outI[pixRad-1      , pixRad+dimI[1]] = 0
         outI[pixRad+dimI[0], pixRad+dimI[1]] = 0
         endcase
      1: begin
         outI[pixRad-1      , pixRad-1] = min(c00)
         outI[pixRad+dimI[0], pixRad-1] = min(c10)
         outI[pixRad-1      , pixRad+dimI[1]] = min(c01)
         outI[pixRad+dimI[0], pixRad+dimI[1]] = min(c11)
         endcase
      2: begin
         outI[pixRad-1      , pixRad-1] = max(c00)
         outI[pixRad+dimI[0], pixRad-1] = max(c10)
         outI[pixRad-1      , pixRad+dimI[1]] = max(c01)
         outI[pixRad+dimI[0], pixRad+dimI[1]] = max(c11)
         endcase
      3: begin
         outI[pixRad-1      , pixRad-1] = mean(c00)
         outI[pixRad+dimI[0], pixRad-1] = mean(c10)
         outI[pixRad-1      , pixRad+dimI[1]] = mean(c01)
         outI[pixRad+dimI[0], pixRad+dimI[1]] = mean(c11)
         endcase
      endcase
      if (pixRad gt 1) then $
        return, s_expand_mirror(outI[pixRad-1:dimI[0]+pixRad,pixRad-1:dimI[1]+pixRad], pixRad-1, mirrorType = mirrorType, cornerRule = cornerRule)
    endcase
    else: begin  ; mirror image values into new border
      if (pixR[1] eq 1) then begin
        outI[*, 0:pixR[1]-1]= outI[*, pixR[1]+1:2*pixR[1]]
        outI[*, pixR[1]+dimI[1] : 2*pixR[1]+dimI[1]-1] = outI[*, dimI[1]-1:pixR[1]+dimI[1]-2]
      endif else begin
        outI[*, 0:pixR[1]-1] = reverse(outI[*, pixR[1]+1:2*pixR[1]], 2)
        outI[*, pixR[1]+dimI[1] : 2*pixR[1]+dimI[1]-1] = reverse(outI[*, dimI[1]-1:pixR[1]+dimI[1]-2], 2)
      endelse
      outI[0:pixR[0]-1, *] = reverse(outI[pixR[0]+1:2*pixR[0], *], 1)
      outI[pixR[0]+dimI[0] : 2*pixR[0]+dimI[0]-1, *] = reverse(outI[dimI[0]-1:pixR[0]+dimI[0]-2, *], 1)
    endcase
  endcase
  return, outI
end
