function C_sImageFilter_AngularCut::getImageFilterType
  return, 'Single_Image_Filter_Method'
end

pro C_sImageFilter_AngularCut::draw_line, image = image, startx = startx, starty = starty, endx = endx, endy = endy
  image[startx, starty] = 1
  dimI = size(image, /dim)
  if ((endy ge 0) and (endy lt dimI[1])) then image[endx, endy] = 1
  if (endx eq startx) then begin
    inc = 1
    if (starty gt endy) then inc = -1
    endy = (endy > 0) < (dimI[1] - 1)
    for i = starty, endy, inc do begin
      if (startx lt dimI[0]) then image[startx, i] = 1
    endfor
    return
  endif
  slope = (1.0 * endy - starty) / (endx - startx)
  base = 1.0 * endy - slope * endx
  inc = 1
  if (endx lt startx) then inc = -1
  for xpos = startx, endx, inc do begin ; TODO Make a better stop criterium
    ypos = round(slope * xpos + base)
    if ((xpos lt dimI[0]) and (ypos lt dimI[1]) and (ypos ge 0)) then image[xpos, ypos] = 1
  end
end

pro C_sImageFilter_AngularCut::line_Bresenham, image, _x0, _x1, _y0, _y1
  x0 = _x0
  x1 = _x1
  y0 = _y0
  y1 = _y1
  steep = abs(y1 - y0) gt abs(x1 - x0)
  szi = size(image, /dim)
  if (steep eq 1) then begin
    x0 = _y0
    y0 = _x0
    x1 = _y1
    y1 = _x1
  endif
  if x0 gt x1 then begin
    tmp = x0
    x0 = x1
    x1 = tmp
    tmp = y0
    y0 = y1
    y1 = tmp
  endif
  deltax = x1 - x0
  deltay = abs(y1 - y0)
  error = 0d
  deltaerr = 1.0 * deltay / deltax
  ystep = -1
  y = y0
  print, 'going from (', x0, ',',y0, ') to (', x1, ',', y1, ')'
  if y0 lt y1 then ystep = 1 else ystep = -1
  for x = x0, x1 do begin
    ; TODO JJ obviously there is a better way to do this... ;)
    if steep then begin
      if ((y ge 0) and (y lt szi[0]) and (x ge 0) and (x lt szi[1])) then image[y, x] = 1
    endif else begin
        if ((x ge 0) and (x lt szi[0]) and (y ge 0) and (y lt szi[1])) then image[x, y] = 1
    endelse
    error = error + deltaerr
    if error ge 0.5 then begin
      y = y + ystep
      error = error - 1.0
    endif
  endfor
end

pro C_sImageFilter_AngularCut::fill_region, image = image, refPos = refPos
  ;print, image
  szImg = size(image, /dim)
  ;window, 31, xsize = szImg[0] + 5, ysize = szImg[1] + 10
  queuePoints = [-1, refPos]
  fEnd = 0b
  while (fEnd ne 1) do begin
    last = n_elements(queuePoints) - 1
    pos = queuePoints[last]
    if (image[pos] eq 0) then image[pos] = 1
    queuePoints = queuePoints[0 : last - 1]

    posUp = pos - szImg[0]
    posDn = pos + szImg[0]
    posR = pos + 1
    posL = pos - 1
    up = posUp ge 0
    if up then up = up and (image[posUp] eq 0)
    down = posDn lt n_elements(image)
    if down then down = down and (image[posDn] eq 0)
    right = posR lt n_elements(image)
    if right then right = right and ((posR mod szImg[0]) ne 0) and (image[posR] eq 0)
    left = posL ge 0
    if left then left = left and ((pos mod szImg[0]) ne 0) and (image[posL] eq 0)
    ; west
    if (left) then begin
      image[posL] = 1
      queuePoints = [queuePoints, posL]
    endif
    ; east
    if (right) then begin
      image[posR] = 1
      queuePoints = [queuePoints, posR]
    endif
    ; north
    if (up) then begin
      image[posUp] = 1
      queuePoints = [queuePoints, posUp]
    endif
    ; south
    if (down) then begin
      image[posDn] = 1
      queuePoints = [queuePoints, posDn]
    endif
    fEnd = n_elements(queuePoints) eq 1
  endwhile
  ;print, image
  ;wset, 31
  ;tvscl, image
end

function C_sImageFilter_AngularCut::whichPath, image = image, pos = pos, varDirection = varDirection
  dimi = size(image, /dim)
  posUp = pos - dimi[0]
  posDn = pos + dimi[0]
  posR = pos + 1
  posL = pos - 1
  up = (posUp ge 0) and (image[posUp] eq 0)
  down = ((posDn) le n_elements(image)) and (image[posDn] eq 0)
  right = (posR le n_elements(image)) and ((posR mod dimi[0]) ne 0) and (image[posR] eq 0)
  left = (posL ge 0) and ((posL mod dimi[0]) ne (dimi[0] - 1)) and (image[posL] eq 0)
  varDirection = [0b, 0b, 0b, 0b]
  dirCount = 0b
  if up then begin
    varDirection[0] = 1b
    dirCount += 1
  endif
  if right then begin
    varDirection[1] = 1b
    dirCount += 1
  endif
  if down then begin
    varDirection[2] = 1b
    dirCount += 1
  endif
  if left then begin
    varDirection[3] = 1b
    dirCount += 1
  endif
  return, dirCount
end


; Based on http://en.wikipedia.org/wiki/Floodfill the right-hand method but with a simplfied case
pro C_sImageFilter_AngularCut::fill_r, image = image, refPos = refPos
  szImg = size(image, /dim)
  szX = szImg[0]
  queuePoints = [-1]
  fEnd = 0b
  pos = refPos
  while (not fEnd) do begin
   dir = self->whichPath(image = image, pos = pos, varDir = dirs)
    case dir of
      0: begin ; paint reference position and exit
        image[pos] = 1
        queueLen = n_elements(queuePoints)
        if (queueLen eq 1) then fEnd = 1b else begin
          ; Jump to point stored in position queueLen-2, and "shift" by an amount of pixels given by pos. queueLen-1
          pos = queuePoints[queueLen - 2 + queuePoints[queueLen - 1]]
          queuePoints = queuePoints[0 : queueLen - 3]
        endelse
      endcase
      1: begin
        image[pos] = 1
        if (varDir[0]) then pos = pos - szX
        if (varDir[1]) then pos = pos + 1
        if (varDir[2]) then pos = pos + szX
        if (varDir[3]) then pos = pos - 1
      endcase
      2: begin
      endcase
      3: begin
      endcase
      4: begin ; there is a more advanced implementation for complex shapes... not used here
        image[pos - szX] = 1
        image[pos + 1] = 1
        image[pos + szX] = 1
        pos = pos - 1
      endcase
    endcase
  endwhile
end

; The idea is to get a "pizza cut" portion of the image, from a given start point, a certain angle and a shift
function C_sImageFilter_AngularCut::apply, image = image
  if (size(image, /n_dim) ne 2) then return, image

  dimI = size (image, /dim)
  szX = dimI[0]
  szY = dimI[1]
  startX = szX / 2 ; When the operands are integer, the integer part of the division is returned.
  startY = szY / 2

  whParam = (where((*(*self.pParamStruct).pNames) eq 'x_Center'))[0]
  if ((*(*self.pParamStruct).pValues)[whParam] ge 0) then startX = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'y_Center'))[0]
  if ((*(*self.pParamStruct).pValues)[whParam] ge 0) then startY = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Degrees'))[0]
  degrees = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Shift'))[0]
  zeroShift = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'Sector_to_show'))[0]
  theSectorNum = (*(*self.pParamStruct).pValues)[whParam]
  whParam = (where((*(*self.pParamStruct).pNames) eq 'display_cut_region_window'))[0]
  fDisplayWindow = (*(*self.pParamStruct).pValues)[whParam]

  numSectors = round(360 / degrees)
  maskImage = bytArr(szX, szY)

  print, 'segment sector ', theSectornum, ' of [0..', numSectors-1, '], each of ', degrees, ', degrees, center is (', startX, ',', startY, ')' 
  ; compute the quadrants for start and end of the "pizza cut"

  startQuadrant = floor((1.0 * theSectorNum * degrees + zeroShift) / 90)
  deltaDegrees = (theSectorNum * degrees + zeroShift) - (startQuadrant * 90)
  print, 'degrees in the start quadrant: ', deltaDegrees
  x = 0
  y = 0
  thresh90 = (!pi - 0.1) / 2
  startQuadrant = startQuadrant mod 4
  case startQuadrant of
  0: begin
    x = szX - 1
    angle = (90 - deltaDegrees) * !pi / 180
    if (angle ge (thresh90)) then begin
      y = 0
      x = startX
    endif else y = startY - tan(angle) * (x - startX)
  endcase
  1: begin
    x = szX - 1
    angle = !pi * deltaDegrees / 180
    if (angle ge thresh90) then begin
      y = szI - 1
      x = startX
    endif else y = startY + tan(angle) * (x - startX)
  endcase
  2: begin
    x = 0
    angle = (90 - deltaDegrees) * !pi / 180
    if (angle ge thresh90) then begin
      y = szY - 1
      x = startx
    endif else y = startY + tan(angle) * startX
  endcase
  3: begin
    x = 0
    angle = !pi * deltaDegrees / 180
    if (angle ge thresh90) then begin
      despy = szI - 1
      x = startx
    endif else y = startY - tan(angle) * startX
  endcase
  endcase
  print, 'start line in quadrant ', startQuadrant, ' from (', startX, ',', startY, ') to (', x, ',', y, ')'
  ;self->draw_line, image = maskImage, startX = startX, starty = startY, endx = x, endy = round(y)
  self->line_Bresenham, maskImage, startX, x, startY, y

  endQuadrant = floor(((1 + theSectorNum) * degrees + zeroShift) / 90)
  deltaDegrees = ((theSectorNum + 1) * degrees)  + zeroShift - (endQuadrant * 90)
  print, 'degrees in the end quadrant: ', deltaDegrees
  xx = 0
  yy = 0
  endQuadrant = endQuadrant mod 4
  case endQuadrant of
  0: begin
    xx = szX - 1
    angle = (90 - deltaDegrees) * !pi / 180
    if (angle ge (thresh90)) then begin
      yy = 0
      xx = startX
    endif else yy = startY - tan(angle) * (xx - startX)
  endcase
  1: begin
    xx = szX - 1
    angle = !pi * deltaDegrees / 180
    if (angle ge thresh90) then begin
      yy = szI - 1
      xx = startX
    endif else yy = startY + tan(angle) * (xx - startX)
  endcase
  2: begin
    xx = 0
    angle = (90 - deltaDegrees) * !pi / 180
    if (angle ge thresh90) then begin
      yy = szY - 1
      xx = startx
    endif else yy = startY + tan(angle) * startX
  endcase
  3: begin
    xx = 0
    angle = !pi * deltaDegrees / 180
    if (angle ge thresh90) then begin
      despy = szI - 1
      xx = startx
    endif else yy = startY - tan(angle) * startX
  endcase
  endcase
  print, 'end line in quadrant ', endQuadrant, ' from (', startX, ',', startY, ') to (', xx, ',', yy, ')'
  self->line_Bresenham, maskImage, startX, xx, startY, yy
  ;self->draw_line, image = maskImage, startX = startX, starty = startY, endx = xx, endy = yy

  px = round((startx + ((x>1) < dimi[0]) + ((xx > 1) < dimi[0])) / 3)
  py = round((starty + ((y>1) < dimi[1]) + ((yy > 1) < dimi[1])) / 3)

  pcount = (py * dimi[0]) + px
  dx = pcount mod dimi[0]
  nx = floor(pcount / dimi[0])
  rp = (nx*dimi[0]) + dx
  if (py le 0) then rp = px
  
  self->fill_region, image = maskImage, refPos = rp

  whPoints = where(maskImage ne 1)
  if n_elements(whPoints) gt 1 then image[whPoints] = 0
  if fDisplayWindow then begin
    window, 31, xsize = 300, ysize = 300
    tvscl, congrid(maskimage, 300, 300)
  endif
  return, image
end

function C_sImageFilter_AngularCut::init

  filterStruct = {Name: 'C_AngularCut',$             ; Filter Name.
                  pWidgetType: ptr_new(),$           ; Pointer on Filter Parameter Names.
                  pNames: ptr_new(),$                ; Pointer on Filter Parameter Names.
                  pImagefilterParamType: ptr_new(),$ ; Pointer on Filter Parameter Type (Bool, Byte, Int, Long, Float, Double).
                  pActive: ptr_new(),$               ; Pointer on Filter Parameter Active Bool.
                  pMin: ptr_new(),$                  ; Pointer on Filter Parameter Min_Values.
                  pMax: ptr_new(),$                  ; Pointer on Filter Parameter Max_Values.
                  pValues: ptr_new()}                ; Pointer on Filter Parameter Values.

  ; Widget for parameters setting. The number of elements must match the size of the following parameter-feature arrays.
  filterParamWidgetType = make_array(6, /string, value = 'widget_slider')

  filterParamNames = ['Degrees',$        ; Number of sexagesimal degrees to perform the cut.
                      'Shift',$          ; Defines a shift for the 0° line (default is center-top).
                      'Sector_to_show',$ ; The number of the sector to be shown in the resulting image. 0 is the first.
                      'x_Center',$       ; x coordinate of the center. A value of -1 indicates the half of the image width.
                      'y_Center',$        ; y coordinate of the center. A value of -1 indicates the half of the image height.
                      'display_cut_region_window'$
                      ]

  filterParamType = ['Int',$
                     'Int',$
                     'Int',$
                     'Int',$
                     'Int',$
                     'Int']

  filterParamActive = [1,$
                       1,$
                       1,$
                       1,$
                       1,$
                       1$
                       ]

  filterParamMin = [1,$
                    0,$
                    0,$
                    -1,$
                    -1,$
                    0]

  filterParamMax = [360,$
                    359,$
                    359,$
                    10000,$
                    10000,$
                    1]

  filterParamValues = [60,$
                       0,$
                       0,$
                       -1,$
                       -1,$
                       0]

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

pro C_sImageFilter_AngularCut__define
  tmp = {C_sImageFilter_AngularCut, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
