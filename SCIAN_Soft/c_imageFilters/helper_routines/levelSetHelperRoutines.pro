; phi = initialize(image, initType = initType)
;
; Initialization of phi
;
; PARAMETERS:
; - image    The reference image
; - initType Initialization function. Accepted values are
;            'distance' initialization with a distance function
;            'circle'   initialization with a peak function
;            'peak'     initialization with a peak function
;            'image'    the input image is used as a binary mask for computation of distance transform
;            Default value is 'distance'
function s_initializeLevelSetFunction, image, initType = initType

  initType = keyword_set(initType) ? initType : 'distance'
  szI = size(image, /dim)
  nx = szI[0]
  ny = szI[1]

  ; MATLAB code... [X Y] = meshgrid(0:ny-1, 0:nx-1)
  ; X, Y replaced with xgrid, ygrid variables
  xGrid = fltArr(nx, nx)
  for k = 0, nx-1 do xGrid[*, k] = findgen(nx)
  yGrid = fltArr(ny, ny)
  for k = 0, ny-1 do yGrid[*, k] = k

  xCenter = floor(nx * 0.5)
  yCenter = floor(ny * 0.5)

  case initType of
              ; distance function with respect to a circle approximately centered at the middle of the image
    'circle'  : begin
                radius = floor((xCenter < yCenter) / 2.2)
                phi = fltArr(nx, ny)
                rasterCircle, image = phi, radius = radius, x0 = xCenter, y0 = yCenter, value = 255, fill = 1
                endcase
    'distance': begin
                phi = -(sqrt((xGrid - xCenter)^2 + (yGrid - yCenter)^2))
                phimin = min(min(phi, dimension = 2)) ; TODO check dimension... transpose phi?
                phimax = max(max(phi, dimension = 2))
                phi = phi + abs(phimin)
                phimin = min(min(phi, dimension = 2))
                endcase
              ; TODO decide about the peak case
    'peak'    : phi = 1. / (sqrt((xGrid - xCenter)^2 + (yGrid - yCenter)^2))
    'image'   : begin
                phi = morph_distance(image, neighbor_sampling = 3)
                phi[where(phi ne 1)] = 0
                endcase
    else      : begin
                print, 'Incorrect option (', initType, '), using <peak> as default'
                phi = 1. / (sqrt((xGrid - xCenter)^2 + (yGrid - yCenter)^2))
                endcase
  endcase
    ; Distance towardw the exterior of the shape
  phiExt = s_expand_mirror(morph_distance(phi, /background, neighbor_sampling = 3), 1, mirrorType = 1)
    ; Distance towards the interior of the shape, reusing the phi variable
  szPhiExt = size(phiExt, /dim)
  phiExt = phiExt[1:szPhiExt[0]-2, 1:szPhiExt[1]-2]
  ;phi = phi ne 0
  phi = morph_distance(phi, neighbor_sampling = 3) - phiExt
  return, phi
end


pro s_initializeLevelSetFunctionTest

  imageFile = dialog_pickfile(FILTER='*.tif')
  image = read_tiff(imageFile)
  xSz = 720
  ySz = 720
  baseTitle = 's_initializeLevelSetFunctionTest - '
  fType = 'image'
  phi = s_initializeLevelSetFunction(image, INITTYPE = fType)
  window, 1, xSize = xSz, ySize = ySz, title = baseTitle + fType
  tvscl, phi

  fType = 'circle'
  phi = s_initializeLevelSetFunction(image, INITTYPE = fType)
  window, 2, xSize = xSz, ySize = ySz, title = baseTitle + fType
  tvscl, phi

  fType = 'distance'
  phi = s_initializeLevelSetFunction(image, INITTYPE = fType)
  window, 3, xSize = xSz, ySize = ySz, title = baseTitle + fType
  tvscl, phi

  fType = 'peak'
  phi = s_initializeLevelSetFunction(image, INITTYPE = fType)
  window, 4, xSize = xSz, ySize = ySz, title = baseTitle + fType
  tvscl, phi

end


; rasterCircle
;
; Midpoint circle algorithm for rasterizing a circle in a given image matrix.
; The algorithm is a variant of Bresenham's line algorithm, and is thus sometimes 
; known as Bresenham's circle algorithm (although not actually invented by Bresenham).
; The algorithm can be generalized to conic sections (See references).
;
; PARAMETERS:
;  - image   the image (2D matrix) for setting pixel values of the circle
;  - x0, y0  the center coordinates of the circle
;  - value   the value to set at the pixels belonging (approximately) to the circle
;  - fFill   a flag that if set can be used to fill the circle interior of the circle
;
; NOTES:
;  Since the algorithm is intended to make a raster circle, the input arguments must 
;  be positive integers.
;
; REFERENCES:
;  [1] Donald Hearn, M. Pauline Baker. Computer graphics. Prentice-Hall. ISBN 9780131615304.
;  [2] http://en.wikipedia.org/wiki/Midpoint_circle_algorithm
;  [3] http://free.pages.at/easyfilter/bresenham.html
pro rasterCircle, image = image, x0 = x0, y0 = y0, radius = radius, value = value, fill = fill

  if keyword_set(fill) then fill = 1
  szImage = size(image, /dim)

  f = 1 - radius
  ddF_x = 1
  ddF_y = -2 * radius
  x = 0
  y = radius
 
  image[x0, y0 + radius] = value
  image[x0, y0 - radius] = value
  image[x0 + radius, y0] = value
  image[x0 - radius, y0] = value

  while (x lt y) do begin
    ; The idea is to have the following...
    ; ddF_x == 2 * x + 1;
    ; ddF_y == -2 * y;
    ; f == x*x + y*y - radius*radius + 2*x - y + 1;
    if (f gt 0) then begin
      y--
      ddF_y += 2
      f += ddF_y
    endif
    x++
    ddF_x += 2
    f += ddF_x
    image[x0 + x, y0 + y] = value
    image[x0 - x, y0 + y] = value
    image[x0 + x, y0 - y] = value
    image[x0 - x, y0 - y] = value
    image[x0 + y, y0 + x] = value
    image[x0 - y, y0 + x] = value
    image[x0 + y, y0 - x] = value
    image[x0 - y, y0 - x] = value
    if (fill eq 1) then begin
      image[x0-x : x0+x, y0 - y] = value
      image[x0-x : x0+x, y0 + y] = value
      image[x0-y : x0+y, y0 - x] = value
      image[x0-y : x0+y, y0 + x] = value
    endif
  endwhile
  if (fill eq 1) then image[x0-radius : x0+radius, y0] = value
end
