
;_____________________________IOISIOI____________________
; NAME:
;      C_sROIObject
;
; PURPOSE:
;       - Object Object
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;     result = obj_new('C_sROIObject' )
;
; METHOHDS:
; function C_sROIObject::init, Name = Name, Number = Number, *pFramePixSize = *pFramePixSize, wherePoints = WherePoints
                           ; Number: Number of Object
                           ; *pFramePixSize: 2D- or 3D-embedding Object-Space
                           ; WherePoints: 2D- or 3D-Object-Coordinates
;_____________________________IOISIOI____________________


pro C_sROIObject::set, pParamStruct = pParamStruct
    *self.pParamStruct = *pParamStruct
end

pro C_sROIObject::setParamAsStruct, paramStruct
    parameterNameList = self->getParameterNameList()
    whParam = (where(parameterNameList[0] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.a
    whParam = (where(parameterNameList[1] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.b
    whParam = (where(parameterNameList[2] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.c
    whParam = (where(parameterNameList[3] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.d
    whParam = (where(parameterNameList[4] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.e
    whParam = (where(parameterNameList[5] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.f
    whParam = (where(parameterNameList[6] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.g
    whParam = (where(parameterNameList[7] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.h
    whParam = (where(parameterNameList[8] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.i
    whParam = (where(parameterNameList[9] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.j
    whParam = (where(parameterNameList[10] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.k
    whParam = (where(parameterNameList[11] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.l
    whParam = (where(parameterNameList[12] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.m
    whParam = (where(parameterNameList[13] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.n
    whParam = (where(parameterNameList[14] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.o
    whParam = (where(parameterNameList[15] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.p
    whParam = (where(parameterNameList[16] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.q
    whParam = (where(parameterNameList[17] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.r
    whParam = (where(parameterNameList[18] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.s
    whParam = (where(parameterNameList[19] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.t
    whParam = (where(parameterNameList[20] eq *(*self.pParamStruct).pNames))[0]
    if (whParam ne -1) then *(*self.pParamStruct).pValues[whParam] = paramStruct.u
end


function C_sROIObject::getpParamStruct
    return, self.pParamStruct
end
function C_sROIObject::getpWherePoints
    return, self.pWherePoints
end
function C_sROIObject::getpPointValues
    return, self.pPointValues
end
function C_sROIObject::getNumber
    return, *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]]
end

function C_sROIObject::getParamAsStruct
    parameterNameList = self->getParameterNameList()
    pArrValues = ptrArr(n_elements(parameterNameList))
    for i = 0, n_elements(parameterNameList)-1 do begin
       whParam = (where(parameterNameList[i] eq *(*self.pParamStruct).pNames))[0]
       if (whParam eq -1) then begin
         pArrValues[i] = ptr_new('NOT CONTAINED')
         print, *pArrValues[i]
       endif else pArrValues[i] = ptr_new(*(*self.pParamStruct).pValues[whParam])
    endfor

    paramStruct = {a: *pArrValues[0],$
          b: *pArrValues[1],$
          c: *pArrValues[2],$
          d: *pArrValues[3],$
          e: *pArrValues[4],$
          f: *pArrValues[5],$
          g: *pArrValues[6],$
          h: *pArrValues[7],$
          i: *pArrValues[8],$
          j: *pArrValues[9],$
          k: *pArrValues[10],$
          l: *pArrValues[11],$
          m: *pArrValues[12],$
          n: *pArrValues[13],$
          o: *pArrValues[14],$
          p: *pArrValues[15],$
          q: *pArrValues[16],$
          r: *pArrValues[17],$
          s: *pArrValues[18],$
          t: *pArrValues[19],$
          u: *pArrValues[20]}

    for i = 0, n_elements(parameterNameList)-1 do ptr_free, pArrValues[i]
    return, paramStruct
end


function C_sROIObject::getxyzPoints
    self->IDLanROI::getProperty, data = xyzPoints
    return, xyzPoints
end


pro C_sROIObject::setIDLgrROIXYZ
    xyzPoints = intArr(3, n_elements(*self.pWherePoints))
    xyzPoints = intArr(2, n_elements(*self.pWherePoints))
    xFramePix = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]]
    xyzPoints[0,*] = *self.pWherePoints mod xFramePix
    xyzPoints[1,*] = floor(*self.pWherePoints/(1.* xFramePix))
    self->IDLanROI::setProperty, data = xyzPoints
end


function C_sROIObject::getCenterXYZ
    self->IDLanROI::getProperty, data = xyzPoints
    if (size(xyzPoints, /n_dim)) then return, xyzPoints[0:1]
    return, [ total(xyzPoints[0,*]), total(xyzPoints[1,*]) ] / (1.*(size(xyzPoints, /dim))[1])
end


pro C_sROIObject::rescalepPointValues, minMaxScales = minMaxScales
    minP = min(*self.pPointValues, max = maxP)
    sclDif = minMaxScales[1] - minMaxScales[0]
    *self.pPointValues = (*self.pPointValues - minP) * (sclDif / ((maxP - minP)>1.)) + minMaxScales[0]
end


function C_sROIObject::makePixelObjectInVoxel,$          ; Returns 2D/3D-Object = 1 in Voxel (2*pixelFrame + dim_xyz)
                       xyzPoints = xyzPoints,$          ; xyzPoints: xyzPoints Pixel Position of Object in Base-Frame
                       PixelFrame = PixelFrame              ; PixelFrame: Frame size in Pixel around Object | Default = 1

    if not(keyWord_set(PixelFrame)) then PixelFrame = 1
    if not(keyWord_set(xyzPoints)) then self->IDLanROI::getProperty, data = xyzPoints
    minXYZPoints0 = min(xyzPoints[0,*], max = maxXYZPoints0)
    minXYZPoints1 = min(xyzPoints[1,*], max = maxXYZPoints1)
    obj = bytArr( maxXYZPoints0 - minXYZPoints0 + (1+2*pixelFrame), maxXYZPoints1 - minXYZPoints1 + (1+2*pixelFrame) )
    obj[xyzPoints[0,*] - (minXYZPoints0 - PixelFrame), xyzPoints[1,*] - (minXYZPoints1 - PixelFrame)] = 1
    return, obj
end


function C_sROIObject::makeObjectBorderInVoxel,$      ; Returns Border Pixels of voxelObject
                       Neighborhood = Neighborhood,$   ; Neighborhood = 4/8 for Object Border Definition | Default = 4
                       voxelObject = voxelObject       ; voxelObject: 2D/3D-Object = 1 in Voxel (PixelFrame + dim_xyz)

    if not(keyWord_set(Neighborhood)) then Neighborhood = 4
    if not(keyWord_set(Neighborhood)) then voxelObject = self->C_sROIObject::makePixelObjectInVoxel(PixelFrame = 1)

    dimVO = size(voxelObject, /dim)
    case n_elements(dimVO) of
        2: begin
              ; Enlarge voxelObject dimension
          enlVO = make_array( dimVO[0]+2, dimVO[1]+2, Type = size(voxelObject, /Type))
              ; Fill center with original picture
          enlVO[ 1:dimVO[0], 1:dimVO[1]] = voxelObject[*,*]
          case Neighborhood of
              4: begin
                   return, (voxelObject ne 0) * $
                           (( enlVO[ 0:dimVO[0]-1, 1:dimVO[1]] ne voxelObject) or $
                            ( enlVO[ 2:dimVO[0]+1, 1:dimVO[1]] ne voxelObject) or $
                            ( enlVO[ 1:dimVO[0], 0:dimVO[1]-1] ne voxelObject) or $
                            ( enlVO[ 1:dimVO[0], 2:dimVO[1]+1] ne voxelObject))
                 endcase
              8: begin
                   return, (voxelObject ne 0) * $
                            (( enlVO[ 0:dimVO[0]-1, 1:dimVO[1]] ne voxelObject) or  $
                            ( enlVO[ 2:dimVO[0]+1, 1:dimVO[1]] ne voxelObject) or $
                            ( enlVO[ 1:dimVO[0], 0:dimVO[1]-1] ne voxelObject) or  $
                            ( enlVO[ 1:dimVO[0], 2:dimVO[1]+1] ne voxelObject) or $
                            ( enlVO[ 0:dimVO[0]-1, 0:dimVO[1]-1] ne voxelObject) or $
                            ( enlVO[ 0:dimVO[0]-1, 2:dimVO[1]+1] ne voxelObject) or $
                            ( enlVO[ 2:dimVO[0]+1, 0:dimVO[1]-1] ne voxelObject) or  $
                            ( enlVO[ 2:dimVO[0]+1, 2:dimVO[1]+1] ne voxelObject))
                 endcase
          endcase
         endcase
       3: begin
              ; Enlarge voxelObject dimension
          enlVO = make_array( dimVO[0]+2, dimVO[1]+2, dimVO[2]+2, Type = size(voxelObject, /Type))
              ; Fill center with original picture
          enlVO[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] = voxelObject[*,*,*]
          case Neighborhood of
          4: begin
                 return, (voxelObject ne 0) * $
                        (( enlVO[ 0:dimVO[0]-1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject))
              endcase
          8: begin
                 return, (voxelObject ne 0) * $
                        (( enlVO[ 0:dimVO[0]-1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject) or $

                         ( enlVO[ 0:dimVO[0]-1, 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 0:dimVO[0]-1, 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $

                         ( enlVO[ 0:dimVO[0]-1, 0:dimVO[1]-1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( enlVO[ 0:dimVO[0]-1, 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject) or $
                         ( enlVO[ 0:dimVO[0]-1, 2:dimVO[1]+1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 0:dimVO[1]-1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 2:dimVO[1]+1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 0:dimVO[1]-1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 2:dimVO[1]+1, 2:dimVO[2]+1] ne voxelObject) or $

                         ( enlVO[ 0:dimVO[0]-1, 0:dimVO[1]-1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 0:dimVO[0]-1, 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 0:dimVO[0]-1, 2:dimVO[1]+1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 0:dimVO[1]-1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 2:dimVO[0]+1, 2:dimVO[1]+1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 0:dimVO[1]-1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( enlVO[ 1:dimVO[0], 2:dimVO[1]+1, 0:dimVO[2]-1] ne voxelObject))
              endcase
         endcase
       endcase
    endcase
end


function C_sROIObject::makeObjectBorderChainCode,$
                       voxelObject = voxelObject        ; voxelObject: 2D/3D-Object = 1 in Voxel (PixelFrame + dim_xyz)
;                                              |3  2  1|
;    Builds chainCodeVector for voxelObject:   |4  X 0|  ; [Xpixel, YPixel, chaincode ......]
;                                              |5  6  7|

    dimVO = size(voxelObject, /dim)

    chainCodeVector = make_array(2*n_elements(voxelObject), /integer, value = -1)

    s = [0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5]
    x = [1, 1, 0, -1,-1, -1, 0, 1]
    y = [0, 1, 1, 1, 0, -1,-1,-1]
    r = [6, 6,0, 0, 2, 2, 4, 4]

    a = (where(voxelObject ne 0))[0]
    chainCodeVector[0:1] = [ a mod dimVO[0], floor(1.0*a/dimVO[0])]    ; Set first Object-Pixel
    x_run = chainCodeVector[0]
    y_run = chainCodeVector[1]

    for j = 3,0,-1 do begin
       if voxelObject[x_run+x[j], y_run+y[j]] then begin
          chainCodeVector[2] = j
          GoDirection = j
          GoRight = r[j]
       endif
    endfor

    if (chainCodeVector[2] eq -1) then GoDirection = -1

    j = 2
    while (chainCodeVector[0] ne x_run) or $
          (chainCodeVector[1] ne y_run) or $
          (chainCodeVector[j>3] ne GoDirection) do begin
       x_run += x[chainCodeVector[j]]
       y_run += y[chainCodeVector[j]]
       a = GoRight + 1
       j += 1
       while (a LT (GoRight+8)) do begin
         if voxelObject[x_run + x[s[a]], y_run + y[s[a]]] then begin
          chainCodeVector[j] = s[a]
          GoRight = r[s[a]]
          a = 20
         endif
         a += 1
       endwhile
    endwhile

    return, [chainCodeVector[Where(chainCodeVector ne -1)], -1]
end


function C_sROIObject::makePolygonFromChainCodeVector, chainCodeVector,$       ; Returns ClosedPolygonxyzPixel
                                            no_closed = no_closed,$
                                            reducedChainCodeVector = reducedChainCodeVector ; Contains ChainCode for polygon

    if ( (size(chainCodeVector, /dim))[0] LE 3) then return, [ transpose( [chainCodeVector[0], chainCodeVector[0]]),$
                                                       transpose( [chainCodeVector[1], chainCodeVector[1]])]
    closedPolygonxPixel = chainCodeVector[0]
    closedPolygonyPixel = chainCodeVector[1]
    xyzStep = [chainCodeVector[0], chainCodeVector[1]]
    StepVector = [[1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1]]
    reducedChainCodeVector = 0

    for i = 2, n_elements(chainCodeVector)-3 do begin
       repeat begin
         xyzStep += StepVector[*, chainCodeVector[i]]
         i += 1
       endrep until (chainCodeVector[i-1] ne chainCodeVector[i])
       i -= 1
       closedPolygonxPixel = [closedPolygonxPixel, xyzStep[0]]
       closedPolygonyPixel = [closedPolygonyPixel, xyzStep[1]]
       reducedChainCodeVector = [reducedChainCodeVector, chainCodeVector[i]]
    endfor

    reducedChainCodeVector = reducedChainCodeVector[1:*]
    if (keyWord_set(no_closed)) then begin
         ; Make PolygonVector that does not close at last position
       polygon = [transpose(closedPolygonxPixel), transpose(closedPolygonyPixel)]
       return, polygon[*,0: (size(polygon, /dim))[1]-2]
    endif else return, [transpose(closedPolygonxPixel), transpose(closedPolygonyPixel)]
end


function C_sROIObject::inflatePolygon, polygon, deflate = deflate

       ; Add 180 degree to the NormalVector in order to shrink Object
    if (keyWord_set(deflate)) then addAngle = 180. else addAngle = 0.

    dim_Polygon = size(polygon, /dim)
    objBorderThickness = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Object Border Thickness'))[0]]

    newPolygonxPixel = 0.
    newPolygonyPixel = 0.

       ;  if only one pixel
    if (polygon[0, 0] eq polygon[0, 1]) and ((polygon[1, 0] eq polygon[1, 1]) ) then begin
       newPolygonxPixel = [newPolygonxPixel,polygon[0, 0] + ([-1.,1., 1.,-1.]*objBorderThickness) ]
       newPolygonyPixel = [newPolygonyPixel, polygon[1, 0] + ([-1.,-1., 1.,1.]*objBorderThickness) ]
    endif else begin

         ;     add last postition as first position
       polygon = [ [polygon[*,dim_Polygon[1]-2]], [polygon] ]

       angleVector = fltArr(dim_Polygon[1])
       for i = 0, dim_Polygon[1]-2 do begin

         ;     calcultate angles between polygons
         angleVector[i] = s_getPolygonAngle( fltArr(2) + polygon[*, i] - polygon[*, i+1],$       ; 1st Vector
                                             fltArr(2) + polygon[*, i+1] - polygon[*, i+2])     ; 2nd Vector

         if (angleVector[i] gt 359.) then begin
          for j = 0, 1 do begin
              newPolygonxyPixel = s_rotVector2D( polygon[*, i+1],$   ; xyVectorFrom (Center of Rotation)
                                               polygon[*, i],$     ; xyVectorTo
                                               ((([135., 225.] + addAngle) mod 360.0001))[j],$
                                               Unit = objBorderThickness)
              newPolygonxPixel = [ newPolygonxPixel, newPolygonxyPixel[0]]
              newPolygonyPixel = [ newPolygonyPixel, newPolygonxyPixel[1]]
          endfor
         endif else begin
          newPolygonxyPixel = s_rotVector2D( polygon[*, i+1],$    ; xyVectorFrom
                                            polygon[*, i],$       ; xyVectorTo (Center of Rotation)
                                            ((angleVector[i]/2.) + addAngle) mod 360.0001,$
                                            Unit = objBorderThickness)
             newPolygonxPixel = [ newPolygonxPixel, newPolygonxyPixel[0]]
             newPolygonyPixel = [ newPolygonyPixel, newPolygonxyPixel[1]]
         endelse
       endfor
    endelse
    newPolygon = ([transpose(newPolygonxPixel), transpose(newPolygonyPixel)]) [*, 1:*]
    return, newPolygon
end


function C_sROIObject::Tesselate2DObject, color = color,$
                                       pPolynomList = pPolynomList   ; PointerList on Polynoms, [0] : BaseObject, [1...n]: Holes
       ; Create a tessellator object.
    oTess = obj_new('IDLgrTessellator')
       ; How many Polynoms ?
    nObjects = size(pPolynomList, /dim)
       ; Pass BaseObject
    oTess->addPolygon, *pPolynomList[0]
       ; Pass Holes
    for i = 1, nObjects[0]-1 do oTess->addPolygon, *pPolynomList[i], /interior
       ; Tesselate everything
    iStatus = oTess->tessellate(fVerts, iConn)
    obj_destroy, oTess

    self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    oPoly = obj_new('IDLgrPolygon',style = 2, color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )

    if (iStatus eq 1) then begin
       oPoly->setProperty, data = fVerts, polygons = iConn
       return, oPoly
    endif else begin
       print,'Unable to tessellate.'
       return, -1
    endelse
end


pro C_sROIObject::addObjectBorderPolygon, color = color, oModel = oModel, modelStyle = modelStyle, borderThick = borderThick, borderLineStyle = borderLineStyle,$
                                          objNumber = objNumber, zPos = zPos

    if (n_elements(modelStyle) le 0) then modelStyle = 1
    if (n_elements(objNumber) le 0) then objNumber = 0
    if not(keyWord_set(color)) then color = [0,255,0]
    pPolynomList = self->getpObjectBorderPolygonList()      ; PointerList on Polynoms, [0] : BaseObject, [1...n]: Holes
    self->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
       ; How many Polynoms ?
    nObjects = size(pPolynomList, /dim)
       ; Pass BaseObject & Holes
    oModel->add, obj_new('IDLgrPolygon', [*pPolynomList[0], transpose(make_array((size(*pPolynomList[0], /dim))[1], /float, value = zPos))],$
                              style = modelStyle, color = color, thick = borderThick, lineStyle = borderLineStyle, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, uValue = {name:'BaseObject', number:objNumber})
    for i = 1, nObjects[0]-1 do $
       oModel->add, obj_new('IDLgrPolygon', [*pPolynomList[i], transpose(make_array((size(*pPolynomList[i], /dim))[1], /float, value = zPos))],$
                              style = modelStyle, color = color, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, uValue = {name:'Hole', number:objNumber})
    ptr_free, pPolynomList
end


function C_sROIObject::getObjectBorderPolygonXX, color = color, pModel = pModel, tesselatedPolygonModel = tesselatedPolygonModel, polygonModel = polygonModel

       ; Get xyzPoints
    self->IDLanROI::getProperty, data = xyzPoints
    case n_elements(*self.pFramePixSize) of
        2: begin
              ; Set Object in small frame
          voxelObject = self->C_sROIObject::makePixelObjectInVoxel(xyzPoints = xyzPoints, PixelFrame = 1)
          dimVO = size(voxelObject, /dim)

              ; Determines number of holes in Object and sets border = 0, main object = 1, and numbers holes = 2, 3, 4, ... n
          labelRegionData = s_2DLabelRegion(voxelObject eq 0, /no_border)
          whereLabelRegionData = Where(labelRegionData ne 0, count)
          if (count ne 0) then labelRegionData[whereLabelRegionData] = temporary(labelRegionData[whereLabelRegionData]) + 1
          labelRegionData = temporary(labelRegionData) + voxelObject
          pPolynomList = ptrArr(max(labelRegionData))

          object_1 = labelRegionData eq 1
          for i = 1, max(labelRegionData) do begin
              if (i eq 1) then object_i = object_1 else object_i = s_rand4(labelRegionData eq i)
              whereObject = Where(object_i eq 1, count)
              xy_pos = intArr(2, count)
              xy_pos[0,*] = whereObject mod dimVO[0]
              xy_pos[1,*] = Floor(whereObject) / (1.*dimVO[0])
              xy_centre = [ total(xy_pos[0,*]), total(xy_pos[1,*]) ] / (1.*count)

                 ; Get chainCodeVector
              chainCodeVector = self->C_sROIObject::makeObjectBorderChainCode(voxelObject = object_i)
                 ; makePolygonFromChainCodeVector
              polygon = self->C_sROIObject::makePolygonFromChainCodeVector(chainCodeVector)

                 ; Inflate the Object Borders, deflate the Holes...
              if (i eq 1) then xy_dif = xyzPoints[0:1] - xy_pos
                 polygon = self->C_sROIObject::inflatePolygon(polygon)
;             endif else polygon = self->C_sROIObject::inflatePolygon(polygon, /deflate)

                 ; Scale to original Position and Add to   pPolynomList
              polygon[0,*] = temporary(polygon[0,*]) + xy_dif[0]
              polygon[1,*] = temporary(polygon[1,*]) + xy_dif[1]
              pPolynomList[i-1] = ptr_new(polygon, /no_copy)
          endfor

          if keyWord_set(pModel) then begin
             *pModel = self->C_sROIObject::Tesselate2DObject(pPolynomList = pPolynomList, color = color)
             for i = 0, n_elements(pPolynomList)-1 do ptr_free, pPolynomList[i]
             ptr_free, pPolynomList
             return, 1
          endif
          if keyWord_set(tesselatedPolygonModel) then begin
             oModel = self->C_sROIObject::Tesselate2DObject(pPolynomList = pPolynomList, color = color)
             for i = 0, n_elements(pPolynomList)-1 do ptr_free, pPolynomList[i]
             ptr_free, pPolynomList
             return, oModel
          endif
          if keyWord_set(polygonModel) then begin
             oModel = self->C_sROIObject::getoPolygonBorderModel(pPolynomList = pPolynomList, color = color)
             for i = 0, n_elements(pPolynomList)-1 do ptr_free, pPolynomList[i]
             ptr_free, pPolynomList
             return, oModel
          endif

;          a = s_chain_code_viceversa(chainCodeVector,dimVO[0], dimVO[1])
;          window, 10
;          tv, bytArr(400,400)+255
;          tvscl, congrid(voxelObject, 200,200)
;          window, 11
;          tv, bytArr(400,400)+255
;          tvscl, congrid(a, 200,200)
;          window, 12
;          tv, bytArr(400,400)+255
;          tvscl, congrid(labelRegionData, 200,200)

         endcase
       3: begin
         endcase
    endcase
    return, oModel
end


function C_sROIObject::getpObjectBorderPolygonList; Return PointerList on Polynoms, [0] : BaseObject, [1...n]: Holes

    self->IDLanROI::getProperty, data = xyzPoints
       ; Set Object in small frame
    voxelObject = self->C_sROIObject::makePixelObjectInVoxel(xyzPoints = xyzPoints, PixelFrame = 1)
    dimVO = size(voxelObject, /dim)

    ; Determines number of holes in Object and sets border = 0, main object = 1, and numbers holes = 2, 3, 4, ... n
    labelRegionData = s_2DLabelObjectAndHoles(voxelObject)
    pPolynomList = ptrArr(max(labelRegionData))

    whereObject = (where(labelRegionData eq 1))[0]
    xy_pos = [whereObject mod dimVO[0], floor(whereObject / (1.*dimVO[0]))]
    xy_dif = xyzPoints[0:1] - xy_pos

    for i = 1, max(labelRegionData) do begin
         ; get ObjectBorder
       if (i eq 1) then object_i = s_rand4(labelRegionData gt 0) else object_i = s_rand4(labelRegionData eq i)
         ; get chainCodeVector
       chainCodeVector = self->C_sROIObject::makeObjectBorderChainCode(voxelObject = object_i)
         ; makePolygonFromChainCodeVector
       polygon = self->makePolygonFromChainCodeVector(chainCodeVector)
         ; inflate the Object Borders, deflate the Holes...
       polygon = self->inflatePolygon(polygon)
         ; scale to original Position and Add to  pPolynomList
       polygon[0,*] += xy_dif[0]
       polygon[1,*] += xy_dif[1]
       pPolynomList[i-1] = ptr_new(polygon, /no_copy)
    endfor
    return, pPolynomList
end


pro C_sROIObject::setIDLgrROIXYZCoord
    xyzFramePix = 1.* [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]],$
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]],$
                        *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Total Number of z-Slices'))[0]] ]

    xyzFract = [ *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Size per Pixel [x]'))[0]],$
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Size per Pixel [y]'))[0]],$
                 *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Interval [real]'))[0]] ]

    if (xyzFract[2] le 0) then xyzFract = [xyzFract[0:1] / (min(xyzFract[0:1]) * max(xyzFramePix[0:1])), 0.] $
       else xyzFract = xyzFract / (min(xyzFract) * max(xyzFramePix))

    if (xyzFramePix[0] gt xyzFramePix[1]) then xyFact = [1., xyzFramePix[1] / xyzFramePix[0]] else xyFact = [xyzFramePix[0] / xyzFramePix[1], 1.] 

    self->setProperty, xCoord_conv = [-0.5*xyFact[0], xyzFract[0]], yCoord_conv = [-0.5*xyFact[1], xyzFract[1]], zCoord_conv = [0., xyzFract[2]]
end


pro C_sROIObject::cleanup
   if ptr_valid(self.pParamStruct) then begin
      ptr_free, (*self.pParamStruct).pNames
      for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
      ptr_free, self.pParamStruct
   endif
   if ptr_valid(self.pWherePoints) then ptr_free, self.pWherePoints
   if ptr_valid(self.pPointValues) then ptr_free, self.pPointValues
   self->IDLgrROI::cleanup
end


function C_sROIObject::getParameterNameList
    return, ['Segmentation Phase',$
             'Number',$
             'ROI-Group ShowBox xSize [pixel]',$
             'ROI-Group ShowBox ySize [pixel]',$
             'Total Number of z-Slices',$
             'Size per Pixel [x]',$
             'Size per Pixel [y]',$
              'z-Interval [real]',$
             'Object Border Thickness',$
             'Border Color_r',$
             'Border Color_g',$
             'Border Color_b',$
             'Border Thick',$
             'Border LineStyle',$
             'ROI Object',$
             'ROI Color_r',$
             'ROI Color_g',$
             'ROI Color_b',$
             'ROI Style',$
             'ROI Thick',$
             'ROI LineStyle',$
             'ROI Hide']
end


function C_sROIObject::init, segPhase = segPhase,$
                             number = number,$   ;Object Number
                             framePixSize = framePixSize,$      ; 2D- or 3D-embedding Object-Space
                             xyzSizePerPixel = xyzSizePerPixel,$
                             objBorderThickness = objBorderThickness,$ ; Frame Units for Object Borders
                             borderColor = borderColor,$
                             borderThick = borderThick,$
                             borderLineStyle = borderLineStyle,$
                             ROIColor = ROIColor,$
                             ROIStyle = ROIStyle,$
                             ROIThick = ROIThick,$
                             ROILineStyle = ROILineStyle,$
                             ROIHide = ROIHide,$
                             pointValues = pointValues,$   ; Point-Values
                             wherePoints = wherePoints   ;2 D- or 3D-Object-Coordinates

    parameterNameList = self->getParameterNameList()
    paramStruct = {ROIParamStruct,$
                   pValues:ptrArr(n_elements(parameterNameList), /allocate),$       ; Pointer on Parameter Values.
                   pNames:ptr_new(parameterNameList)}     ; Pointer on Parameter Names.

    if (n_elements(segPhase) eq 0) then segPhase = 'Phase'
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Segmentation Phase'))[0]] = segPhase
    if (n_elements(number) eq 0) then number = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Number'))[0]] = number
    if (n_elements(framePixSize) eq 0) then framePixSize = [-1,-1,-1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]] = framePixSize[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]] = framePixSize[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Total Number of z-Slices'))[0]] = framePixSize[2]
    if (n_elements(xyzSizePerPixel) eq 0) then xyzSizePerPixel = framePixSize
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Size per Pixel [x]'))[0]] = xyzSizePerPixel[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Size per Pixel [y]'))[0]] = xyzSizePerPixel[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Interval [real]'))[0]] = xyzSizePerPixel[2]
    if (n_elements(objBorderThickness) eq 0) then objBorderThickness = .5
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Object Border Thickness'))[0]] = objBorderThickness
    if (n_elements(borderColor) eq 0) then borderColor = [0,255,0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_r'))[0]] = borderColor[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_g'))[0]] = borderColor[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_b'))[0]] = borderColor[2]
    if (n_elements(borderThick) eq 0) then borderThick = 1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Thick'))[0]] = borderThick
    if (n_elements(borderLineStyle) eq 0) then borderLineStyle = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border LineStyle'))[0]] = borderLineStyle

    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Object'))[0]] = 'CONTAINED'
    if (n_elements(ROIColor) eq 0) then ROIColor = [255,0,0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Color_r'))[0]] = ROIColor[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Color_g'))[0]] = ROIColor[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Color_b'))[0]] = ROIColor[2]
    if (n_elements(ROIStyle) eq 0) then ROIStyle = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Style'))[0]] = ROIStyle
    if (n_elements(ROIThick) eq 0) then ROIThick = 1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Thick'))[0]] = ROIThick
    if (n_elements(ROILineStyle) eq 0) then ROILineStyle = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI LineStyle'))[0]] = ROILineStyle
    if (n_elements(ROIHide) eq 0) then ROIHide = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'ROI Hide'))[0]] = ROIHide

    self.pParamStruct = ptr_new(paramStruct, /no_copy)

    if (n_elements(wherePoints) ne 0) then self.pWherePoints = ptr_new(wherePoints, /no_copy)
    if (n_elements(pointValues) ne 0) then self.pPointValues = ptr_new(pointValues, /no_copy)

    dummy = self->IDLgrROI::init(make_array(2,3, /integer),$
                                 name = strCompress(*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]], /remove),$
                                 color = ROIColor,$
                                 style = ROIStyle,$
                                 thick = ROIThick,$
                                 lineStyle = ROILineStyle,$
                                 hide = ROIHide, /double)
    self->setIDLgrROIXYZ
    self->setIDLgrROIXYZCoord
    return, 1
end


pro C_sROIObject__define
   tmp = {C_sROIObject, pParamStruct:ptr_new(),$
                        pWherePoints:ptr_new(),$
                        pPointValues:ptr_new(),$
                        inherits IDLgrROI}
end
