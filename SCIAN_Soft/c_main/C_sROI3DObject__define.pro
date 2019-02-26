
;_____________________________IOISIOI____________________
; NAME:
;      C_sROI3DObject
;
; PURPOSE:
;       - Object Object
;
; AUTHor:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROI3DObject' )
;
; METHOHDS:
;function C_sROI3DObject::init, Name = Name, Number = Number, *pFramePixSize = *pFramePixSize, wherePoints = WherePoints
                           ; Number: Number of Object
                           ; *pFramePixSize: 2D- or 3D-embedding Object-Space
                           ; WherePoints: 2D- or 3D-Object-Coordinates
;_____________________________IOISIOI____________________

function C_sROI3DObject::getModelContainer
    return, self.oModelContainer
end

pro C_sROI3DObject::set, pParamStruct = pParamStruct
    *self.pParamStruct = *pParamStruct
end

pro C_sROI3DObject::setParamsFromModels
    if not(obj_valid(self.oModelContainer)) then return
    for i = 0, self.oModelContainer->count()-1 do begin
       oModel = self.oModelContainer->get(position = i)
       objParams = oModel->getModelParamsForObj()
    endfor
end

function C_sROI3DObject::getxyzPoints
    self->IDLanROI::getProperty, data = xyzPoints
    return, xyzPoints
end

function C_sROI3DObject::getxyzDim
   return, [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]]]
end

function C_sROI3DObject::getxyzRealSize
   return, [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [real]'))[0]]]
end


function C_sROI3DObject::getxyzSizePerPixel
   return, [*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]] / *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]] / *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]],$
            *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [real]'))[0]] / *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]]]
end


pro C_sROI3DObject::setIDLgrROIXYZData

   xyzDim = 1l * self->getxyzDim()

   nPoints = n_elements(*self.pWherePoints)
   xyzPoints = intArr(3, nPoints)
   xyzPoints[0,*] = *self.pWherePoints mod xyzDim[0]
   xyzPoints[1,*] = floor( (*self.pWherePoints mod (xyzDim[0] * xyzDim[1])) / (1.*xyzDim[0]))
   xyzPoints[2,*] = floor( *self.pWherePoints / (1. * xyzDim[0] * xyzDim[1]))

   if (nPoints eq 1) then (*self.pEigenSys).centerXYZ = xyzPoints $
      else (*self.pEigenSys).centerXYZ = [total(xyzPoints[0,*]), total(xyzPoints[1,*]), total(xyzPoints[2,*])] / (1.* nPoints)

   if (((*self.pEigenSys).sizePerXYZ)[2] le 0) then xyzFract = [(*self.pEigenSys).sizePerXYZ[0:1] / (min((*self.pEigenSys).sizePerXYZ[0:1]) * max(xyzDim[0:1])), 0.] $
      else xyzFract = (*self.pEigenSys).sizePerXYZ / (min((*self.pEigenSys).sizePerXYZ) * max(xyzDim))

   if (xyzDim[0] gt xyzDim[1]) then xyFact = [1., 1. * xyzDim[1] / xyzDim[0]] else xyFact = [1. * xyzDim[0] / xyzDim[1], 1.] 

   self->setProperty, data = xyzPoints, xCoord_conv = [-.5*xyFact[0], xyzFract[0] ], yCoord_conv = [-.5*xyFact[1], xyzFract[1] ], zCoord_conv = [.0, xyzFract[2] ]
end


pro C_sROI3DObject::setParamAsStruct, paramStruct
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]] = paramStruct.a
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]] = paramStruct.b
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]] = paramStruct.c
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Position'))[0]] = paramStruct.d
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Initial'))[0]] = paramStruct.e
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]] = paramStruct.f
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]] = paramStruct.g
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [real]'))[0]] = paramStruct.h
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]] = paramStruct.i
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]] = paramStruct.j
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]] = paramStruct.k
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thickness'))[0]] = paramStruct.l
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Object'))[0]] = paramStruct.m
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Style'))[0]] = paramStruct.n
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]] = paramStruct.o
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]] = paramStruct.p
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]] = paramStruct.q
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]] = paramStruct.r
    *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]] = paramStruct.s
end


function C_sROI3DObject::getpParamStruct
    return, self.pParamStruct
end
function C_sROI3DObject::getpWherePoints
    return, self.pWherePoints
end
pro C_sROI3DObject::setWherePoints, wherePoints
    *self.pWherePoints = wherePoints
    self->setIDLgrROIXYZData
end
function C_sROI3DObject::getpPointValues
    return, self.pPointValues
end
pro C_sROI3DObject::setPointValues, pointValues
    *self.pPointValues = pointValues
end
function C_sROI3DObject::getNumber
    return, *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]]
end

function C_sROI3DObject::getParamAsStruct
    return, {a: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]],$
             b: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Time Position'))[0]] ,$
             c: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Channel Position'))[0]] ,$
             d: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Position'))[0]] ,$
             e: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Slice Initial'))[0]] ,$
             f: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [real]'))[0]] ,$
             g: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [real]'))[0]] ,$
             h: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [real]'))[0]] ,$
             i: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'x-Size [pixel]'))[0]] ,$
             j: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'y-Size [pixel]'))[0]] ,$
             k: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'z-Size [pixel]'))[0]] ,$
             l: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thickness'))[0]],$
             m: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Object'))[0]] ,$
             n: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Style'))[0]],$
             o: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_r'))[0]],$
             p: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_g'))[0]],$
             q: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Color_b'))[0]],$
             r: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thick'))[0]],$
             s: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border LineStyle'))[0]]}
end

function C_sROI3DObject::getCenterXYZ
   return, (*self.pEigenSys).centerXYZ
end

function C_sROI3DObject::getpEigenSys
   return, self.pEigenSys
end


function C_sROI3DObject::calcEigenSys, setEigenSys = setEigenSys, xyzPoints = xyzPoints, centerXYZ = centerXYZ, sizePerXYZ = sizePerXYZ
   ; In mechanics, the eigenvectors of the inertia tensor T define the principal axes of a rigid body.
   ; The tensor of inertia is required in order to determine the rotation of a rigid body around its center of mass.
   ; Here we first calculate center moments (MYijk) in order to define the inertia tensor T.
   ; T can also be understood as the covariance matrix.

   ; |T200 T110 T101|
   ; |T110 T020 T011| / M000
   ; |T101 T011 T002|

   if (n_elements(xyzPoints) le 0) then self->getProperty, data = xyzPoints
   if (n_elements(xyzPoints) le 0) then xyzPoints = MAKE_ARRAY(3,2, /DOUBLE, value = 0);    if xyzpoints is empty.. this function fails...   
   if (n_elements(setEigenSys) le 0) then setEigenSys = 0b else setEigenSys = 1b
   dimXYZ = size(xyzPoints, /dim)
   if (n_elements(dimXYZ) eq 1) then dimXYZ = [dimXYZ, 1.]
   MYxyz = make_array(dimXYZ, /double)

   case setEigenSys of
      1: begin
         MYxyz[0,*] = (xyzPoints[0,*] - ((*self.pEigenSys).centerXYZ)[0]) * ((*self.pEigenSys).sizePerXYZ)[0]
         MYxyz[1,*] = (xyzPoints[1,*] - ((*self.pEigenSys).centerXYZ)[1]) * ((*self.pEigenSys).sizePerXYZ)[1]
         MYxyz[2,*] = (xyzPoints[2,*] - ((*self.pEigenSys).centerXYZ)[2]) * ((*self.pEigenSys).sizePerXYZ)[2]

         (*self.pEigenSys).inertiaT[0:2,0] = [ total(MYxyz[0,*]*MYxyz[0,*]),      total(MYxyz[0,*]*MYxyz[1,*]),      total(MYxyz[0,*]*MYxyz[2,*]) ]
         (*self.pEigenSys).inertiaT[0:2,1] = [ ((*self.pEigenSys).inertiaT)[1,0], total(MYxyz[1,*]*MYxyz[1,*]),      total(MYxyz[1,*]*MYxyz[2,*]) ]
         (*self.pEigenSys).inertiaT[0:2,2] = [ ((*self.pEigenSys).inertiaT)[2,0], ((*self.pEigenSys).inertiaT)[2,1], total(MYxyz[2,*]*MYxyz[2,*]) ]
         (*self.pEigenSys).inertiaT /= dimXYZ[1]

           ; now look for the diagonal matrix T' = R^-1 T R with R^-1 = tranpose(R)
           ; the ith row of eigenVect contains the ith eigenvalue
         (*self.pEigenSys).eigenVals = eigenQL((*self.pEigenSys).inertiaT, /double, eigenvectors = eigenVect, residual = var)
           ; the ith column of eigenVect contains the ith eigenvalue
         (*self.pEigenSys).eigenVect = transpose(eigenVect)

        ; |EV11 EV21 EV31|
        ; |EV12 EV22 EV32| , eigenVectors
        ; |EV13 EV23 EV33|

          ; the T -> T' contains mirroring if det|EV| = -1
          ; changing sign of one Ev prevents mirroring, det|EV'| = 1
         if (determ((*self.pEigenSys).eigenVect) eq -1) then (*self.pEigenSys).eigenVect = [-((*self.pEigenSys).eigenVect)[0,*], ((*self.pEigenSys).eigenVect)[1,*], ((*self.pEigenSys).eigenVect)[2,*]]

         return, 1b
      endcase
   else: begin
         if (n_elements(centerXYZ) le 0) then begin
            case (dimXYZ[1] eq 1) of
            1: centerXYZ = xyzPoints
            else: centerXYZ = [total(xyzPoints[0,*]), total(xyzPoints[1,*]), total(xyzPoints[2,*])] / (1.*(size(xyzPoints, /dim))[1])
            endcase
         endif
         if (n_elements(sizePerXYZ) le 0) then sizePerXYZ = (*self.pEigenSys).sizePerXYZ

         MYxyz[0,*] = (xyzPoints[0,*] - centerXYZ[0]) * sizePerXYZ[0]
         MYxyz[1,*] = (xyzPoints[1,*] - centerXYZ[1]) * sizePerXYZ[1]
         MYxyz[2,*] = (xyzPoints[2,*] - centerXYZ[2]) * sizePerXYZ[2]

         inertiaT = make_array(3,3, /double)
         inertiaT[0:2,0] = [ total(MYxyz[0,*]*MYxyz[0,*]), total(MYxyz[0,*]*MYxyz[1,*]), total(MYxyz[0,*]*MYxyz[2,*]) ]
         inertiaT[0:2,1] = [ inertiaT[1,0],                total(MYxyz[1,*]*MYxyz[1,*]), total(MYxyz[1,*]*MYxyz[2,*]) ]
         inertiaT[0:2,2] = [ inertiaT[2,0],                inertiaT[2,1],                total(MYxyz[2,*]*MYxyz[2,*]) ]
         inertiaT /= dimXYZ[1]

         eigenVals = eigenQL(inertiaT, /double, eigenvectors = eigenVect, residual = var)
         eigenVect = transpose(eigenVect)
         if (determ(eigenVect) eq -1) then eigenVect = [-(eigenVect)[0,*], (eigenVect)[1,*], (eigenVect)[2,*]]

         return, {eigenVect:eigenVect, eigenVals:eigenVals}
      endcase
   endcase
end


pro C_sROI3DObject::rotateEVS

   xyzDim = self->getxyzDim()
   self->getProperty, data = xyzPoints
   dimXYZ = size(xyzPoints, /dim)

   MYxyz = make_array(dimXYZ, /double)
   MYxyz[0,*] = (xyzPoints[0,*] - ((*self.pEigenSys).centerXYZ)[0]) * ((*self.pEigenSys).sizePerXYZ)[0]
   MYxyz[1,*] = (xyzPoints[1,*] - ((*self.pEigenSys).centerXYZ)[1]) * ((*self.pEigenSys).sizePerXYZ)[1]
   MYxyz[2,*] = (xyzPoints[2,*] - ((*self.pEigenSys).centerXYZ)[2]) * ((*self.pEigenSys).sizePerXYZ)[2]

    ; rotate elongated axis (smallest EV) parallel to z
    ; rotate 2nd elongated axis (2nd smallest EV) parallel to y
    ; rotate less elongated axis (biggest EV) parallel to x
    ; sort Eigenvectors in respect to size of Eigenvalues
   sortEV = sort((*self.pEigenSys).eigenVals)

; |cos -sin 0|
; |sin  cos 0| :   Rz(alpha)
; | 0    0  1|
   alpha = acos(total([((*self.pEigenSys).eigenVect)[*,sortEV[2]]] * [1.,0.,0.]))
   rot3D = make_array(3,3, /double)
   rot3D[0:2,0] = [cos(alpha), -sin(alpha), 0.]
   rot3D[0:2,1] = [sin(alpha),  cos(alpha), 0.]
   rot3D[0:2,2] = [        0.,          0., 1.]
   for i = 0,dimXYZ[1]-1 do MYxyz[*,i] = rot3D # MYxyz[*,i]


; |cos  0 sin|
; |0    1 0  | :   Ry(alpha)
; |-sin 0 cos|
   alpha = acos(total([((*self.pEigenSys).eigenVect)[*,sortEV[1]]] * [0.,1.,0.]))
   rot3D = make_array(3,3, /double)
   rot3D[0:2,0] = [ cos(alpha), 0., sin(alpha)]
   rot3D[0:2,1] = [         0., 1.,         0.]
   rot3D[0:2,2] = [-sin(alpha), 0., cos(alpha)]
   for i = 0,dimXYZ[1]-1 do MYxyz[*,i] = rot3D # MYxyz[*,i]

; |1  0    0 |
; |0 cos -sin| :   Rx(alpha)
; |0 sin  cos|
   alpha = acos(total([((*self.pEigenSys).eigenVect)[*,sortEV[0]]] * [0.,0.,1.]))
   rot3D = make_array(3,3, /double)
   rot3D[0:2,0] = [1.,         0.,          0.]
   rot3D[0:2,1] = [0., cos(alpha), -sin(alpha)]
   rot3D[0:2,2] = [0., sin(alpha),  cos(alpha)]
   for i = 0,dimXYZ[1]-1 do MYxyz[*,i] = rot3D # MYxyz[*,i]

   xyzPoints[0,*] = MYxyz[0,*] / ((*self.pEigenSys).sizePerXYZ)[0] + ((*self.pEigenSys).centerXYZ)[0]
   xyzPoints[1,*] = MYxyz[1,*] / ((*self.pEigenSys).sizePerXYZ)[1] + ((*self.pEigenSys).centerXYZ)[1]
   xyzPoints[2,*] = MYxyz[2,*] / ((*self.pEigenSys).sizePerXYZ)[2] + ((*self.pEigenSys).centerXYZ)[2]
   self->setProperty, data = xyzPoints

   *self.pWherePoints = round(xyzPoints[0,*]) + round(xyzPoints[1,*]) * xyzDim[0] + round(xyzPoints[2,*]) * (1. * xyzDim[0] * xyzDim[1])
   (*self.pEigenSys).fOK = self->calcEigenSys(/setEigenSys)
end


function C_sROI3DObject::transformEVSintoXYZ, xyzPoints = xyzPoints, sDirection = sDirection

   if (n_elements(sDirection) eq 0) then sDirection = 'calcGalleryPosition'
   if (n_elements(xyzPoints) le 0) then begin
      self->getProperty, data = xyzPoints
      fSetData = 1b
   endif else fSetData = 0b

   dimXYZ = size(xyzPoints, /dim)
   if (n_elements(dimXYZ) eq 1) then dimXYZ = [dimXYZ, 1.]

   MYxyz = make_array(dimXYZ, /double)
   MYxyz[0,*] = (xyzPoints[0,*] - ((*self.pEigenSys).centerXYZ)[0]) * ((*self.pEigenSys).sizePerXYZ)[0]
   MYxyz[1,*] = (xyzPoints[1,*] - ((*self.pEigenSys).centerXYZ)[1]) * ((*self.pEigenSys).sizePerXYZ)[1]
   MYxyz[2,*] = (xyzPoints[2,*] - ((*self.pEigenSys).centerXYZ)[2]) * ((*self.pEigenSys).sizePerXYZ)[2]

   case 1 of
         ; XYZ' = EV XYZ, rotate EVS and object into XYZ
      sDirection eq 'calcGalleryPosition':for i = 0l,dimXYZ[1]-1 do MYxyz[*,i] = (*self.pEigenSys).eigenVect # MYxyz[*,i]
         ; XYZ = EV-1 XYZ', rotate back
      sDirection eq 'calcOriginalPosition':for i = 0l,dimXYZ[1]-1 do MYxyz[*,i] = invert((*self.pEigenSys).eigenVect, /double) # MYxyz[*,i]
   endcase

   xyzPoints[0,*] = MYxyz[0,*] / ((*self.pEigenSys).sizePerXYZ)[0] + ((*self.pEigenSys).centerXYZ)[0]
   xyzPoints[1,*] = MYxyz[1,*] / ((*self.pEigenSys).sizePerXYZ)[1] + ((*self.pEigenSys).centerXYZ)[1]
   xyzPoints[2,*] = MYxyz[2,*] / ((*self.pEigenSys).sizePerXYZ)[2] + ((*self.pEigenSys).centerXYZ)[2]

   if fSetData then begin
      self->IDLanROI::setProperty, data = xyzPoints
      return, 1b
   endif else return,xyzPoints
end


function C_sROI3DObject::getEVSProj, xyzPoints = xyzPoints, fCalcBottleNeck = fCalcBottleNeck, fGetEv1Proj = fGetEv1Proj, fKeepXYZPoints = fKeepXYZPoints

   ; function returns evsProj: xyzPoints projected onto the three pricipal axis
   ;                  sqProj: 8 vertexes of the box enclosing xyzPoints
   ;                  bottleAxisRatio: axis lenth ratios of principal axis for bottle neck quantification
   ;                  bottleMassRatio: mass ratios of principal axis for bottle neck quantification
   
   if (n_elements(xyzPoints) le 0) then self->getProperty, data = xyzPoints
   if (n_elements(fKeepXYZPoints) le 0) then fKeepXYZPoints = 0b else fKeepXYZPoints = 1b
   if (n_elements(fCalcBottleNeck) le 0) then fCalcBottleNeck = 0b else fCalcBottleNeck = 1b
   if (n_elements(fGetEv1Proj) le 0) then fGetEv1Proj = 0b else fGetEv1Proj = 1b
   xyzSizePerPixel = self->getxyzSizePerPixel()

      ; set xyzPoints relative to center of mass
   xyzPoints[0,*] -= ((*self.pEigenSys).centerXYZ)[0]
   xyzPoints[1,*] -= ((*self.pEigenSys).centerXYZ)[1]
   xyzPoints[2,*] -= ((*self.pEigenSys).centerXYZ)[2]

      ; transform xyzPoints to real coordinate system
   xyzPoints[0,*] *= xyzSizePerPixel[0]
   xyzPoints[1,*] *= xyzSizePerPixel[1]
   xyzPoints[2,*] *= xyzSizePerPixel[2]

   dimxyzPoints = size(xyzPoints, /dim)
   evProj = make_array(dimxyzPoints, /float)
   evsProj = make_array([3, dimxyzPoints], /float)

      ; project xyzPoints to the EVS
   evProj[0,*] = xyzPoints[0,*] * ((*self.pEigenSys).eigenVect)[0,0] + xyzPoints[1,*] * ((*self.pEigenSys).eigenVect)[0,1] + xyzPoints[2,*] * ((*self.pEigenSys).eigenVect)[0,2]
   evProj[1,*] = xyzPoints[0,*] * ((*self.pEigenSys).eigenVect)[1,0] + xyzPoints[1,*] * ((*self.pEigenSys).eigenVect)[1,1] + xyzPoints[2,*] * ((*self.pEigenSys).eigenVect)[1,2]
   evProj[2,*] = xyzPoints[0,*] * ((*self.pEigenSys).eigenVect)[2,0] + xyzPoints[1,*] * ((*self.pEigenSys).eigenVect)[2,1] + xyzPoints[2,*] * ((*self.pEigenSys).eigenVect)[2,2]

   if fKeepXYZPoints then begin
         ; transform xyzPoints to pixel coordinate system
      xyzPoints[0,*] /= xyzSizePerPixel[0]
      xyzPoints[1,*] /= xyzSizePerPixel[1]
      xyzPoints[2,*] /= xyzSizePerPixel[2]   
         ; set xyzPoints independent of center of mass
      xyzPoints[0,*] += ((*self.pEigenSys).centerXYZ)[0]
      xyzPoints[1,*] += ((*self.pEigenSys).centerXYZ)[1]
      xyzPoints[2,*] += ((*self.pEigenSys).centerXYZ)[2]
   endif else xyzPoints = 0b ; free some memory

   minEVProj0 = min(evProj[0,*], max = maxEVProj0)
   minEVProj1 = min(evProj[1,*], max = maxEVProj1)
   minEVProj2 = min(evProj[2,*], max = maxEVProj2)

      ; calculate bottleness parameters bottleAxisRatio and bottleMassRatio
   if fCalcBottleNeck then begin
      boxCenter = [(minEVProj0 + maxEVProj0) / 2., (minEVProj1 + maxEVProj1) / 2. , (minEVProj2 + maxEVProj2) / 2.]      
      wh = where(evProj[0,*] ge boxCenter[0], nPosG0)
      wh = where(evProj[0,*] le boxCenter[0], nPosL0)
      wh = where(evProj[1,*] ge boxCenter[1], nPosG1)
      wh = where(evProj[1,*] le boxCenter[1], nPosL1)
      wh = where(evProj[2,*] ge boxCenter[2], nPosG2)
      wh = where(evProj[2,*] le boxCenter[2], nPosL2)
      bottleAxisRatio = [(abs(minEVProj0) < maxEVProj0) / (abs(minEVProj0) > maxEVProj0), (abs(minEVProj1) < maxEVProj1) / (abs(minEVProj1) > maxEVProj1), (abs(minEVProj2) < maxEVProj2) / (abs(minEVProj2) > maxEVProj2)]
      bottleMassRatio = [1.*nPosG0/nPosL0, 1.*nPosG1/nPosL1, 1.*nPosG2/nPosL2]

      boxCenter = [(minEVProj0 + maxEVProj0) * (2./3), (minEVProj1 + maxEVProj1) * (2./3), (minEVProj2 + maxEVProj2) * (2./3)]      
      wh = where(evProj[0,*] ge boxCenter[0], nPosG0)
      wh = where(evProj[1,*] ge boxCenter[1], nPosG1)
      wh = where(evProj[2,*] ge boxCenter[2], nPosG2)
      boxCenter = [(minEVProj0 + maxEVProj0) * (1./3), (minEVProj1 + maxEVProj1) * (1./3), (minEVProj2 + maxEVProj2) * (1./3)]      
      wh = where(evProj[0,*] le boxCenter[0], nPosL0)
      wh = where(evProj[1,*] le boxCenter[1], nPosL1)
      wh = where(evProj[2,*] le boxCenter[2], nPosL2)
      wh = 0
      bottleMassRatio_3 = [1.*nPosG0/nPosL0, 1.*nPosG1/nPosL1, 1.*nPosG2/nPosL2]
      print, "bottleMass Pos: ", [1.*nPosG0, 1.*nPosG1, 1.*nPosG2]
      print, "bottleMass Neg: ", [1.*nPosL0, 1.*nPosL1, 1.*nPosL2]
      print, "bottleMassRatio in Obj: ", bottleMassRatio
   endif else begin
      bottleAxisRatio = [-1,-1,-1]
      bottleMassRatio = [-1,-1,-1]
      bottleMassRatio_3 = [-1,-1,-1]
   endelse
       ; get square coordinates
   sqsProj = make_array([3,8], /float)
   sqsProj[*,0] = [minEVProj0, minEVProj1, minEVProj2]
   sqsProj[*,1] = [maxEVProj0, minEVProj1, minEVProj2]
   sqsProj[*,2] = [maxEVProj0, maxEVProj1, minEVProj2]
   sqsProj[*,3] = [minEVProj0, maxEVProj1, minEVProj2]
   sqsProj[*,4] = [minEVProj0, minEVProj1, maxEVProj2]
   sqsProj[*,5] = [maxEVProj0, minEVProj1, maxEVProj2]
   sqsProj[*,6] = [maxEVProj0, maxEVProj1, maxEVProj2]
   sqsProj[*,7] = [minEVProj0, maxEVProj1, maxEVProj2]

       ; get evsMinMax coordinates
   evsMinMax = make_array([3,6], /float)
   evsMinMax[*,0] = [minEVProj0, 0, 0]
   evsMinMax[*,1] = [maxEVProj0, 0, 0]
   evsMinMax[*,2] = [0, minEVProj1, 0]
   evsMinMax[*,3] = [0, maxEVProj1, 0]
   evsMinMax[*,4] = [0, 0, minEVProj2]
   evsMinMax[*,5] = [0, 0, maxEVProj2]

      ; transform into real coordinate system
   sqProj = make_array([3,8], /float)
   sqProj[0,*] = sqsProj[0,*] * ((*self.pEigenSys).eigenVect)[0,0] + sqsProj[1,*] * ((*self.pEigenSys).eigenVect)[1,0] + sqsProj[2,*] * ((*self.pEigenSys).eigenVect)[2,0]
   sqProj[1,*] = sqsProj[0,*] * ((*self.pEigenSys).eigenVect)[0,1] + sqsProj[1,*] * ((*self.pEigenSys).eigenVect)[1,1] + sqsProj[2,*] * ((*self.pEigenSys).eigenVect)[2,1]
   sqProj[2,*] = sqsProj[0,*] * ((*self.pEigenSys).eigenVect)[0,2] + sqsProj[1,*] * ((*self.pEigenSys).eigenVect)[1,2] + sqsProj[2,*] * ((*self.pEigenSys).eigenVect)[2,2]

      ; transform into real coordinate system
   evMinMax = make_array([3,6], /float)
   evMinMax[0,*] = evsMinMax[0,*] * ((*self.pEigenSys).eigenVect)[0,0] + evsMinMax[1,*] * ((*self.pEigenSys).eigenVect)[1,0] + evsMinMax[2,*] * ((*self.pEigenSys).eigenVect)[2,0]
   evMinMax[1,*] = evsMinMax[0,*] * ((*self.pEigenSys).eigenVect)[0,1] + evsMinMax[1,*] * ((*self.pEigenSys).eigenVect)[1,1] + evsMinMax[2,*] * ((*self.pEigenSys).eigenVect)[2,1]
   evMinMax[2,*] = evsMinMax[0,*] * ((*self.pEigenSys).eigenVect)[0,2] + evsMinMax[1,*] * ((*self.pEigenSys).eigenVect)[1,2] + evsMinMax[2,*] * ((*self.pEigenSys).eigenVect)[2,2]

      ; transform 1st EVect into real coordinate system
   evsProj[0,0,*] = evProj[0,*] * ((*self.pEigenSys).eigenVect)[0,0]
   evsProj[0,1,*] = evProj[0,*] * ((*self.pEigenSys).eigenVect)[0,1]
   evsProj[0,2,*] = evProj[0,*] * ((*self.pEigenSys).eigenVect)[0,2]

      ; transform 2nd EVect into real coordinate system
   evsProj[1,0,*] = evProj[1,*] * ((*self.pEigenSys).eigenVect)[1,0]
   evsProj[1,1,*] = evProj[1,*] * ((*self.pEigenSys).eigenVect)[1,1]
   evsProj[1,2,*] = evProj[1,*] * ((*self.pEigenSys).eigenVect)[1,2]

      ; transform 3rd EVect into real coordinate system
   evsProj[2,0,*] = evProj[2,*] * ((*self.pEigenSys).eigenVect)[2,0]
   evsProj[2,1,*] = evProj[2,*] * ((*self.pEigenSys).eigenVect)[2,1]
   evsProj[2,2,*] = evProj[2,*] * ((*self.pEigenSys).eigenVect)[2,2]

      ; remember ev1Proj if necessary & free some memory
   if fGetEv1Proj then ev1Proj = reform(evProj[0,*]) else ev1Proj = 0b 
   evProj = 0b

      ; transform EVects to xyz-coordinates
   evsProj[*,0,*] /= xyzSizePerPixel[0]
   evsProj[*,1,*] /= xyzSizePerPixel[1]
   evsProj[*,2,*] /= xyzSizePerPixel[2]

      ; transform squares to xyz-coordinates
   sqProj[0,*] /= xyzSizePerPixel[0]
   sqProj[1,*] /= xyzSizePerPixel[1]
   sqProj[2,*] /= xyzSizePerPixel[2]

      ; transform evMinMax to xyz-coordinates
   evMinMax[0,*] /= xyzSizePerPixel[0]
   evMinMax[1,*] /= xyzSizePerPixel[1]
   evMinMax[2,*] /= xyzSizePerPixel[2]

      ; transform EVects to original position
   evsProj[*,0,*] += ((*self.pEigenSys).centerXYZ)[0]
   evsProj[*,1,*] += ((*self.pEigenSys).centerXYZ)[1]
   evsProj[*,2,*] += ((*self.pEigenSys).centerXYZ)[2]

      ; transform EVects to original position
   sqProj[0,*] += ((*self.pEigenSys).centerXYZ)[0]
   sqProj[1,*] += ((*self.pEigenSys).centerXYZ)[1]
   sqProj[2,*] += ((*self.pEigenSys).centerXYZ)[2]

      ; transform evMinMax to original position
   evMinMax[0,*] += ((*self.pEigenSys).centerXYZ)[0]
   evMinMax[1,*] += ((*self.pEigenSys).centerXYZ)[1]
   evMinMax[2,*] += ((*self.pEigenSys).centerXYZ)[2]

   return, {evsProj:evsProj, sqProj:sqProj, evMinMax:evMinMax, bottleAxisRatio:bottleAxisRatio, bottleMassRatio:bottleMassRatio, bottleMassRatio_3:bottleMassRatio_3, ev1Proj:ev1Proj}
end


function C_sROI3DObject::getxyzDist, xyzPoints = xyzPoints, xyzCenter = xyzCenter

   if (n_elements(xyzPoints) le 2) then self->IDLanROI::getProperty, data = xyzPoints
   if (n_elements(xyzCenter) le 0) then xyzCenter = ((*self.pEigenSys).centerXYZ)

   xyzSizePerPixel = self->getxyzSizePerPixel()

      ; transform xyzPoints to real coordinate system
   xyzPoints[0,*] *= xyzSizePerPixel[0]
   xyzPoints[1,*] *= xyzSizePerPixel[1]
   xyzPoints[2,*] *= xyzSizePerPixel[2]
   xyzCenter *= xyzSizePerPixel

   xyzDist = reform(sqrt( (xyzPoints[0,*] - xyzCenter[0])^2 + (xyzPoints[1,*] - xyzCenter[1])^2 + (xyzPoints[2,*] - xyzCenter[2])^2))

      ; transform real coordinate system to xyzPoints
   xyzPoints[0,*] /= xyzSizePerPixel[0]
   xyzPoints[1,*] /= xyzSizePerPixel[1]
   xyzPoints[2,*] /= xyzSizePerPixel[2]
   xyzCenter /= xyzSizePerPixel

   return, xyzDist
end


function C_sROI3DObject::makePixelObjectInVoxel,$; Returns 2D/3D-Object = 1 in voxel (2*pixelFrame + dim_xyz)
                         xyzPoints = xyzPoints,$; xyzPoints: xyzPoints Pixel Position of Object in Base-Frame
                         pixelFrame = pixelFrame,$; pixelFrame: Frame size in Pixel around Object | Default = 1
                         all = all

   if (n_elements(pixelFrame) le 0) then pixelFrame = 1
   if (n_elements(xyzPoints) le 0) then self->getProperty, data = xyzPoints

   minX = min(xyzPoints[0,*], max = maxX)
   minY = min(xyzPoints[1,*], max = maxY)
   minZ = min(xyzPoints[2,*], max = maxZ)

   obj = make_array(maxX - minX + 1 + 2 * pixelFrame, maxY - minY + 1 + 2 * pixelFrame, maxZ - minZ + 1 + 2 * pixelFrame, /byte)
   obj[xyzPoints[0,*] - (minX - pixelFrame), xyzPoints[1,*] - (minY - pixelFrame), xyzPoints[2,*] - (minZ - pixelFrame)] = 1b

   if (n_elements(all) gt 0) then return, {obj:obj,$
                                           minX:minX,$
                                           minY:minY,$
                                           minZ:minZ,$
                                           maxX:maxX,$
                                           maxY:maxY,$
                                           maxZ:maxZ,$
                                           pixelFrame:pixelFrame} $
      else return, obj
end


function C_sROI3DObject::makeObjectBorderInVoxel,$       ; Returns Border Pixels of voxelObject
                         neighborhood = neighborhood,$   ; neighborhood = 4/8 for Object Border Definition | Default = 4
                         voxelObject = voxelObject       ; voxelObject: 2D/3D-Object = 1 in Voxel (pixelFrame + dim_xyz)

    if (n_elements(neighborhood) le 0) then begin
       neighborhood = 4
       voxelObject = self->makePixelObjectInVoxel()
    endif

    dimVO = size(voxelObject, /dim)

    case n_elements(dimVO) of
        2: begin
              ; Enlarge voxelObject dimension
          extVO = make_array( dimVO[0]+2, dimVO[1]+2, type = size(voxelObject, /type))
              ; Fill center with original picture
          extVO[ 1:dimVO[0], 1:dimVO[1]] = voxelObject[*,*]
          case neighborhood of
              4: begin
                   return, (voxelObject ne 0) * $
                           (( extVO[ 0:dimVO[0]-1, 1:dimVO[1]] ne voxelObject) or $
                            ( extVO[ 2:dimVO[0]+1, 1:dimVO[1]] ne voxelObject) or $
                            ( extVO[ 1:dimVO[0], 0:dimVO[1]-1] ne voxelObject) or $
                            ( extVO[ 1:dimVO[0], 2:dimVO[1]+1] ne voxelObject))
                 endcase
              8: begin
                   return, (voxelObject ne 0) * $
                            (( extVO[ 0:dimVO[0]-1, 1:dimVO[1]] ne voxelObject) or  $
                            ( extVO[ 2:dimVO[0]+1, 1:dimVO[1]] ne voxelObject) or $
                            ( extVO[ 1:dimVO[0], 0:dimVO[1]-1] ne voxelObject) or  $
                            ( extVO[ 1:dimVO[0], 2:dimVO[1]+1] ne voxelObject) or $
                            ( extVO[ 0:dimVO[0]-1, 0:dimVO[1]-1] ne voxelObject) or $
                            ( extVO[ 0:dimVO[0]-1, 2:dimVO[1]+1] ne voxelObject) or $
                            ( extVO[ 2:dimVO[0]+1, 0:dimVO[1]-1] ne voxelObject) or  $
                            ( extVO[ 2:dimVO[0]+1, 2:dimVO[1]+1] ne voxelObject))
                 endcase
          endcase
         endcase
       3: begin
              ; Enlarge voxelObject dimension
          extVO = make_array( dimVO[0]+2, dimVO[1]+2, dimVO[2]+2, type = size(voxelObject, /type))
              ; Fill center with original picture
          extVO[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] = voxelObject[*,*,*]
          case neighborhood of
          4: begin
                 return, (voxelObject ne 0) * $
                        (( extVO[ 0:dimVO[0]-1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject))
              endcase
          8: begin
                 return, (voxelObject ne 0) * $
                        (( extVO[ 0:dimVO[0]-1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject) or $

                         ( extVO[ 0:dimVO[0]-1, 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 0:dimVO[0]-1, 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject) or $

                         ( extVO[ 0:dimVO[0]-1, 0:dimVO[1]-1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( extVO[ 0:dimVO[0]-1, 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject) or $
                         ( extVO[ 0:dimVO[0]-1, 2:dimVO[1]+1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 0:dimVO[1]-1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 2:dimVO[1]+1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 0:dimVO[1]-1, 2:dimVO[2]+1] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 2:dimVO[1]+1, 2:dimVO[2]+1] ne voxelObject) or $

                         ( extVO[ 0:dimVO[0]-1, 0:dimVO[1]-1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 0:dimVO[0]-1, 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 0:dimVO[0]-1, 2:dimVO[1]+1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 0:dimVO[1]-1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 2:dimVO[0]+1, 2:dimVO[1]+1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 0:dimVO[1]-1, 0:dimVO[2]-1] ne voxelObject) or $
                         ( extVO[ 1:dimVO[0], 2:dimVO[1]+1, 0:dimVO[2]-1] ne voxelObject))
              endcase
         endcase
       endcase
    endcase
end


function C_sROI3DObject::getObjectSurfaceFromPolygons, polygons = polygons, vertices = vertices, realSize = realSize

    if (n_elements(realSize) gt 0) then begin
       vertices[0,*]  *= ((*self.pEigenSys).sizePerXYZ)[0]
       vertices[1,*]  *= ((*self.pEigenSys).sizePerXYZ)[1]
       vertices[2,*]  *= ((*self.pEigenSys).sizePerXYZ)[2]
    endif

    pos = 0l
    triPos = [0]
    repeat begin
       case polygons[pos] of
         3: begin
           triPos = [triPos, polygons[pos+1:pos+3]]
           pos+=4
          endcase
         4: begin
          triPos = [triPos, polygons[[pos+1, pos+3, pos+2]], polygons[[pos+1,pos+4,pos+3]]]
          pos+=5
         endcase
       else: begin
          print, 'Polygon of higher order than 4'
          pos += polygons[pos]
         endcase
       endcase
    endRep until (pos ge n_elements(polygons))
    triPos = triPos[1:*]
    triTri = make_array(n_elements(triPos)/3, /index) * 3

    a = sqrt((vertices[0,triPos[triTri+1]]-vertices[0,triPos[triTri]])^2+(vertices[1,triPos[triTri+1]]-vertices[1,triPos[triTri]])^2+(vertices[2,triPos[triTri+1]]-vertices[2,triPos[triTri]])^2)
    b = sqrt((vertices[0,triPos[triTri+2]]-vertices[0,triPos[triTri+1]])^2+(vertices[1,triPos[triTri+2]]-vertices[1,triPos[triTri+1]])^2+(vertices[2,triPos[triTri+2]]-vertices[2,triPos[triTri+1]])^2)
    c = sqrt((vertices[0,triPos[triTri+2]]-vertices[0,triPos[triTri]])^2+(vertices[1,triPos[triTri+2]]-vertices[1,triPos[triTri]])^2+(vertices[2,triPos[triTri+2]]-vertices[2,triPos[triTri]])^2)

    s = (a+b+c)/2.
    a = sqrt(s*(s-a)*(s-b)*(s-c))
    return, total(a)
end


function C_sROI3DObject::getObjectSurfacePosition

    if (n_elements(realSize) gt 0) then begin
       xySize = ((*self.pEigenSys).sizePerXYZ)[0] * ((*self.pEigenSys).sizePerXYZ)[1]
       xzSize = ((*self.pEigenSys).sizePerXYZ)[0] * ((*self.pEigenSys).sizePerXYZ)[2]
       yzSize = ((*self.pEigenSys).sizePerXYZ)[1] * ((*self.pEigenSys).sizePerXYZ)[2]
    endif else begin
       xySize = 1
       xzSize = 1
       yzSize = 1
    endelse

    voxelObject = self->makePixelObjectInVoxel()
    dimVO = size(voxelObject, /dim) - 2

    return, total(yzSize * ( voxelObject[ 0:dimVO[0]-1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  yzSize * ( voxelObject[ 2:dimVO[0]+1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xzSize * ( voxelObject[ 1:dimVO[0], 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xzSize * ( voxelObject[ 1:dimVO[0], 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xySize * ( voxelObject[ 1:dimVO[0], 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xySize * ( voxelObject[ 1:dimVO[0], 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ))
end


function C_sROI3DObject::getObjectSurfaceInVoxels, realSize = realSize

    if (n_elements(realSize) gt 0) then begin
       xySize = ((*self.pEigenSys).sizePerXYZ)[0] * ((*self.pEigenSys).sizePerXYZ)[1]
       xzSize = ((*self.pEigenSys).sizePerXYZ)[0] * ((*self.pEigenSys).sizePerXYZ)[2]
       yzSize = ((*self.pEigenSys).sizePerXYZ)[1] * ((*self.pEigenSys).sizePerXYZ)[2]
    endif else begin
       xySize = 1
       xzSize = 1
       yzSize = 1
    endelse

    voxelObject = self->makePixelObjectInVoxel()
    dimVO = size(voxelObject, /dim) - 2

    return, total(yzSize * ( voxelObject[ 0:dimVO[0]-1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  yzSize * ( voxelObject[ 2:dimVO[0]+1, 1:dimVO[1], 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xzSize * ( voxelObject[ 1:dimVO[0], 0:dimVO[1]-1, 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xzSize * ( voxelObject[ 1:dimVO[0], 2:dimVO[1]+1, 1:dimVO[2]] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xySize * ( voxelObject[ 1:dimVO[0], 1:dimVO[1], 0:dimVO[2]-1] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ) + $
                  xySize * ( voxelObject[ 1:dimVO[0], 1:dimVO[1], 2:dimVO[2]+1] ne voxelObject[ 1:dimVO[0], 1:dimVO[1], 1:dimVO[2]] ))
end


function C_sROI3DObject::makeObjectBorderChainCode, voxelObject = voxelObject
; voxelObject: 2D/3D-Object = 1 in Voxel (pixelFrame + dim_xyz)
;                                           |3  2  1|
; Builds chainCodeVector for voxelObject:   |4  X  0|     ; [Xpixel, YPixel, chaincode ......]
;                                           |5  6  7|

    dimVO = size(voxelObject, /dim)
    chainCodeVector = make_array(2*n_elements(voxelObject), /int, value = -1)

    s = [0, 1, 2, 3, 4, 5, 6, 7, 0, 1, 2, 3, 4, 5]
    x = [1, 1, 0, -1,-1, -1, 0, 1]
    y = [0, 1, 1, 1, 0, -1,-1,-1]
    r = [6, 6, 0, 0, 2, 2, 4, 4]

    a = (where(voxelObject ne 0))[0]
    chainCodeVector[0:1] = [ a mod dimVO[0], floor(1.*a/dimVO[0])]  ; Set first Object-Pixel
    xRun = chainCodeVector[0]
    yRun = chainCodeVector[1]

    for j = 3, 0, -1 do begin
       if voxelObject[xRun+x[j], yRun+y[j]] then begin
          chainCodeVector[2] = j
          goDirection = j
          goright = r[j]
       endif
    endfor

    if (chainCodeVector[2] eq -1) then goDirection = -1

    j = 2
    while ((chainCodeVector[0] ne xRun) or (chainCodeVector[1] ne yRun) or (chainCodeVector[j>3] ne goDirection)) do begin
       xRun += x[chainCodeVector[j]]
       yRun += y[chainCodeVector[j]]
       a = goright + 1
       j += 1
       while (a lt (goright+8)) do begin
          if voxelObject[xRun + x[s[a]], yRun + y[s[a]]] then begin
             chainCodeVector[j] = s[a]
             goright = r[s[a]]
             a = 20
          endif
          a += 1
       endwhile
    endwhile

    return, [chainCodeVector[where(chainCodeVector ne -1)], -1]
end


function C_sROI3DObject::makePolygonFromChainCodeVector, chainCodeVector,$   ; Returns ClosedPolygonxyzPixel
                                                         no_closed = no_closed,$
                                                         reducedChainCodeVector = reducedChainCodeVector ; Contains ChainCode for polygon

    if ((size(chainCodeVector, /dim))[0] le 3) then return, [ transpose( [chainCodeVector[0], chainCodeVector[0]]),$
                                                              transpose( [chainCodeVector[1], chainCodeVector[1]])]
    closedPolygonxPixel = chainCodeVector[0]
    closedPolygonyPixel = chainCodeVector[1]
    xyzStep = [chainCodeVector[0], chainCodeVector[1]]
    stepVector = [[1, 0], [1, 1], [0, 1], [-1, 1], [-1, 0], [-1, -1], [0, -1], [1, -1]]
    reducedChainCodeVector = 0

    for i = 2, n_elements(chainCodeVector)-3 do begin
       repeat begin
         xyzStep = temporary(xyzStep) + stepVector[*, chainCodeVector[i]]
         i += 1
       endrep until (chainCodeVector[i-1] ne chainCodeVector[i])
       i -= 1
       closedPolygonxPixel = [closedPolygonxPixel, xyzStep[0]]
       closedPolygonyPixel = [closedPolygonyPixel, xyzStep[1]]
       reducedChainCodeVector = [reducedChainCodeVector, chainCodeVector[i]]
    endfor

       ; make PolygonVector that does not close at last position
    reducedChainCodeVector = reducedChainCodeVector[1:*]
    if keyWord_set(no_closed) then begin
       polygon = [transpose(closedPolygonxPixel), transpose(closedPolygonyPixel)]
       return, polygon[*,0: (size(polygon, /dim))[1]-2]
    endif else return, [transpose(closedPolygonxPixel), transpose(closedPolygonyPixel)]
end


function C_sROI3DObject::inflatePolygon, polygon, deflate = deflate

       ; Add 180 degree to the NormalVector in order to shrink Object
    if keyWord_set(deflate) then addAngle = 180. else addAngle = 0.

    dim_Polygon = size(polygon, /dim)
    objBorderThickness = *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Border Thickness'))[0]]

    newPolygonxPixel = 0.
    newPolygonyPixel = 0.

       ;  if only one pixel
    if (polygon[0, 0] eq polygon[0, 1]) and ((polygon[1, 0] eq polygon[1, 1]) ) then begin
       newPolygonxPixel = [ newPolygonxPixel,polygon[0, 0] + ([-1.,1., 1.,-1.]*objBorderThickness) ]
       newPolygonyPixel = [ newPolygonyPixel, polygon[1, 0] + ([-1.,-1., 1.,1.]*objBorderThickness) ]
    endif else begin

       ;  add last postition as first position
    polygon = [[polygon[*,dim_Polygon[1]-2]], [polygon]]

    angleVector = fltArr(dim_Polygon[1])
    for i = 0, dim_Polygon[1]-2 do begin

         ;     calcultate angles between polygons
         angleVector[i] = s_GetPolygonAngle( fltArr(2) + polygon[*, i] - polygon[*, i+1],$       ; 1st Vector
                                             fltArr(2) + polygon[*, i+1] - polygon[*, i+2])   ; 2nd Vector

         if (angleVector[i] GT 359.) then begin
           for j = 0, 1 do begin
              newPolygonxyPixel = s_RotVector2D(polygon[*, i+1],$   ; xyVectorFrom (Center of Rotation)
                                                polygon[*, i],$     ; xyVectorTo
                                                ((([135., 225.] + addAngle) mod 360.0001))[j],$   ;, alfa;
                                                unit = objBorderThickness)
              newPolygonxPixel = [newPolygonxPixel, newPolygonxyPixel[0]]
              newPolygonyPixel = [newPolygonyPixel, newPolygonxyPixel[1]]
            endfor
         endif else begin
            newPolygonxyPixel = s_RotVector2D(polygon[*, i+1],$    ; xyVectorFrom
                                              polygon[*, i],$       ; xyVectorTo (Center of Rotation)
                                              ((angleVector[i]/2.) + addAngle) mod 360.0001,$    ;, alfa;
                                              unit = objBorderThickness)
            newPolygonxPixel = [ newPolygonxPixel, newPolygonxyPixel[0] ]
            newPolygonyPixel = [ newPolygonyPixel, newPolygonxyPixel[1] ]
         endelse
       endfor
    endelse
    newPolygon = ([transpose(newPolygonxPixel), transpose(newPolygonyPixel)]) [*, 1:*]
    return, newPolygon
end


function C_sROI3DObject::Tesselate2DObject, color = color, pPolynomList = pPolynomList
; PointerList on Polynoms, [0]: BaseObject, [1...n]: Holes

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


pro C_sROI3DObject::makeZSliceObjectBorderPolygon, color = color, oModel = oModel, modelStyle = modelStyle, borderThick = borderThick, borderLineStyle = borderLineStyle

    if (n_elements(color) le 0) then color = [0, 255, 0]

    self->getProperty, data = xyzPoints, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

       ; get Object in small Voxel
    voxelObject = self->makePixelObjectInVoxel(xyzPoints = xyzPoints, pixelFrame = 1)
    dimVO = size(voxelObject, /dim)
    voxelZSliceObject = intArr(dimVO[0], dimVO[1])

       ; determines number of Objects in all Slices, Objects = 1, 2, 3, 4, ... n
    countObjectPolynoms = 0
    for z = 1, dimVO[2]-2 do begin
       voxelZSliceObject[*,*] = voxelObject[*,*,z]
       voxelObject[*,*,z] = s_2DLabelRegion(voxelZSliceObject, /no_sort, offSet = countObjectPolynoms)
       countObjectPolynoms >= max(voxelObject[*,*,z])
    endfor

    whereObject = (where(voxelObject[*,*,1] ne 0))[0]
    xyz_dif = xyzPoints[0:2] - [whereObject mod dimVO[0], floor(whereObject / (1.*dimVO[0])), 1]

       ; for all Slices
    for z = 1, dimVO[2]-2 do begin
       voxelZSliceObject[*,*] = voxelObject[*,*,z]
       minZSO = min(voxelZSliceObject, max = maxZSO)
          ; for all Objects in Slices
       for i = (minZSO > 1), maxZSO do begin
          ; determines number of holes in object and sets border = 0, main object = 1, and numbers holes = 2, 3, 4, ... n
         voxelHoles = s_2DLabelObjectAndHoles(voxelZSliceObject eq i)

          ; for all Objects and Holes in Objects
         for k = 1, max(voxelHoles) do begin
              ; get ObjectBorder
           if (k eq 1) then object_i = s_rand4(voxelHoles gt 0) else object_i = s_rand4(voxelHoles eq k)
              ; get chainCodeVector
           chainCodeVector = self->makeObjectBorderChainCode(voxelObject = object_i)
              ; makePolygonFromChainCodeVector
           polygon = self->makePolygonFromChainCodeVector(chainCodeVector)
              ; inflate the Object Borders, deflate the Holes...
           polygon = self->inflatePolygon(polygon)
              ; scale to original Position and Add to    pPolynomList
           polygon[0,*] += xyz_dif[0]
           polygon[1,*] += xyz_dif[1]
           oModel->add, obj_new('IDLgrPolygon', [polygon, transpose(make_array( (size(polygon, /dim))[1], /float, value = z+xyz_dif[2]))],$
                                style = modelStyle, color = color,thick = borderThick, lineStyle = borderLineStyle, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv )
         endfor
       endfor
    endfor
end


function C_sROI3DObject::getObjectBorderPolygonXX, color = color,$
                                                   pModel = pModel
                                                   TesselatedPolygonModel = TesselatedPolygonModel
                                                   PolygonModel = PolygonModel

    self->IDLanROI::getProperty, data = xyzPoints
    case n_elements(*self.pFramePixSize) of
        2: begin
              ; Set Object in small frame
              voxelObject = self->C_sROI3DObject::makePixelObjectInVoxel(xyzPoints = xyzPoints, pixelFrame = 1)
              dimVO = size(voxelObject, /dim)

                  ; Determines number of holes in Object and sets border = 0, main object = 1, and numbers holes = 2, 3, 4, ... n
              labelRegionData = s_2DLabelRegion(voxelObject eq 0, /no_border)
              whereLabelRegionData = where(labelRegionData ne 0, count)
              if (count ne 0) then labelRegionData[whereLabelRegionData] = temporary(labelRegionData[whereLabelRegionData]) + 1
              labelRegionData = temporary(labelRegionData) + voxelObject

              pPolynomList = ptrArr(max(labelRegionData))

              object_1 = labelRegionData eq 1
              for i = 1, max(labelRegionData) do begin
                 if (i eq 1) then object_i = object_1 $
                    else object_i = s_rand4(labelRegionData eq i)

                 whereObject = where(object_i eq 1, count)
                 xy_pos = intArr(2, count)
                 xy_pos[0,*] = whereObject mod dimVO[0]
                 xy_pos[1,*] = floor(whereObject) / (1.*dimVO[0])
                 xy_centre = [ total(xy_pos[0,*]), total(xy_pos[1,*]) ] / (1.*count)

                    ; Get chainCodeVector
                 chainCodeVector = self->C_sROI3DObject::makeObjectBorderChainCode(voxelObject = object_i)
                    ; makePolygonFromChainCodeVector
                 polygon = self->C_sROI3DObject::makePolygonFromChainCodeVector(chainCodeVector)

                    ; Inflate the Object Borders, deflate the Holes...
                 if (i eq 1) then xy_dif = xyzPoints[0:1] - xy_pos
                    polygon = self->C_sROI3DObject::inflatePolygon(polygon)
    ;            endif else polygon = self->C_sROI3DObject::inflatePolygon(polygon, /deflate)

                    ; Scale to original Position and Add to   pPolynomList
                 polygon[0,*] = temporary(polygon[0,*]) + xy_dif[0]
                 polygon[1,*] = temporary(polygon[1,*]) + xy_dif[1]
                 pPolynomList[i-1] = ptr_new(polygon, /no_copy)
              endfor

              if keyWord_set(pModel) then begin
                 *pModel = self->C_sROI3DObject::Tesselate2DObject(pPolynomList = pPolynomList, color = color)
                 for i = 0, n_elements(pPolynomList)-1 do ptr_free, pPolynomList[i]
                 ptr_free, pPolynomList
                 return, 1
              endif
              if keyWord_set(TesselatedPolygonModel) then begin
                 oModel = self->C_sROI3DObject::Tesselate2DObject(pPolynomList = pPolynomList, color = color)
                 for i = 0, n_elements(pPolynomList)-1 do ptr_free, pPolynomList[i]
                 ptr_free, pPolynomList
                 return, oModel
              endif
              if keyWord_set(PolygonModel) then begin
                 oModel = self->C_sROI3DObject::getoPolygonBorderModel(pPolynomList = pPolynomList, color = color)
                 for i = 0, n_elements(pPolynomList)-1 do ptr_free, pPolynomList[i]
                 ptr_free, pPolynomList
                 return, oModel
              endif
         endcase
       3: begin
         endcase
    endcase
    return, oModel
end


pro C_sROI3DObject::cleanUp
    if ptr_valid(self.pParamStruct) then begin
       ptr_free, (*self.pParamStruct).pNames
       for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
       ptr_free, self.pParamStruct
    endif

    if obj_valid(self.oModelContainer) then begin
       oModels = self.oModelContainer->get(/all)
       self.oModelContainer->remove, /all
       if obj_valid(oModels[0]) then for i = 0, n_elements(oModels)-1 do obj_destroy, oModels[i]
       obj_destroy, self.oModelContainer
    endif

    if ptr_valid(self.pWherePoints) then ptr_free, self.pWherePoints
    if ptr_valid(self.pPointValues) then ptr_free, self.pPointValues
    if ptr_valid(self.pEigenSys) then ptr_free, self.pEigenSys
    self->IDLgrROI::cleanup
end


function C_sROI3DObject::getParameterNameList
   return, ['Number',$
            'Time Position',$
            'Channel Position',$
            'z-Slice Position',$
            'z-Slice Initial',$
            'x-Size [real]',$
            'y-Size [real]',$
            'z-Size [real]',$
            'x-Size [pixel]',$
            'y-Size [pixel]',$
            'z-Size [pixel]',$
            'Border Thickness',$
            'Border Object',$
            'Border Style' ,$
            'Border Color_r',$
            'Border Color_g',$
            'Border Color_b',$
            'Border Thick' ,$
            'Border LineStyle']
end


function C_sROI3DObject::init, number = number,$                   ;Object Number
                               xyzFramePixSize = xyzFramePixSize,$      ; 2D- or 3D-embedding Object-Space
                               xyzFrameRealSize = xyzFrameRealSize,$
                               tPos = tPos,$
                               chPos = chPos,$
                               zPos = zPos,$
                               ZSliceInitial = ZSliceInitial,$
                               objBorderThickness = objBorderThickness,$       ; Frame Units for Object Borders
                               borderColor = borderColor,$
                               borderThick = borderThick,$
                               borderLineStyle = borderLineStyle,$
                               pointValues = pointValues,$        ; Point-Values
                               wherePoints = wherePoints          ; 3D-Object-Coordinates

    parameterNameList = self->getParameterNameList()
    paramStruct = {ROI3DParamStruct,$
                   pValues: ptrArr(n_elements(parameterNameList), /allocate),$    ; Pointer on Parameter Values.
                   pNames: ptr_new(parameterNameList, /no_copy)}                  ; Pointer on Parameter Names.

    if (n_elements(number) eq 0) then number = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Number'))[0]] = number
    if (n_elements(xyzFrameRealSize) eq 0) then xyzFrameRealSize = [1,1,1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [real]'))[0]] = xyzFrameRealSize[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [real]'))[0]] = xyzFrameRealSize[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Size [real]'))[0]] = xyzFrameRealSize[2]
    if (n_elements(xyzFramePixSize) eq 0) then xyzFramePixSize = [1,1,1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'x-Size [pixel]'))[0]] = xyzFramePixSize[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'y-Size [pixel]'))[0]] = xyzFramePixSize[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Size [pixel]'))[0]] = xyzFramePixSize[2]
    if (n_elements(tPos) eq 0) then tPos = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Time Position'))[0]] = tPos
    if (n_elements(chPos) eq 0) then chPos = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Channel Position'))[0]] = chPos
    if (n_elements(zPos) eq 0) then zPos = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Slice Position'))[0]] = zPos
    if (n_elements(ZSliceInitial) eq 0) then ZSliceInitial = -1
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'z-Slice Initial'))[0]] = ZSliceInitial
    if (n_elements(objBorderThickness) eq 0) then objBorderThickness = .5
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Thickness'))[0]] = objBorderThickness
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Object'))[0]] = 'CONTAINED'
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Style'))[0]] = 1
    if (n_elements(borderColor) eq 0) then borderColor = [255,0,0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_r'))[0]] = borderColor[0]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_g'))[0]] = borderColor[1]
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Color_b'))[0]] = borderColor[2]
    if (n_elements(borderThick) eq 0) then borderThick = 1.
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border Thick'))[0]] = borderThick
    if (n_elements(borderLineStyle) eq 0) then borderLineStyle = 0
    *paramStruct.pValues[(where(*paramStruct.pNames eq 'Border LineStyle'))[0]] = borderLineStyle
    self.pParamStruct = ptr_new(paramStruct, /no_copy)
    if (n_elements(wherePoints) ne 0) then self.pWherePoints = ptr_new(wherePoints, /no_copy)
    if (n_elements(pointValues) ne 0) then self.pPointValues = ptr_new(pointValues, /no_copy)

    eigenState = {fOK:0b,$
                  sizePerXYZ:xyzFrameRealSize/xyzFramePixSize,$
                  centerXYZ:make_array(3, /double),$
                  inertiaT:make_array(3,3, /double),$
                  eigenVect:make_array(3,3, /double),$
                  eigenVals:make_array(3, /double)}

    self.pEigenSys = ptr_new(eigenState, /no_copy)

    if ptr_valid(self.pWherePoints) then begin
       f = self->IDLgrROI::init(make_array(3,4, /int, value = 1), style = 0,$
                                name = strCompress(*(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]], /rem),$
                                color = [255,0,0], /double, thick = 1)
       self->setIDLgrROIXYZData
    endif

    (*self.pEigenSys).fOK = self->calcEigenSys(/setEigenSys)
    self.oModelContainer = obj_new('IDL_Container')
    return, 1
end


pro C_sROI3DObject__define
   tmp = {C_sROI3DObject, pParamStruct:ptr_new(),$
                          pWherePoints:ptr_new(),$
                          pPointValues:ptr_new(),$
                          pEigenSys:ptr_new(),$
                          oModelContainer:obj_new(),$
                          inherits IDLgrROI}
end