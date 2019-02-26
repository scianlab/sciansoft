;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_TriangleArea.pro
;
; PURPOSE:
;       calculate generic triangle area
;
; AUTHOR:
;   Felipe Santibañez (2012)
;   e_mail: fsantibanez@med.uchile.cl
;
; CALLING SEQUENCE:
;      pro s_TriangleArea
;
; REQUIRED INPUTS:
;      NONE
;_____________________________IOISIOI____________________

function s_TriangleAreaByPoints, vP1 = vP1, vP2 = vP2, vP3 = vP3
; Heron Method

  dummy = (vP1 - vP2)
  segAB = sqrt(dummy*dummy) 
  dummy = (vP2 - vP3)
  segBC = sqrt(dummy*dummy)
  dummy = (vP3 - vP1)
  segCA = sqrt(dummy*dummy)
  
  semiP = (segAB+segBC+segCA)/2.0d

  return, sqrt(semiP*(semiP-segAB)*(semiP-segBC)*(semiP-segCA))
end

function s_TriangleAreaByTCOS, vE1 = vE1, vE2 = vE2, vAngle = vAngle
; Heron Method

  ; Find third segment using cosine theorem
  ; angle in radians.....
  segAB = vE1
  segBC = vE2
  segCA = sqrt(vE1*vE1 + vE2*vE2 - 2*vE1*vE2*cos(vAngle))
  
  semiP = (segAB+segBC+segCA)/2.0d

  return, sqrt(semiP*(semiP-segAB)*(semiP-segBC)*(semiP-segCA))
end