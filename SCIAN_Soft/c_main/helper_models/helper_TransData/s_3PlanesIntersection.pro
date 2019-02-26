;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_3PlanesIntersection
;
; PURPOSE:
;       Calculates and returns point intersection between 3 planes
;
; AUTHOR:
;   Hector Moraga (2012)
;   e_mail: hector.moraga.aros@gmail.com
;
; CALLING SEQUENCE:
;      point = s_3PlanesIntersection(point1, normal1, point2, normal2, point3, normal3)
;
; REQUIRED INPUTS:
;      pointi, normali
;    where:  pointi = (x,y,z) of point from plane i 
;            normali = (nx, ny, nz) normal components of normal vector of plane i 
;        i={0..2}
; returns:      point
;_____________________________IOISIOI____________________

function s_3PlanesIntersection, point1, normal1, point2, normal2, point3, normal3 

   ; Evaluate if each input component has 3 dimensions 
   
   eval=0
   if (size(point1, /DIMENSIONS) ne 3) then eval+=1
   if (size(normal1, /DIMENSIONS) ne 3) then eval+=1
   if (size(point2, /DIMENSIONS) ne 3) then eval+=1
   if (size(normal2, /DIMENSIONS) ne 3) then eval+=1
   if (size(point3, /DIMENSIONS) ne 3) then eval+=1
   if (size(normal3, /DIMENSIONS) ne 3) then eval+=1
   
   if (eval ne 0) then begin 
      message, 'Error: alguna componente no tiene 3 dimensiones'
      return, -1
   endif
   
   
   D1=-(normal1[0]*point1[0]+normal1[1]*point1[1]+normal1[2]*point1[2])
   D2=-(normal2[0]*point2[0]+normal2[1]*point2[1]+normal2[2]*point2[2])
   D3=-(normal3[0]*point3[0]+normal3[1]*point3[1]+normal3[2]*point3[2])
   
   A=[[normal1[0],normal1[1],normal1[2]],[normal2[0],normal2[1],normal2[2]],[normal3[0],normal3[1],normal3[2]]]
   A=double(A)
   
   result=cramer(A,-1d*[D1,D2,D3], /DOUBLE)
   
   return, result
end