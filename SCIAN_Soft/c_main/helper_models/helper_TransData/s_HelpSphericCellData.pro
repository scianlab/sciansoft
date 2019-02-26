;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_HelpSphericCellData
;
; PURPOSE:
;       Calculates and returns center point for a circle based in 3 points 
;
; AUTHOR:
;   Felipe Santibáñez Leal (2012)
;   e_mail: fsantibanez@med.uchile.cl
;
; CALLING SEQUENCE:
;      s_HelpSphericCellData
;
; REQUIRED INPUTS:
;        i
; returns:      point
;_____________________________IOISIOI____________________

function s_CenterCircleBy3Point, point1 = point1, point2 = point2, point3 = point3, xyzDim = xyzDim  

  centroCircle = -1

   a = point1
   b = point2
   c = point3

   a[1] = xyzDim[1] - a[1]
   b[1] = xyzDim[1] - b[1]
   c[1] = xyzDim[1] - c[1]

   ;; Calculo centro...
   p1 = (a+b)/2.0
   p2 = (c+b)/2.0
   normalAlPlano = [0.0,0.0,1.0]
   vectorAB = a - b
   vectorAB = vectorAB / sqrt(Total(vectorAB*vectorAB))
   vectorCB = c - b
   vectorCB = vectorCB / sqrt(Total(vectorCB*vectorCB))
   
   N1 = CROSSP(normalAlPlano,vectorAB)
     tempN = sqrt(total(N1*N1))
       if(tempN gt 0.0) then N1 = N1 / tempN   
   N2 = CROSSP(normalAlPlano,vectorCB)
     tempN = sqrt(total(N2*N2))
       if(tempN gt 0.0) then N2 = N2 / tempN    
   alfa1 = -1000000.0
   if( (abs(N2[1]) gt 0.0001) and (abs(N1[0]-(N2[0]*N1[1]/N2[1])) gt 0.0001)) then begin ; x,y
      alfa1 = p2[0] - p1[0]+ (N2[0]/N2[1])*(p1[1]-p2[1]) 
      alfa1 = alfa1/(N1[0]-(N2[0]*N1[1]/N2[1]))
   endif else begin
       if( (abs(N2[2]) gt 0.0001) and (abs(N1[0]-(N2[0]*N1[2]/N2[2])) gt 0.0001)) then begin ; x,z
          alfa1 = p2[0] - p1[0]+ (N2[0]/N2[2])*(p1[2]-p2[2]) 
          alfa1 = alfa1/(N1[0]-(N2[0]*N1[2]/N2[2]))
      endif else begin
           if( (abs(N2[0]) gt 0.0001) and (abs(N1[1]-(N2[1]*N1[0]/N2[0])) gt 0.0001)) then begin ; y,x
              alfa1 = p2[1] - p1[1]+ (N2[1]/N2[0])*(p1[0]-p2[0]) 
              alfa1 = alfa1/(N1[1]-(N2[1]*N1[0]/N2[0]))
           endif else begin
               if( (abs(N2[2]) gt 0.0001) and (abs(N1[1]-(N2[1]*N1[2]/N2[2])) gt 0.0001)) then begin ; y,z
                  alfa1 = p2[1] - p1[1]+ (N2[1]/N2[2])*(p1[2]-p2[2]) 
                  alfa1 = alfa1/(N1[1]-(N2[1]*N1[2]/N2[2]))
                endif else begin
                     if( (abs(N2[0]) gt 0.0001) and (abs(N1[2]-(N2[2]*N1[0]/N2[0])) gt 0.0001)) then begin ; z,x
                        alfa1 = p2[2] - p1[2]+ (N2[2]/N2[0])*(p1[0]-p2[0]) 
                        alfa1 = alfa1/(N1[2]-(N2[2]*N1[0]/N2[0]))
                    endif else begin
                       if( (abs(N2[1]) gt 0.0001) and (abs(N1[2]-(N2[2]*N1[1]/N2[1])) gt 0.0001)) then begin ; z,y
                          alfa1 = p2[2] - p1[2]+ (N2[2]/N2[1])*(p1[1]-p2[1]) 
                          alfa1 = alfa1/(N1[2]-(N2[2]*N1[1]/N2[1]))
                       endif
                    endelse
                endelse
           endelse 
      endelse
   endelse
   if(alfa1 ne -1000000.0) then begin
      centroCircle = p1 + alfa1 * N1
   endif

   return, centroCircle
end

function s_CenterCircleBy3PointfromFile, filename = filename, point1 = point1, point2 = point2, point3 = point3, drifted = drifted, xyzDim = xyzDim    
    drifted = -1 ; means error  
    point1 = make_Array(3,/double)
    point2 = make_Array(3,/double)
    point3 = make_Array(3,/double)
    if(FILE_TEST(filename)) then begin
       line = make_Array(3,/double)
       h=0
       GET_LUN, inunit
       openr, inunit, filename
          salirCiclo = 0b
          while (~ EOF(inunit) and (salirCiclo eq 0b)) do begin
             if(h le 2) then begin
                 READF, inunit, line
                 if(h eq 0) then point1 =line
                 if(h eq 1) then point2 =line
                 if(h eq 2) then point3 =line
               endif else begin
                 if(h eq 3) then begin
                   READF, inunit, drifted
                   salirCiclo = 1b
                 endif
               endelse
              h++ 
          endwhile    
       FREE_LUN, inunit
    endif       

    centroCircle = s_CenterCircleBy3Point( point1 = point1, point2 = point2, point3 = point3, xyzDim = xyzDim )  

   if(N_ELEMENTS(centroCircle) gt 1) then centroCircle[1] = xyzDim[1] - centroCircle[1]
   return, centroCircle
end