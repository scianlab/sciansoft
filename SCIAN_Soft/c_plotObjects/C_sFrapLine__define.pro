;_____________________________IOISIOI____________________
; NAME:
;      C_sFrapLine
;
; PURPOSE:
;       - Frap Line Objects
;
; AUTHOR:
;     Maria Osorio-Reich (2011)
; 
;
; CALLING SEQUENCE:
;        result = Obj_New('C_sFrapLine' )
;
; METHOHDS:
;   based on C_sCircle class
;  PRO               ->draw, pImageData = pImageData                       ;pImageData             Pointer on Image Data Matrix
;  PRO               ->SetProperty, x=x, y=y, r=r, color = color        ; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;  PRO               ->GetProperty, x=x, y=y, r=r, color = color        ; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;_____________________________IOISIOI____________________


pro C_sFrapLine::SetProperty, x=x, y=y, r=r, dr = dr, color = color, angle = angle
   if (n_elements(x) ne 0) then self.x = x
   if (n_elements(y) ne 0) then self.y = y
   if (n_elements(r) ne 0) then self.r = r
   if (n_elements(dr) ne 0) then self.dr = dr
   if (n_elements(color) ne 0) then self.color = color
   if (n_elements(angle) ne 0) then self.angle = angle
end

pro C_sFrapLine::GetProperty, x=x, y=y, r=r, dr = dr, color = color, angle = angle
    x= self.x
    y= self.y
    r= self.r
    dr= self.dr
    color = self.color
    angle = self.angle
end

pro C_sFrapLine::GetConcentricIntensities, pImageData = pImageData,$
                                        pXYDistMatrix = pXYDistMatrix,$
                                        pRadMatrix = pRadMatrix,$
                                        xyRealSize = xyRealSize,$
                                        count=count, rad = rad, xc = xc, yc = yc, dr = dr, ang = ang, pMaskData = pMaskData, useMask = useMask
   ang = self.angle                                     
   a = Findgen(360.) * !dtor
   imageDim = size(*pImageData, /dimension)
   xPixSize = 1. * xyRealSize[0] / imageDim[0]
   yPixSize = 1. * xyRealSize[1] / imageDim[1]
   
   count = n_elements(a) ; all the angles for which to find intensity
   ddr = self.r/self.dr ; increments between circles
   DR = ddr # make_array(self.dr+1,/index) 
   xR = self.x + DR ; beginning x-coord of next circle
   yR = self.y + DR ; beginning y-coord of next circle
   
   if (count gt 0) then begin
      ; matrices holding data for each concentric circle, for all angles and the radius vector of that circle intensity
       pRadMatrix = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
       pXYDistMatrix  = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
      
      for radcount = 0, count-1 do begin

            xstep = (self.r*cos(a(radcount)) ) / self.r
            ystep = (self.r*sin(a(radcount)) ) / self.r

            xrun =  self.x
            yrun =  self.y
            
            ; 10 March 2011 - pass in mask to limit where we get intensities from in the image  
            if(useMask) then begin 
              IntensityVector = (*pImageData)[Round(xrun)>0, Round(yrun)>0] * ((*pMaskData)[Round(xrun)>0, Round(yrun)>0])
            endif else  begin 
              IntensityVector = (*pImageData)[Round(xrun)>0, Round(yrun)>0]
            endelse
             
            distVector = 0.0

            xrun += xstep
            yrun += ystep
            

            while  (sqrt((xrun-self.x)^2+(yrun-self.y)^2) le self.r ) do begin
              ; add check to see if going to be positive or negative sign for the distance
               if ((xrun gt 0) and (yrun gt 0) and (xrun lt imageDim[0]) and (yrun lt imageDim[1])) then begin
                  if(useMask) then begin ; 11Mar2011
                    IntensityVector = [IntensityVector, (*pImageData)[xrun, yrun]*((*pMaskData)[xrun, yrun])]
                  endif else begin
                    IntensityVector = [IntensityVector, (*pImageData)[xrun, yrun]]
                  endelse                                
                  distVector = [distVector, sqrt((  (xrun-self.x)*xPixSize  )^2+(  (yrun-self.y)*xPixSize )^2) ]
;                  distVector = roundpv(distVector,0) ; MOR - round - 1.0001 -> 1.0000 12 Apr 2011
               endif else begin
                  IntensityVector = [IntensityVector, -1]
                  distVector = [distVector, -1.]
               endelse

               xrun += xstep
               yrun += ystep
            endwhile
;            print, 'I before congrid: ', IntensityVector
;            print, 'distance before congrid: ', distVector
            IntensityVector = Congrid(IntensityVector, Round(self.r), /MINUS_ONE)
;            print, 'distVector b4 rounding: ', distVector
            distVector = Congrid(distVector, Round(self.r), /MINUS_ONE)
;            print, 'I after congrid: ', IntensityVector
;            print, 'Number of points found for angle = : ', n_elements(IntensityVector), a(radcount) / !dtor
;            print, 'distance after congrid: ', distVector
;            
            (*pRadMatrix)[radcount,*] = IntensityVector
            (*pXYDistMatrix)[radcount,*] = distVector
            
         endfor ; end radcount
   endif ; end if count gt 0
      rad = self.r
      dr = self.dr
      xc = self.x
      yc = self.y
end
pro C_sFrapLine::GetSectionIntensities, rad1 = rad1, rad2 = rad2, ang1 = ang1, ang2 = ang2, Imatrix = Imatrix, intensities = intensities
    ; function that returns intensities only in specified polar coordinates
    ; create mask to only extract intensities asked for
    ; assume Imatrix comes in x,y ??? 
    ; intensities = make_array((rad2-rad1), (ang2-ang1))
    ; find pixels which are less than (lt) rad2 and greater than or equal to (ge) rad1
    strucElem = (shift(dist(2*rad2+1), rad2, rad2) lt rad2) and (shift(dist(2*rad2+1), rad2, rad2) ge rad1)  
    ; make a mask of this in image dimensions - need to center it in the image
    dimI = size(Imatrix,/dim)
    circMask = bytarr(dimI)
    circMask[self.x-self.r, self.y-self.r] = strucElem 
    
end

function C_sFrapLine::GetIntegral, h = h, fx = fx
    ; set up for integral on tabulated data
    ; assumes fx is evaluated on x in ascending order and evenly spaced
    ; assumes you want a define integral evaluated on xmin through xmax, where x[0] = xmin and x[end] = xmax
    ; composite trapezoidal rule used for integration
    
    n = n_elements(fx)-1;
    result = h*(fx(0) + 2* total(fx(1:n)) + fx(n) )/2
    return, result
end

pro C_sFrapLine::Get2DIntegral, x = x, fx = fx, y = y, result = result
    ; set up for integral on tabulated data
    ; assumes x is in ascending order and evenly spaced
    ; assumes you want a define integral evaluated on xmin through xmax, where x[0] = xmin and x[end] = xmax
    
    tempFy = int_tabulated(x, fx)
    result = int_tabulated(y,tempFy)
    
   ; return, result
end

pro C_sFrapLine::cleanup
end

pro C_sFrapLine::draw
    if !d.window lt 0 then window,/free
    
    a=findgen(365.)*!dtor
       
    ; determine number of circles that need to be drawn, ddr
    ; total radius (r) divided by number of subcircles (dr)
    ddr = 1.
    DR = make_array(self.dr+1, /index, /float)
    ddr = 1.*self.r/self.dr
    DRx = ddr * DR
    ; points for lines
    a = (self.angle * !dtor) mod (2 * !pi)
    b = (self.angle * !dtor + !pi) mod (2*!pi)
    x1 = self.x + self.r*cos(a)
    x2 = self.x + self.r*cos(b)
    y1 = self.y + self.r*sin(a)
    y2 = self.y + self.r*sin(b)

    plots, [x1, x2], [y1, y2], color = self.color, thick = 2.0, /device
    ;plots, [self.x, self.x], [y1, y2],color = self.color, thick = 2.0, /device

end
pro C_sFrapLine::getArea, rad1 = rad1, rad2 = rad2, ang1 = ang1, ang2 = ang2, area = area
    ; calculates area comprised by two rays and two angles
    ; make sure that the angles and radius are ordered
    ; convert angles from degrees to radians
    ; make sure radius are positive
    angle1 = min([ang1, ang2], max = angle2)
    radius1 = min([rad1, rad2], max = radius2)
    ; area = 0.5*(R2 + R1)*deltaR*deltaTheta 
    area = 0.5 * (radius2 + radius1) * (radius2-radius1) * ((angle2*!dtor) - (angle1*!dtor))
   ; return, area
end

function C_sFrapLine::init, x=x, y=y, r=r, dr = dr, color = color, angle = angle
    if keyword_set(x) then self.x = x
    if keyword_set(y) then self.y = y
    if keyword_set(r) then self.r = r
    if keyword_set(dr) then self.dr = dr
    if keyword_set(color) then self.color = color
    if keyword_set(angle) then self.angle = angle
    if keyword_set(anywhere) then self.anywhere = 1
    
    return, 1
end

pro C_sFrapLine__define
   tmp = {C_sFrapLine, x:0., y:0., r:0., dr:0., color:0,  angle:0., anywhere:1}
end

