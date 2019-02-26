;_____________________________IOISIOI____________________
; NAME:
;      C_sFrapCircle
;
; PURPOSE:
;       - Frap Circle Objects
;
; AUTHOR:
;     Maria Osorio-Reich (2011)
; 
;
; CALLING SEQUENCE:
;        result = Obj_New('C_sFrapCircle' )
;
; METHOHDS:
;   based on C_sCircle class
;  PRO               ->draw, pImageData = pImageData                       ;pImageData             Pointer on Image Data Matrix
;  PRO               ->SetProperty, x=x, y=y, r=r, color = color        ; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;  PRO               ->GetProperty, x=x, y=y, r=r, color = color        ; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;_____________________________IOISIOI____________________


pro C_sFrapCircle::SetProperty, x=x, y=y, r=r, dr = dr, color = color
   if (n_elements(x) ne 0) then self.x = x
   if (n_elements(y) ne 0) then self.y = y
   if (n_elements(r) ne 0) then self.r = r
   if (n_elements(dr) ne 0) then self.dr = dr
   if (n_elements(color) ne 0) then self.color = color
end

pro C_sFrapCircle::GetProperty, x=x, y=y, r=r, dr = dr, color = color
    x= self.x
    y= self.y
    r= self.r
    dr= self.dr
    color = self.color
end

function C_sFrapCircle::GetLine, angle

    a=findgen(365.)*!dtor
       
    ; determine number of circles that need to be drawn, ddr
    ; total radius (r) divided by number of subcircles (dr)
    ddr = 1.
    DR = make_array(self.dr+1, /index, /float)
    ddr = 1.*self.r/self.dr
    DRx = ddr * DR
    ; points for lines
    a = (angle * !dtor) mod (2 * !pi)
    b = (angle * !dtor + !pi) mod (2*!pi)
    x1 = self.x + self.r*cos(a)
    x2 = self.x + self.r*cos(b)
    y1 = self.y + self.r*sin(a)
    y2 = self.y + self.r*sin(b)
    
   return, [x1,y1,x2,y2]
end

pro C_sFrapCircle::GetConcentricIntensities, pImageData = pImageData,$
                                        pXYDistMatrix = pXYDistMatrix,$
                                        left = left,$
                                        right = right,$
                                        pRadMatrix = pRadMatrix,$
                                        intLeft = intLeft,$
                                        intRight = intRight,$
                                        xyRealSize = xyRealSize,$
                                        count=count, rad = rad, xc = xc, yc = yc, dr = dr, $
                                        useMask = useMask, pMaskData = pMaskData, angle = angle

  
   a = Findgen(360.) * !dtor
   imageDim = size(*pImageData, /dimension)
   xPixSize = 1. * xyRealSize[0] / imageDim[0]
   yPixSize = 1. * xyRealSize[1] / imageDim[1]
   
   count = n_elements(a) ; all the angle for which to find intensity
   ddr = self.r/self.dr ; increments between circles
   DR = ddr # make_array(self.dr+1,/index) 
   xR = self.x + DR ; beginning x-coord of next circle
   yR = self.y + DR ; beginning y-coord of next circle
  
   
   if (count gt 0) then begin
      ; matrices holding data for each concentric circle, for all angles and the radius vector of that circle intensities
       pRadMatrix = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
       pXYDistMatrix  = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
       intLeft = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
       left  = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
       intRight = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
       right  = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
       
      ; radcount is 0 thru 2pi
      for radcount = 0, count-1 do begin
            
            ; stepping through grid in order to obtain image intensities
            xstep = (self.r*cos(a(radcount)) ) / self.r
            ystep = (self.r*sin(a(radcount)) ) / self.r

            xrun =  self.x
            yrun =  self.y

            ;IntensityVector = (*pImageData)[Round(xrun)>0, Round(yrun)>0]
            ;10 March 2011 - pass in mask to limit where we get intensities from in the image  
            if(useMask) then begin
              IntensityVector = (*pImageData)[Round(xrun)>0, Round(yrun)>0] * ((*pMaskData)[Round(xrun)>0, Round(yrun)>0])
            endif else  begin 
              IntensityVector = (*pImageData)[Round(xrun)>0, Round(yrun)>0]
            endelse
            distVector = 0.0
            distVectorL = 0.0
            distVectorR = 0.0
            
            xrun += xstep
            yrun += ystep
            
            while  (sqrt((xrun-self.x)^2+(yrun-self.y)^2) le self.r ) do begin

               if ((xrun gt 0) and (yrun gt 0) and (xrun lt imageDim[0]) and (yrun lt imageDim[1])) then begin
                  if(useMask) then begin
                    IntensityVector = [IntensityVector, (*pImageData)[xrun, yrun]*((*pMaskData)[xrun, yrun])]
                  endif else begin
                    IntensityVector = [IntensityVector, (*pImageData)[xrun, yrun]]
                  endelse
                  distVector = [distVector, sqrt((  (xrun-self.x)*xPixSize  )^2+(  (yrun-self.y)*xPixSize )^2) ]
               endif else begin
                    IntensityVector = [IntensityVector, -1]
                    distVector = [distVector, -1.]
               endelse

               xrun += xstep
               yrun += ystep
            endwhile
            
                IntensityVector = Congrid(IntensityVector, Round(self.r), /MINUS_ONE)
                distVector = Congrid(distVector, Round(self.r), /MINUS_ONE)
            
               (*pRadMatrix)[radcount,*] = IntensityVector
               (*pXYDistMatrix)[radcount,*] = distVector
                        
         endfor ; end radcount
   endif ; end if count gt 0
      rad = self.r
      dr = self.dr
      xc = self.x
      yc = self.y
end

pro C_sFrapCircle::GetDistanceMap, pImageData = pImageData,$
                                        count=count, rad = rad, xc = xc, yc = yc, dr = dr, $
                                        useMask = useMask, pMaskData = pMaskData,$
                                        pDistMatrix = pDistMatrix, distMaskFull = distMaskFull, tPos = tPos,$
                                        intRad = intRad, intCon = intCon , intCount = intCount, $
                                        distMaskIntervals = distMaskIntervals
                                        
    dr = self.dr                                  
    dimI = size(*pMaskData,/dim)
    circMask = bytarr(dimI)
    distMaskFull = fltarr(dimI)
    distMaskIntervals = fltarr(dimI)

     ; determine distance from 2sig FRAP gaussian fit border
      circMask = self->GetCircle(rad1 = self.r,pMaskData = *pMaskData)
      ; in each pixel in the segmentation, include a distance to border to be identified along with the intensity in that region        
      *pDistMatrix = self->GetDistances(pMaskData = *pMaskData, circMask = circMask, rad1 = 0, rad2 =self.r, pDistMatrix = *pDistMatrix)
      ; take the longest distance and divide by the number of intervals specified by the user
      ddr = max(self.r)/self.dr
      R = make_array(self.dr+1, /index, /float)
      DRx = ddr * R
      intCon =  make_array(self.dr)
      intRad =  make_array(self.dr)
      intCount = make_array(self.dr, /integer)
      
      
;      ht= histogram((*pDistMatrix)[*,*], min = 0, max = max((*pDistMatrix)[*,*]), nbins =max((*pDistMatrix)[*,*]) , locations = locsB, reverse_indices = rI)
      ht = histogram((*pDistMatrix)[*,*], min = 0, omax = maxH, locations = locsB, reverse_indices = rI)

      for j  = 0,n_elements(ht)-1 do begin  
        if rI[j] ne rI[j+1] then begin 
          distMaskFull[rI[rI[j] : rI[j+1]-1]] = (*pDistMatrix)[rI[rI[j] : rI[j+1]-1]]
        endif 
      endfor
      
      
      ht = histogram((*pDistMatrix)[*,*], min = 0, max = ceil(self.r), binsize = ddr, locations = locsB, reverse_indices = rI)
      for j  = 0,n_elements(DRx)-2 do begin 
        if rI[j] ne rI[j+1] then begin 
          distMaskIntervals[rI[rI[j] : rI[j+1]-1]] = mean(DRx[j:j+1])
        endif 
      endfor
           
      
      for j = 0, n_elements(DRx) - 2 do begin
        whDistRadBin = where(((*pDistMatrix)[*,*] ge DRx[j]) and ((*pDistMatrix)[*,*] lt DRx[j+1]) and (*pMaskData ne 0),ct,/L64)
        if (ct gt 0) then begin
            ; get the pixel coordinates of the match
            ind = array_indices((*pDistMatrix)[*,*], whDistRadBin)
            intCon[j] = mean((*pImageData)[ind[0,*],ind[1,*]])
            intRad[j] =  mean( DRx[j : j+1] )
            intCount[j] = ct
        endif 
      endfor
      
      window, 10, title = 'mean intensities', xsize = 400, ysize = 300
      plot,  intRad, intCon, psym = 6,xtitle = 'distance from FRAP border [pix]', ytitle = 'mean intensity'
                                 
end

pro C_sFrapCircle::GetIntensities, ImageData = ImageData,$
                                        rad = rad, xc = xc, yc = yc, dr = dr, $
                                        MaskData = MaskData,$
                                        DistMatrix = DistMatrix, distMaskFull = distMaskFull, $
                                        intRad = intRad, intCon = intCon , intCount = intCount, $
                                        distMaskIntervals = distMaskIntervals
                                        
    dr = self.dr                                  
    dimI = size(MaskData,/dim)
    circMask = bytarr(dimI)
    distMaskFull = fltarr(dimI)
    distMaskIntervals = fltarr(dimI)

     ; determine distance from 2sig FRAP gaussian fit border
;      circMask = self->GetCircle(rad1 = self.r,pMaskData = *pMaskData)
      ; in each pixel in the segmentation, include a distance to border to be identified along with the intensity in that region        
      DistMatrix = self->GetDistances(pMaskData = MaskData, circMask = circMask, rad1 = 0, rad2 =self.r, pDistMatrix = DistMatrix)
      ; take the longest distance and divide by the number of intervals specified by the user
      ddr = max(self.r)/self.dr
      R = make_array(self.dr+1, /index, /float)
      DRx = ddr * R
      intCon =  make_array(self.dr)
      intRad =  make_array(self.dr)
      intCount = make_array(self.dr, /integer)
      
     
      for j = 0, n_elements(DRx) - 2 do begin
        whDistRadBin = where((DistMatrix[*,*] ge DRx[j]) and (DistMatrix[*,*] lt DRx[j+1]) and (MaskData ne 0),ct,/L64)
        if (ct gt 0) then begin
            ; get the pixel coordinates of the match
            ind = array_indices(DistMatrix[*,*], whDistRadBin)
            intCon[j] = mean(ImageData[ind[0,*],ind[1,*]])
            intRad[j] =  mean( DRx[j : j+1] )
            intCount[j] = ct
        endif 
      endfor
      
                                 
end

function C_sFrapCircle::GetDisks, rad1 = rad1, rad2 = rad2, pMaskData = pMaskData 

    ; check the order of the radius size being passed in
;    rad2 = max(rad1, rad2)
;    rad1 = min(rad1, rad2)

    ; define the disk structures -- >= rad1 and <= rad2
    strucElem = (shift(dist(2*rad2+1), rad2, rad2) le rad2) and (shift(dist(2*rad2+1), rad2, rad2) ge rad1)  
    ; make a mask of this in image dimensions - need to center it in the image
    dimI = size(pMaskData,/dim)
    circMask = bytarr(dimI)
    circMask[self.x-self.r, self.y-self.r] = strucElem 
    
    return, circMask

end

function C_sFrapCircle::GetCircle, rad1 = rad1, pMaskData = pMaskData 

    ; define the disk structures 
    strucElem = (shift(dist(2*rad1+1), rad1, rad1) le rad1)
    ; make a mask of this in image dimensions - need to center it in the image
    dimI = size(pMaskData,/dim)
    circMask = bytarr(dimI)
    circMask[self.x-self.r, self.y-self.r] = strucElem 
    
    return, circMask

end

function C_sFrapCircle::GetDistances, pMaskData = pMaskData, circMask = circMask, rad1 = rad1, rad2 = rad2, pDistMatrix = pDistMatrix

    ; check the order of the radius size being passed in
;    rad2 = floor(max(rad1, rad2))
;    rad1 = floor(min(rad1, rad2))

    ; find only the points inside the radius of interest and the segmentation for further analysis
    whSeg = where((pMaskData ne 0) * (circMask ne 0), ctSeg, /l64)
    ; define the center of the circle as a point to form a vector  
    P = floor([self.x, self.y])
    ; make a vector of all the distances of interest
    ; creates a vector [rad1, rad2) in increments of 1
    ;rad = make_array(rad2 - rad1, /integer, /index) + rad1
    rad = rad2;
    
    if (whSeg[0] ne -1) then begin
      indSeg = array_indices(pMaskData, whSeg)
      ; for every element in the segmentation, get the distance to each radius
     
      for j = long(0), ctSeg-1 do begin
        ; for k = 0, n_elements(rad)-1 do begin
            Q = [indSeg[0,j],indSeg[1,j]]
            V = Q - P ; vector with direction towards the border
            ; currently using the Euclidean distance, check results for L-1 (taxicab) norm
;            U = round(P + V / round(norm(V,lnorm =2)) * rad[k])
;            pDistMatrix[indSeg[0,j],indSeg[1,j]] = rad[k] - norm(P-U) 
            ;U = P + (V / norm(V,lnorm =2) )* rad
            ;pDistMatrix[indSeg[0,j],indSeg[1,j], tPos] = rad - total((Q-P)*((U-P)/rad))
            pDistMatrix[indSeg[0,j],indSeg[1,j]] = rad - total((V)*(V/norm(V)))
            ;print, pDistMatrix[indSeg[0,j],indSeg[1,j], tPos]          
         ;endfor
      endfor
    endif
    
    return, pDistMatrix

end



pro C_sFrapCircle::cleanup
end

pro C_sFrapCircle::draw
    if !d.window lt 0 then window,/free
    
    a=findgen(365.)*!dtor
       
    ; determine number of circles that need to be drawn, ddr
    ; total radius (r) divided by number of subcircles (dr)
    ddr = 1.
    DR = make_array(self.dr+1, /index, /float)
    ddr = 1.*self.r/self.dr
    DRx = ddr * DR 

    for j = 1, self.dr do begin 
      plots, self.x + DRx[j] * cos(a),$
           self.y + DRx[j] * sin(a),$
           color = self.color,thick = 2.0,$
            /device
    end

end

function C_sFrapCircle::init, x=x, y=y, r=r, dr = dr, color = color
    if keyword_set(x) then self.x = x
    if keyword_set(y) then self.y = y
    if keyword_set(r) then self.r = r
    if keyword_set(dr) then self.dr = dr
    if keyword_set(color) then self.color = color
    if keyword_set(anywhere) then self.anywhere = 1
    return, 1
end

pro C_sFrapCircle__define
   tmp = {C_sFrapCircle, x:0., y:0., r:0., dr:0., color:0, anywhere:1}
end

