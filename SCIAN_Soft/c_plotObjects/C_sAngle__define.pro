;_____________________________IOISIOI____________________
; NAME:
;      C_sAngle
;
; PURPOSE:
;       - Angle Object
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = Obj_New('C_sAngle' )
;
; METHOHDS:
;	PRO					->draw, pImageData = pImageData							   ;pImageData 			   Pointer on Image Data Matrix
;	PRO					->SetProperty, xcenter=x, ycenter=y, r=r, color = color			; xcenter: xcenter-Position of centre,  ycenter: ycenter-Position of centre,  r: radius, color:colorindex
;	PRO					->GetProperty, xcenter=x, ycenter=y, r=r, color = color			; xcenter: xcenter-Position of centre,  ycenter: ycenter-Position of centre,  r: radius, color:colorindex
;_____________________________IOISIOI____________________


pro C_sAngle::SetProperty, xcenter = xcenter, ycenter = ycenter, color = color, xalpha1 = xalpha1, yalpha1 = yalpha1,$
 																						       xalpha2 = xalpha2, yalpha2 = yalpha2
   if (n_elements(xcenter) ne 0) then self.xcenter = xcenter
   if (n_elements(ycenter) ne 0) then self.ycenter = ycenter
   if (n_elements(color) ne 0) then self.color = color
   if (n_elements(xalpha1) ne 0)  then begin
   		self.xalpha1 = xalpha1
   		self.xalpha2 = xalpha1
   	endif
   if (n_elements(yalpha1) ne 0) then begin
   		self.yalpha1 = yalpha1
   		self.yalpha2 = yalpha1
	    self.r = ( SQRT( (1.*self.xcenter -  1.*self.xalpha1)^2 +  (1.*self.ycenter - 1.*self.yalpha1)^2 ) )
		if ((self.xalpha1-self.xcenter) EQ 0) then $
			if((self.yalpha1-self.ycenter) GE 0) then self.alpha1 = 90.  * !dtor else self.alpha1 = 270. * !dtor

		if ((self.yalpha1-self.ycenter) EQ 0) then $
			if((self.xalpha1-self.xcenter) GE 0) then self.alpha1 = 0.  * !dtor else self.alpha1 = 180. * !dtor

		if ((self.xalpha1-self.xcenter) GT 0) then $
			self.alpha1 = (( 360 + ( asin(  (self.yalpha1-self.ycenter)/(1.*self.r)   ) /!dtor ) ) MOD 360 )  * !dtor

		if ((self.xalpha1-self.xcenter) LT 0) then $
			self.alpha1 =  (( 180 - ( asin(  (self.yalpha1-self.ycenter)/(1.*self.r)   ) /!dtor ) ) MOD 360 )  * !dtor

		 self.memalpha2 =  self.alpha1
	endif
   if (n_elements(xalpha2) ne 0) then self.xalpha2 = xalpha2 else self.xalpha2 = self.xalpha1
   if (n_elements(yalpha2) ne 0)  then self.yalpha2 = yalpha2 else self.yalpha2 = self.yalpha2

 	r = ( SQRT( (1.*self.xcenter -  1.*self.xalpha2)^2 +  (1.*self.ycenter - 1.*self.yalpha2)^2 ) )

	if ((self.xalpha2-self.xcenter) EQ 0) then $
		if((self.yalpha2-self.ycenter) GE 0) then self.alpha2 = 90.  * !dtor else self.alpha2 = 270. * !dtor

	if ((self.yalpha2-self.ycenter) EQ 0) then $
		if((self.xalpha2-self.xcenter) GE 0) then self.alpha2 = 0.  * !dtor else self.alpha2 = 180. * !dtor

	if ((self.xalpha2-self.xcenter) GT 0) then $
		self.alpha2 = (( 360 + ( asin(  (self.yalpha2-self.ycenter)/(1.*r)   ) /!dtor ) ) MOD 360 )  * !dtor

	if ((self.xalpha2-self.xcenter) LT 0) then $
		self.alpha2 =  (( 180 - ( asin(  (self.yalpha2-self.ycenter)/(1.*r)   ) /!dtor ) ) MOD 360 )  * !dtor

end

pro C_sAngle::GetProperty, xcenter=x, ycenter=y, r=r, color = color, alpha1 = alpha1, xalpha1 = xalpha1, yalpha1 = yalpha1,$
 																								  	  alpha2 = alpha2, xalpha2 = xalpha2, yalpha2 = yalpha2
    xcenter= self.xcenter
    ycenter= self.ycenter
    r= self.r
    color = self.color
    alpha1 = self.alpha1 /!dtor
    xalpha1 = self.xalpha1
    yalpha1 = self.yalpha1
    alpha2 = self.alpha2 /!dtor
    xalpha2 = self.xalpha2
    yalpha2 = self.yalpha2
end

pro C_sAngle::GetRadialIntensities, pImageData = pImageData,$
													 pXYDistMatrix = pXYDistMatrix,$
													 pRadMatrix = pRadMatrix,$
													 xyRealSize = xyRealSize,$
													 count=count, rad=rad

    a = Findgen(365) * !dtor
	imageDim = size(*pImageData, /dimension)
	xPixSize = 1. * xyRealSize[0] / imageDim[0]
	yPixSize = 1. * xyRealSize[1] / imageDim[1]

	if (Abs(self.memalpha2 - self.alpha2) GT 1.5) then self.fdirection = 1 - self.fdirection

	if (self.fdirection) then asub = Where( ((a GE  (self.alpha1< self.alpha2 )) * (a LE  (self.alpha1> self.alpha2 )))	EQ 0, count) $
		else asub = Where( ((a GE  (self.alpha1< self.alpha2 )) * (a LE  (self.alpha1> self.alpha2 )))	EQ 1, count)

		if (count GT 0) then begin

		pRadMatrix = Ptr_New(FltArr(count, Round(self.r)), /no_copy)
		pXYDistMatrix  = Ptr_New(FltArr(count, Round(self.r)), /no_copy)

		for radcount = 0, count-1 do begin

				xstep = (self.r*cos(a(asub(radcount))) ) / self.r
				ystep = (self.r*sin(a(asub(radcount))) ) / self.r

				xrun =  self.xcenter
				yrun =  self.ycenter

				IntensityVector = (*pImageData)[Round(xrun)>0, Round(yrun)>0]
				distVector = 0.0

				xrun = xrun + xstep
				yrun = yrun + ystep

				while  (sqrt((xrun-self.xcenter)^2+(yrun-self.ycenter)^2) LE self.r) do begin

					if ((xrun gt 0) and (yrun gt 0) and (xrun lt imageDim[0]) and (yrun lt imageDim[1])) then begin
	 					IntensityVector = [IntensityVector, (*pImageData)[xrun, yrun]]
	 					distVector = [distVector, sqrt((  (xrun-self.xcenter)*xPixSize  )^2+(  (yrun-self.ycenter)*xPixSize )^2) ]
 					endif else begin
 						IntensityVector = [IntensityVector, -1]
 						distVector = [distVector, -1.]
 					endelse

					xrun = xrun + xstep
					yrun = yrun + ystep
				endwhile

				IntensityVector = Congrid(IntensityVector, Round(self.r), /MINUS_ONE)
				distVector = Congrid(distVector, Round(self.r), /MINUS_ONE)

				(*pRadMatrix)[radcount,*] = IntensityVector
				(*pXYDistMatrix)[radcount,*] = distVector

		endfor
	endif
   	self.memalpha2 = self.alpha2
   	rad = self.r
end

pro C_sAngle::cleanup
end

pro C_sAngle::draw
    if !d.window lt 0 then window,/free

    a = Findgen(365) * !dtor

	if (Abs(self.memalpha2 - self.alpha2) GT 1.5) then self.fdirection = 1 - self.fdirection

	if (self.fdirection) then asub = Where( ((a GE  (self.alpha1< self.alpha2 )) * (a LE  (self.alpha1> self.alpha2 )))	EQ 0, count) $
		else asub = Where( ((a GE  (self.alpha1< self.alpha2 )) * (a LE  (self.alpha1> self.alpha2 )))	EQ 1, count)

		if (count GT 0) then begin

		    plots, self.xcenter + self.r*cos(a(asub)),$
		    		  self.ycenter + self.r*sin(a(asub)),$
		    		  color = self.color,$
		    		   /device

		    plots, [self.xcenter , self.xcenter + self.r*cos(self.alpha1)],$
		    		  [self.ycenter , self.ycenter + self.r*sin(self.alpha1)],$
		    		  color = self.color,$
		    		   /device

	    endif
	    plots, [self.xcenter , self.xcenter + self.r*cos(self.alpha2)],$
	    		  [self.ycenter , self.ycenter + self.r*sin(self.alpha2)],$
	    		  color = self.color,$
	    		   /device

    	self.memalpha2 = self.alpha2
end

function C_sAngle::init, xcenter=xcenter, ycenter=ycenter, r=r, color = color, alpha1 = alpha1, xalpha1 = xalpha1, yalpha1 = yalpha1,$
 																									 alpha2 = alpha2, xalpha2 = xalpha2, yalpha2 = yalpha2,$
 																									 fdirection =  fdirection, memalpha2 = memalpha2
    if keyword_set(xcenter) then self.xcenter = xcenter
    if keyword_set(ycenter) then self.ycenter = ycenter
    if keyword_set(r) then self.r = r
    if keyword_set(color) then self.color = color
    if keyword_set(alpha1) then self.alpha1 = alpha1
    if keyword_set(xalpha1) then self.xalpha1 = xalpha1
    if keyword_set(yalpha1) then self.yalpha1 = yalpha1
    if keyword_set(alpha2) then self.alpha2 = alpha2
    if keyword_set(xalpha2) then self.xalpha2 = xalpha2
    if keyword_set(yalpha2) then self.yalpha2 = yalpha2
    if keyword_set(fdirection) then self.fdirection = fdirection
    if keyword_set(memalpha2) then self.memalpha2 = memalpha2

    self.fdirection = 0
    return, 1
end

pro C_sAngle__define
   tmp = { C_sAngle, xcenter:0., ycenter:0., r:0., color:0., alpha1:0., xalpha1:0., yalpha1:0.,$
      													 			alpha2:0., xalpha2:0., yalpha2:0., fdirection:0, memalpha2:0.}
end
