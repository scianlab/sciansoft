;_____________________________IOISIOI____________________
; NAME:
;      C_sCross
;
; PURPOSE:
;       - Cross Object
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;		03Feb2011- MOR added a thickness to the crosshair lines
; CALLING SEQUENCE:
;      	result = Obj_New('C_sCross' )
;
; METHOHDS:
;	PRO					->draw, pImageData = pImageData							   ;pImageData 			   Pointer on Image Data Matrix
;	PRO					->SetProperty, x1=x1, y1=y1, x2=x2, y2=y2, color = color		; x1/2: x-Position of Cross,  y1/2: y-Position of Cross, color:colorindex
;	PRO					->GetProperty,x1=x1, y1=y1, x2=x2, y2=y2, color = color			; x1/2: x-Position of Cross,  y1/2: y-Position of Cross, color:colorindex
;_____________________________IOISIOI____________________


pro C_sCross::SetProperty,x1=x1, y1=y1, x2=x2, y2=y2,  color = color
   if (n_elements(x1) ne 0) then self.x1 = x1
   if (n_elements(x2) ne 0) then self.x2 = x2
   if (n_elements(y1) ne 0) then self.y1 = y1
   if (n_elements(y2) ne 0) then self.y2 = y2
   if (n_elements(color) ne 0) then self.color = color
end

pro C_sCross::GetProperty, x1=x1, y1=y1, x2=x2, y2=y2,  color = color
    x1= self.x1
    x2= self.x2
    y1= self.y1
    y2= self.y2
    color = self.color
end

pro C_sCross::cleanup
end

pro C_sCross::draw
    if !d.window lt 0 then window,/free

    plots, [self.x1, self.x1],$
    	      [(self.y1)-Round((self.y1-self.y2)/2.),(self.y1)+Round((self.y1-self.y2)/2.)],$
    		  color = self.color,thick = 2.0,$
			   /Device
    plots, [(self.x1)-Round((self.x1-self.x2)/2.),(self.x1)+Round((self.x1-self.x2)/2.)],$
    	      [self.y1,self.y1],$
    		  color = self.color,thick = 2.0,$
			   /Device
end

function C_sCross::init, x1=x1, y1=y1, x2=x2, y2=y2,  color = color
    if keyword_set(x) then self.x1 = x1
    if keyword_set(x) then self.x2 = x2
    if keyword_set(y) then self.y1 = y1
    if keyword_set(y) then self.y2 = y2
    if keyword_set(color) then self.color = color
    return, 1
end

pro C_sCross__define
   tmp = { C_sCross, x1:0, y1:0, x2:0, y2:0, color:0}
end
