;_____________________________IOISIOI____________________
; NAME:
;      C_sLine
;
; PURPOSE:
;       - Square Object
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = Obj_New('C_sLine' )
;
; METHOHDS:
;	PRO					->draw, pImageData = pImageData							   ;pImageData 			   Pointer on Image Data Matrix
;	PRO					->SetProperty, x1=x1, y1=y1, x2=x2, y2=y2, color = color		; x1/2: x-Position of sqare,  y1/2: y-Position of centre, color:colorindex
;	PRO					->GetProperty,x1=x1, y1=y1, x2=x2, y2=y2, color = color			; x1/2: x-Position of sqare,  y1/2: y-Position of centre, color:colorindex
;_____________________________IOISIOI____________________


pro C_sLine::SetProperty,x1=x1, y1=y1, x2=x2, y2=y2,  color = color
   if (n_elements(x1) ne 0) then self.x1 = x1
   if (n_elements(x2) ne 0) then self.x2 = x2
   if (n_elements(y1) ne 0) then self.y1 = y1
   if (n_elements(y2) ne 0) then self.y2 = y2
   if (n_elements(color) ne 0) then self.color = color
end

pro C_sLine::GetProperty, x1=x1, y1=y1, x2=x2, y2=y2,  color = color
    x1= self.x1
    x2= self.x2
    y1= self.y1
    y2= self.y2
    color = self.color
end

pro C_sLine::cleanup
end

pro C_sLine::draw
    if !d.window lt 0 then window,/free
    plots, [self.x1, self.x1, self.x2, self.x2, self.x1],$
    	      [self.y1, self.y2, self.y2, self.y1, self.y1],$
    		  color = self.color,$
 		       /Device
end

function C_sLine::init, x1=x1, y1=y1, x2=x2, y2=y2,  color = color
    if keyword_set(x) then self.x1 = x1
    if keyword_set(x) then self.x2 = x2
    if keyword_set(y) then self.y1 = y1
    if keyword_set(y) then self.y2 = y2
    if keyword_set(color) then self.color = color
    return, 1
end

pro C_sLine__define
   tmp = { C_sLine, x1:0, y1:0, x2:0, y2:0, color:0}
end
