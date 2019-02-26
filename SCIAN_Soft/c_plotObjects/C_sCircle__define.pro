;_____________________________IOISIOI____________________
; NAME:
;      C_sCircle
;
; PURPOSE:
;       - Circle Object
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = Obj_New('C_sCircle' )
;
; METHOHDS:
;	PRO					->draw, pImageData = pImageData							   ;pImageData 			   Pointer on Image Data Matrix
;	PRO					->SetProperty, x=x, y=y, r=r, color = color			; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;	PRO					->GetProperty, x=x, y=y, r=r, color = color			; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;_____________________________IOISIOI____________________


pro C_sCircle::SetProperty, x=x, y=y, r=r, color = color
   if (n_elements(x) ne 0) then self.x = x
   if (n_elements(y) ne 0) then self.y = y
   if (n_elements(r) ne 0) then self.r = r
   if (n_elements(color) ne 0) then self.color = color
end

pro C_sCircle::GetProperty, x=x, y=y, r=r, color = color
    x = self.x
    y = self.y
    r = self.r
    color = self.color
end

pro C_sCircle::cleanup
end

pro C_sCircle::draw
    if !d.window lt 0 then window,/free
    a = findgen(365.)*!dtor
    plots, self.x + self.r*cos(a),$
    		  self.y + self.r*sin(a),$
    		  color = self.color,$
    		   /device

end

function C_sCircle::init, x=x, y=y, r=r, color = color
    if keyword_set(x) then self.x = x
    if keyword_set(y) then self.y = y
    if keyword_set(r) then self.r = r
    if keyword_set(color) then self.color = color
    return, 1
end

pro C_sCircle__define
   tmp = { C_sCircle, x:0., y:0., r:0., color:0}
end
