;_____________________________IOISIOI____________________
; NAME:
;      C_sArc
;
; PURPOSE:
;       - Arc Object
;
; AUTHOR:
;     Hector Moraga (2011)
;     e_mail: hmoraga@med.uchile.cl
;
; CALLING SEQUENCE:
;        result = Obj_New('C_sArc' )
;
; METHOHDS:
;  PRO               ->draw, pImageData = pImageData                       ;pImageData             Pointer on Image Data Matrix
;  PRO               ->SetProperty, x=x, y=y, r=r, initialAngle = initialAngle, angle = angle, color = color
;                                                                                      ; x: x-Position of centre,  y: y-Position of centre, r: radius, 
;                                                                                      ; initialAngle: angle respect the horizontal, angle: arc angle to draw starting from initialAngle,
;                                                                                      ; color:colorindex
;  PRO               ->GetProperty, x=x, y=y, r=r, initialAngle = initialAngle, angle = angle, color = color
;                                                                                      ; x: x-Position of centre,  y: y-Position of centre, r: radius, 
;                                                                                      ; initialAngle: angle respect the horizontal, angle: arc angle to draw starting from initialAngle,
;                                                                                      ; color:colorindex
;_____________________________IOISIOI____________________


pro C_sArc::SetProperty, x=x, y=y, r=r, initialAngle = initialAngle, angle = angle, color = color, lineType = lineType
   if (n_elements(x) ne 0) then self.x = x
   if (n_elements(y) ne 0) then self.y = y
   if (n_elements(r) ne 0) then self.r = r
   if (n_elements(initialAngle) ne 0) then self.initialAngle = initialAngle
   if (n_elements(angle) ne 0) then self.angle = angle
   if (n_elements(color) ne 0) then self.color = color
   if (n_elements(lineType) ne 0) then self.lineType = lineType
end

pro C_sArc::GetProperty, x=x, y=y, r=r, initialAngle = initialAngle, angle = angle, color = color, lineType = lineType
   x = self.x
   y = self.y
   r = self.r
   initialAngle = self.initialAngle
   angle = self.angle
   color = self.color
   lineType = self.lineType
end

pro C_sArc::cleanup
end

pro C_sArc::draw
    if !d.window lt 0 then window,/free
    a = (findgen(self.angle-self.initialAngle+1)+self.initialAngle)*!dtor
    plots, self.x + self.r*cos(a),$
           self.y + self.r*sin(a),$
           color = self.color,$
           linestyle = self.lineType,$
           /device

end

function C_sArc::init, x=x, y=y, r=r, initialAngle = initialAngle, angle = angle, color = color, lineType = lineType
    if keyword_set(x) then self.x = x
    if keyword_set(y) then self.y = y
    if keyword_set(r) then self.r = r
    if keyword_set(initialAngle) then self.initialAngle = initialAngle
    if keyword_set(angle) then self.angle = angle
    if keyword_set(color) then self.color = color
    if keyword_set(lineType) then self.lineType = lineType
    return, 1
end

pro C_sArc__define
   tmp = { C_sArc, x:0., y:0., r:0., initialAngle:0., angle:0., color:0, lineType:0}
end
