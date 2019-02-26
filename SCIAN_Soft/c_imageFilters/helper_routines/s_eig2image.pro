;_____________________________IOISIOI____________________
; NAME:
;      s_eig2image
;
; PURPOSE:
; This function eig2image calculates the eigen values from the
; hessian matrix, sorted by abs value. And gives the direction
; of the ridge (eigenvector smallest eigenvalue)
;
; | Dxx  Dxy |
; |          |
; | Dxy  Dyy |
;
; AUTHOR:
;   Hector Moraga (2011)
;   e_mail: hmoraga@med.uchile.cl
;
;   MATLAB version written by D.Kroon University of Twente (June 2009)
;
; CALLING SEQUENCE:
;   s_eig2image ( Lambda1 = Lambda1, Lambda2 = Lambda2, Ix = Ix, Iy = Iy, Dxx = Dxx, Dxy = Dxy, Dyy = Dyy )
;   Lambda1: 
;   Lambda2:
;   Ix:
;   Iy:
;   Dxx: Second derivate in x of image
;   Dxy: Derivate in y of image and then in x of the result
;   Dxx: Second derivate in y of image
;
; METHODS:
;_____________________________IOISIOI____________________
pro s_eig2image, Dxx = Dxx, Dxy = Dxy, Dyy = Dyy, Lambda1 = Lambda1, Lambda2 = Lambda2, Ix = Ix, Iy = Iy

; Compute the eigenvectors of J, v1 and v2
   ; solo por debug esta linea
   t = (Dxx-Dyy)*(Dxx-Dyy)+4*Dxy*Dxy
   tmp = sqrt(t)
   v2x = 2*Dxy
   v2y = Dyy - Dxx + tmp
   
; calculo el valor de EPS
   res = MACHAR()

; Normalize
   mag = sqrt(v2x^2 + v2y^2)
    
   i = where(mag ne 0, count, COMPLEMENT=c_i, NCOMPLEMENT=c_count)
   if (count ne 0) then begin
      v2x[i] = v2x[i]/mag[i]
      v2y[i] = v2y[i]/mag[i]
   endif

   if (c_count ne 0) then begin
      v2x[c_i] = res.xmax
      v2y[c_i] = res.xmax
   endif
   
; The eigenvectors are orthogonal
   v1x = -v2y 
   v1y = v2x

; Compute the eigenvalues
   mu1 = 0.5*(Dxx + Dyy + tmp)
   mu2 = 0.5*(Dxx + Dyy - tmp)

   Lambda1 = make_array(size(mu1,/DIMENSIONS),VALUE=0,/DOUBLE)
   Lambda2 = make_array(size(mu2,/DIMENSIONS),VALUE=0,/DOUBLE)   
   Ix = make_array(size(v2x,/DIMENSIONS),VALUE=0,/DOUBLE)
   Iy = make_array(size(v2y,/DIMENSIONS),VALUE=0,/DOUBLE)
   
; Sort eigen values by absolute value abs(Lambda1)<abs(Lambda2)
   check=where(abs(mu1) gt abs(mu2),count, COMPLEMENT=c_check, NCOMPLEMENT=c_count)

   if (c_count ne 0) then begin
      Lambda1[c_check]=mu1[c_check]
      Lambda2[c_check]=mu2[c_check]
      Ix[c_check]=v1x[c_check]
      Iy[c_check]=v1y[c_check]
   endif
  
   if (count ne 0) then begin
      Lambda1[check]=mu2[check]
      Lambda2[check]=mu1[check]
      Ix[check]=v2x[check]
      Iy[check]=v2y[check]      
   endif

end