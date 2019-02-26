;_____________________________IOISIOI____________________
; NAME:
;   s_hessian2D
;
; PURPOSE:
;   Calculate Hessian 2D Matrix of an image.
;
; AUTHOR:
;   Hector Moraga (2011)
;   e_mail: hmoraga@med.uchile.cl
;
;   MATLAB version written by D.Kroon University of Twente (June 2009)
;
; CALLING SEQUENCE:
;   result = s_hessian2D(image = image, desv = desv, Dxx = Dxx, Dxy = Dxy, Dyy = Dyy)
;
;   image: The input image
;   Desv : The sigma of the gaussian kernel used 
;   Dxx  : Second derivate in x of image
;   Dxy  : Derivate in y of image and then in x of the result
;   Dyy  : Second derivate in y of image
;
;_____________________________IOISIOI____________________
pro s_hessian2D, image = image, desv = desv , Dxx = Dxx, Dxy = Dxy, Dyy = Dyy

  image = double(image)
  if ~keyword_set(desv) then desv = 1

  ;; Make kernel coordinates
  VN = s_dynamicRangeArray(min = -round(3*desv), step = 1, max = round(3*desv), dType = 2)
  VM = s_dynamicRangeArray(min = -round(3*desv), step = 1, max = round(3*desv), dType = 2)

  LN = N_ELEMENTS(VN)
  LM = N_ELEMENTS(VM)

  X = VN#REPLICATE(1,LM)
  Y = VM##REPLICATE(1,LN)

  ; Build the gaussian 2nd derivatives filters
  DGaussxx = transpose(((X*X/(desv^2)-1)* exp(-(X*X + Y*Y)/(2*desv^2)))/(2*!PI*desv^4))
  DGaussxy = transpose((X*Y)* exp(-(X*X + Y*Y)/(2*desv^2))/(2*!PI*desv^6))
  DGaussyy = transpose(DGaussxx)

  Dxx = CONVOL( image, DGaussxx, /EDGE_ZERO )
  Dxy = CONVOL( image, DGaussxy, /EDGE_ZERO )
  Dyy = CONVOL( image, DGaussyy, /EDGE_ZERO )

end
