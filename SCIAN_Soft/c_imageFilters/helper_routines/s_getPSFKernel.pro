;_____________________________IOISIOI____________________
; NAME:
;   s_getPSFKernel  (c:\rsi\Steffen\s_TimeCalc\C_sImageFilter\helper_routines\s_getPSFKernel.pro)
;
; PURPOSE:
;   returns 2-D Kernel calculated with PSF-Data
;
; AUTHOR:
;   Dr. Steffen Härtel (2005)
;   e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;   result = s_getPSFKernel(PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize)
;
; METHOHDS:
;_____________________________IOISIOI____________________


function s_getPSFKernel, PSF = PSF, xPixSize = xPixSize, yPixSize = yPixSize

   if (n_elements(PSF) eq 0) then return, -1

     ; Gauss-Fit of PSF
   PSFVect = [reverse(PSF), PSF[1:*]]
   xVect = make_array(n_elements(PSF)*2-1, /float, /index) * .04 -.28
   maxG = max(PSF)
   maxAt = 0.
   sdG = .12
   PSFGauss = gaussFit(xVect, PSFVect, fitValues, estimates = [maxG, 0., sdG], nterms = 3, chisq = chisq, sigma = sigma)
   print, '_______________________'
   print, 'Result of Gauss-Fit: A0*exp(-.5*((x-A1)/A2)^2)'
   print, 'A0: ', strCompress(string(fitValues[0])), ' A1: ', strCompress(string(fitValues[1])), ' A2: ', strCompress(string(fitValues[2]))
   print, '2*sigma: ', 2.*fitValues[2]
   print, 'chisq: ', chisq, 'sigma: ', sigma

     ;define 2D-Gauss-Kernel-Size. Values gt 0.5% of GaussMax
   xVect = make_array(n_elements(PSF)*2-1, /float, /index) * .04
   kernelSize = (where( (fitValues[0] * exp(-.5 * (xVect/fitValues[2])^2)) lt (fitValues[0] * 0.005)  ))[0]

     ; calculate Kernel-Values
   PSFKernel = make_array(2*kernelSize-1, 2*kernelSize-1, /float)
   for i = -kernelSize+1,kernelSize-1 do for j = -kernelSize+1,kernelSize-1 do $
      PSFKernel[i+kernelSize-1,j+kernelSize-1] = fitValues[0] * exp(-.5 * ((sqrt((i*0.04)^2 + (j*0.04)^2) - fitValues[1])/fitValues[2])^2)

      ; adjust Kernel to Picture-Pixel Dimensions
   return, congrid(PSFKernel, (2*ceil(kernelSize*0.04/xPixSize)-1) > 3, (2*ceil(kernelSize*0.04/yPixSize)-1) > 3, /interp, /minus)
end