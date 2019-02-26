; ____________________ (this is art) IOISIOI (this is art) ____________________
;
; NAME:
;     polygonCalcs.pro
;
; PURPOSE:
;     - Utility functions to detect lines in a image.
;
; AUTHOR:
;     Mauricio Cerda (2014) mauriciocerda@med.uchile.cl
;
; NOTES:
;     Some methods are coupled to a dynamic link library (DLL) made from a LSD IPOL implementation.
;
; ____________________ (this is art) IOISIOI (this is art) ____________________


pro lineSegmentDetectFullTest

  dllLocation = getDLLfilename('idl_lsd_full', /GETFILENAMEANDPATH)
  fname  = 'C:\RSI\jjara\lsd\epitelio-small.tif' ;Dialog_Pickfile(FILTER='*.tif')
  fname  = Dialog_Pickfile(FILTER='*.tif')
  image1 = Read_tiff(fname)

  szI      = size(image1, /dim)
  imLength = double(szI[0] * szI[1])
  width    = fix(szI[0], type = 2) ; width
  height   = fix(szI[1], type = 2) ; height
  params   = double([0.5, -1000.0, 45.0, 0.5]) ;LSD paramenters, density (d), log_eps (e), ang_th (a), scale (s)
  extra_pad = 0d
  inc_factor = 4.0d
  deltaLineNumber = 0

  ;test more (free memory)
  repeat begin
    estimatedLineNumber = ceil(imLength/100.0 + inc_factor*extra_pad*imLength/100.0)

    ;output varaibles
    x0     = dblArr(estimatedLineNumber)
    y0     = dblArr(estimatedLineNumber)
    x1     = dblArr(estimatedLineNumber)
    y1     = dblArr(estimatedLineNumber)
    owidth = dblArr(estimatedLineNumber)
    op     = dblArr(estimatedLineNumber)
    olog   = dblArr(estimatedLineNumber)
    output = call_external(dllLocation, 'applylsd', double(image1), width, height,$ ;input
                           params[0], params[1], params[2], params[3],$ ;parameters
                           x0, y0, x1, y1,$ ;output
                           owidth, op, olog, estimatedLineNumber, deltaLineNumber, /unload);output
    extra_pad += 1.0d
  endrep until (deltaLineNumber[0] le 0)

  totalSegments = (estimatedLineNumber + deltaLineNumber) / 7
  ;low values have higher confidence
  ;this value is always less than 10^(-params[2]) 
  confidence = 10.0^(-olog[0:totalSegments-1])
  print, 'confidence values: ', confidence
end


function lineSegmentDetect, image, segmentsOut, fVerbose = fVerbose
  dllLocation = getDLLfilename('ipol_lsd', /GETFILENAMEANDPATH)
  imSize      = size(image, /dim)
  imWidth     = fix(imSize[0], type = 2)
  imHeight    = fix(imSize[1], type = 2)
  extra_pad   = 0d
  delta_n     = 0
  inc_factor  = 4.0
  repeat begin
    outputSize = ceil(imLength/100.0 + inc_factor * extra_pad * imLength/100.0)
    segmentsOut = dblArr(outputSize)
    segmentsOutSize = 0
    output = call_external(dllLocation, 'calc_lsd', double(image), imWidth, imHeight,$ ;input
             outputSize, segmentsOut, segmentsOutSize, delta_n)
    extra_pad += 1.0d
  endrep until (delta_n[0] le 0)
  if keyword_set(fVerbose) then begin
    print, 'done'
    print, 'output:          ', output
    print, 'segmentsOutSize: ', segmentsOutSize
    print, 'segmentsOut:     ', segmentsOut
  endif
  return, (output eq -1) ? -1 : segmentsOutSize
end


function lineSegmentDetectFull, image, confidence = confidence

  dllLocation = getDLLfilename('idl_lsd_full', /GETFILENAMEANDPATH)
  imSize      = size(image, /dim)
  imLength    = double(imSize[0] * imSize[1])
  imWidth     = fix(imSize[0], type = 2)
  imHeight    = fix(imSize[1], type = 2)
  params      = double([0.5, -1000.0, 45.0, 0.5]) ;LSD paramenters: density (d), log_eps (e), ang_th (a), scale (s)
  extra_pad   = 0d
  delta_n     = 0
  inc_factor  = 4.0

  ;test more (free memory)
  repeat begin
    outputSize = ceil(imLength/100.0 + inc_factor * extra_pad * imLength/100.0)

    ;output variables
    x0     = dblArr(outputSize)
    y0     = dblArr(outputSize)
    x1     = dblArr(outputSize)
    y1     = dblArr(outputSize)
    oWidth = dblArr(outputSize)
    op     = dblArr(outputSize)
    oLog   = dblArr(outputSize)
    output = call_external(dllLocation, 'applylsd', double(image), imWidth, imHeight,$ ;input
                           params[0], params[1], params[2], params[3],$                ;parameters
                           x0, y0, x1, y1,$                                            ;output
                           oWidth, op, oLog, outputSize, delta_n, /unload)
    extra_pad += 1.0d
  endrep until (delta_n[0] le 0)

  totalSegments = (outputSize + delta_n) / 7
  ; Low values have higher confidence
  ; This value is always less than 10^(-params[2])
  confidence = 10.0^(-oLog[0 : totalSegments-1])
  return, segments
end
