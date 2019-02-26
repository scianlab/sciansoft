;_____________________________IOISIOI____________________
; NAME:
;      s_dynamicRangeArray
;
; PURPOSE:
;       - create an Array indicated by min:step:max
;         (something TRIVIAL in MATLAB)
;
; AUTHOR:
;     Hector Moraga A. (2011)
;     e_mail: hmoraga@med.uchile.cl
;
; CALLING SEQUENCE:
;     array = s_dynamicRangeArray (min = a, step = b, max = c)
; generates an array starting with a, finished with c stepped by b.
;
; dType=
;Type Code  Type Name  Data Type  
;    0      UNDEFINED  Undefined 
;    1      BYTE       Byte 
;    2      INT        Integer
;    3      LONG       Longword integer 
;    4      FLOAT      Floating point 
;    5      DOUBLE     Double-precision floating 
;    12     UINT       Unsigned Integer 
;    13     ULONG      Unsigned Longword Integer 
;    14     LONG64     64-bit Integer 
;    15     ULONG64    Unsigned 64-bit Integer 
;_____________________________IOISIOI____________________

function s_dynamicRangeArray, min = min, step = step, max = max, dType = dType

  if (~keyword_set(min) and ~keyword_set(step)) then return, fIndGen(max)
  len = round((max - min)/step) + 1
  arr = make_array(len, type = dType, value = 0)
  for i=0,len-1 do arr[i] = min + i*step

  return, arr

end
