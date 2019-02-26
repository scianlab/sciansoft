;_____________________________IOISIOI____________________
; FUNCTION_NAME:
;       s_conv2
;
; PURPOSE:
;      Implementation of CONV2 of MATLAB
;
; AUTHOR:
;   Hector Moraga A. (2012)
;   e_mail: hmoraga@med.uchile.cl
;
; CALLING SEQUENCE:
;   result = conv2(A = A, B = B, CTYPE = tipo)
;   A, B:  2D-Arrays
;          if ma = size(A, /DIMENSIONS)
;             mb = size(B, /DIMENSIONS)
;   CTYPE:
;     0 = 'full', resulting matrix size (ma[0]+mb[0]-1,ma[1]+mb[1]-1), default option
;     1 = 'same', resulting matrix size (ma[0],ma[1])
;     2 = 'valid', if size(A,/DIMENSIONS)>size(B,/DIMENSIONS):
;                  resulting matrix size (ma[0],ma[1]) else -1 
;
; RETURNS:
;     Convolution of matrices according to CTYPE option
;
;_____________________________IOISIOI____________________

function s_conv2, A = A, B = B, CTYPE = tipo

  on_error, 2

  if (~keyword_set(A) and ~keyword_set(B)) then message, level = 1, 'At least one input matrix is required'
  if ~(keyword_set(CTYPE)) then CTYPE = 0
  if (keyword_set(A) and ~keyword_set(B)) then B = A
  if (keyword_set(A) and keyword_set(B)) then begin
    ma = size(A, /DIMENSIONS)
    mb = size(B, /DIMENSIONS)
    C = make_array(ma[0] + mb[0]-1, ma[1] + mb[1]-1, TYPE = 5, VALUE = 0)

    for n2 = 0, ma[0]+mb[0]-2 do begin
      for n1 = 0, ma[1]+mb[1]-2 do begin
        for k1 = max([0, n1-mb[0]+1]), min([ma[0]-1, n1]) do begin
          k2min = max([0,n2-mb[1]+1])
          k2max = min([ma[1]-1, n2])
          C[n2,n1] += A[k2min:k2max, k1] ## transpose(rotate(B[n2-k2max:n2-k2min, n1-k1], 2))
        endfor
      endfor
    endfor
  endif
  case tipo of
    1: begin
       x1 = ceil((mb[0]-1) / 2)
       y1 = ceil((mb[1]-1) / 2)
       return, C[x1:(ma[0]+x1-1), y1:(ma[1]+y1-1)]
    end
    2: begin
    end 
  else: return, C
  endcase
end
