
;_____________________________IOISIOI____________________
; NAME:
;     C_sTrackObject
;
; PURPOSE:
;      - Object for xyzt-Tracking Data
;
; AUTHOR:
;    Dr. Steffen Härtel (2009)
;    e_mail: shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;      result = obj_new('C_sTrackObject')
;
; METHOHDS:
;
;_____________________________IOISIOI____________________


function C_sTrackObject::getParamNameList
   return, ['Number',$
            'Extra']
end

pro C_sTrackObject::setParamAsStruct, paramStruct
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]] = paramStruct.a
   *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Extra'))[0]] = paramStruct.b
end

function C_sTrackObject::getParamAsStruct
   return, {a: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]],$
            b: *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Extra'))[0]]}
end


pro C_sTrackObject::set, pParamStruct = pParamStruct
   *self.pParamStruct = *pParamStruct
end

function C_sTrackObject::getpParamStruct
   return, self.pParamStruct
end

pro C_sTrackObject::setXYZValues, XYZValues
   if (n_elements(XYZValues) ne 0) then begin
      dimXYZValues = size(XYZValues, /dim)
      case n_elements(dimXYZValues) of
      1: if (dimXYZValues eq 3) then *self.pXYZValues = XYZValues
      2: if (dimXYZValues[1] eq 3) then *self.pXYZValues = XYZValues
      else:
      endcase
   endif
end

function C_sTrackObject::getpXYZValues
 return, self.pXYZValues
end


function C_sTrackObject::getXYZVelocity
   lrr=n_elements(*(self.pTValues))
   if lrr gt 1 then begin
      dt=shift(*(self.pTValues),-1)-*(self.pTValues)
      dr=shift(*(self.pXYZValues),-1,0)-(*(self.pXYZValues))
      dt=dt[0:lrr-2]
      dr = dr[0:lrr-2,*]
      d=sqrt(dr[*,0]^2+dr[*,1]^2+dr[*,2]^2)  ;¿linea innecesaria?
      rapidez=d/dt  ;¿linea innecesaria?
      dt=rebin(dt,lrr-1,3)
      ;print,dt
      XYZVelocity=dr/dt
      
      
        ; TODO this is only a temporal change until we fix dt !!!!!!!!!!!!!!!!!!
      ;XYZVelocity=dr/0.033
      ;vtotal=sqrt(XYZVelocity[*,0]^2+XYZVelocity[*,1]^2+XYZVelocity[*,2]^2)
      ;duda existencial:
      ; si esta función solo recibe una dimension (X Y o Z) ¿como explico la linea anterior?
   endif else XYZVelocity = [[-1],[-1],[-1]]
   return, XYZVelocity
end

function C_sTrackObject::getXYZDisplacement
   ;VCL corregida MOR y Susana
   lrr=n_elements(*(self.pTValues))
   if lrr gt 1 then begin
      dt=shift(*(self.pTValues),-1)-*(self.pTValues)
      dr=shift(*(self.pXYZValues),-1,0)-(*(self.pXYZValues))
      dt=dt[0:lrr-2]
      dr = dr[0:lrr-2,*]
      d=sqrt(dr[*,0]^2+dr[*,1]^2+dr[*,2]^2)  ;¿linea innecesaria?
      ;rapidez=d/dt  ;¿linea innecesaria?
      ;dt=rebin(dt,lrr-1,3)
      ;XYZVelocity=dr/dt
    endif else d = [[-1],[-1],[-1]]
   return, d
end


 function C_sTrackObject::getSpath
   ; MOR - 18 Aug 2010
   ; implement smoothing moving average
   lrr = n_elements(*(self.pTValues))
   Spath = make_array(lrr,3)
   sw = 3    ; number of neighbors on each side on which to average  
   ; MOR - do smoothing in each dimension individually
  
   Spathx = self->getMovingAverage(n=sw, X=(*(self.pXYZValues))[*,0], lines = [0,3])
   if(Spathx[0] eq -1) then begin 
      Spath[*,0] = (*(self.pXYZValues))[*,0] 
      endif  else begin
      Spath[*,0] = Spathx
      endelse
  
   Spathy = self->getMovingAverage(n=sw, X=(*(self.pXYZValues))[*,1], lines = [1,4])
   if(Spathy[0] eq -1) then begin 
          Spath[*,1] = (*(self.pXYZValues))[*,1] 
   endif  else begin
      Spath[*,1] = Spathy
   endelse
   
   Spathz = self->getMovingAverage(n =sw, X =(*(self.pXYZValues))[*,2], lines = [2,5])
   if(Spathz[0] eq -1) then begin 
          Spath[*,2] = (*(self.pXYZValues))[*,2] 
   endif else begin
   Spath[*,2] = Spathz
   endelse
   
  return,Spath
end

function C_sTrackObject::getMovingAverage, n = n, X = X, lines = lines
   ; inputs:  X - array which will be smoothed
   ;          n - number of neighbors to use on either side for averaging 
   ;          n should be an integer
   ;         lines - 2 element vector on which determining on 
   ;         which lines to find the rotated edges 

   ; check the dimensions of the input array
   sizX = n_elements(X)

   ; size of window on which smoothing will occur 
   wid = 2*n+1

   ; check that X has enough data to do smooth using n points
   if(wid gt sizX) then begin
     ; print, 'Not enough points in vector for a moving average using specified neighbors.'
      return, [-1]
   endif else begin
  

   ; alternatively...use a for loop
   rotEdges = self->getRotatedEdges(sw = n, lrr = sizX)
   XX = [rotEdges[*,lines[0]], X, rotEdges[*,lines[1]]]  ; pad with the rotated edges
   s  = make_array(sizX, /float, value = 0)
   lenXX = n_elements(XX)
  
  for j = n+1, lenXX-n do begin
      s[j-(n+1)] = total(XX[j-n-1:j+n-1],/double)
  endfor
  
  s/= wid
   
     return, s
   endelse
end

 
function C_sTrackObject::getRotatedEdges, sw = sw, lrr = lrr
; MOR - 16 Aug 2010 function called by getSpath which returns vectors with
; the endpoints rotated 180 degs in order to 
; do a smoothing of the first and last point in the xyz-track path 
; used for the VAP calculation
; input: n, window used for the smoothing
;        lrr, number of elements in the xyz-path vectors
;        should have equal number of elmts in all dims.
; @todo - make this a 3D rotation!
      
   ; check if the vector has enough values in order to use for the reflection
   if lrr gt sw then begin
   ; get the x, y points necessary to reflect on either end
   tempBeg = (*(self.pXYZValues))[0:sw-1,0:1] ; currently testing only 2D
  
   tempEnd = (*(self.pXYZValues))[(lrr-sw):(lrr-1),0:1]
   
   ; determine points off of which to rotate
   a = tempBeg[0,*] 
   b = tempEnd[sw-1,*] 
   
   ; form the translation matrices
   ; reflecting across y = a or y = b lines
   Txa = [[1, 0, a[0]] ,[0, 1, a[1]], [0, 0, 1]]
   negTxa = [[1, 0, -a[0]] ,[0, 1, -a[1]], [0, 0, 1]]
   Txb = [[1, 0, b[0]] ,[0, 1, b[1]], [0, 0, 1]]
   negTxb = [[1, 0, -b[0]] ,[0, 1, -b[1]], [0, 0, 1]]

   ; form rotation matrix components using quaternions
   A = [0, 0, -1]; % unit vector in the axis of rotation, z-axis out of the page
   rotAng = !pi
   qx = A[0]*sin(rotAng/2)
   qy = A[1]*sin(rotAng/2)
   qz = A[2]*sin(rotAng/2)
   qw = cos(rotAng/2)

   sx = 2.0*qx
   sy = 2.0*qy
   sz = 2.0*qz
   wx = sx*qw
   wy = sy*qw
   wz = sz*qw
   xx = sx*qx
   yy = sy*qy
   zz = sz*qz
   xy = sy*qx
   yz = sz*qy
   zx = sx*qz
   
   ; form rotation matrix
   RotM = [[1.0-(yy + zz), xy+wz, zx-wy],[xy-wz, 1.0-(xx+zz), yz+wx],[zx+wy, yz-wx, 1.0-(xx+yy)]]
   
   ; reflect endpoints
   ones = make_array(sw,1, /float, value = 1) 
   BegVect = Txa ## RotM ## negTxa ## [[tempBeg], [ones]]
   EndVect = Txb ## RotM ## negTxb ## [[tempEnd], [ones]]

   ; these vectors have a n x 3 dimension
   return, [[BegVect], [EndVect]]
   
   endif else begin
      return, [[-1],[-1]]
   endelse

end
 
 

function C_sTrackObject::getVAP
   lrr = n_elements(*(self.pTValues))
   x = self->getSpath()
   if lrr gt 1 then begin
      dt = shift(*(self.pTValues),-1)-*(self.pTValues)  
      dr = shift(self->getSpath(),-1)-(self->getSpath())
      dt = dt[0:lrr-2]
      dr = dr[0:lrr-2,*]
      ;dt = rebin(dt,lrr-1,3)
      d = sqrt((dr[*,0]^2) + (dr[*,1]^2) + (dr[*,2]^2))
      
      return, [d/dt] ;return, [dr/dt]
   endif
   out = make_array(lrr, /float, value = -1)
   return,out ;return, [[-1],[-1],[-1]]
end

function C_sTrackObject::getVSL
   dimR = n_elements(*(self.pTValues))
;   print,'elements='+string(dimR)
;   print,'-----'
   ;t = rebin(*(self.pTValues), dimR, 3) 
   t = *(self.pTValues)
;   print,max((*(self.pXYZValues)))
   d = sqrt( (((*(self.pXYZValues))[dimR-1,0] - (*(self.pXYZValues))[0,0])^2) + (((*(self.pXYZValues))[dimR-1,1] - (*(self.pXYZValues))[0,1])^2) + (((*(self.pXYZValues))[dimR-1,2] - (*(self.pXYZValues))[0,2])^2) ) / (t[dimR-1] - t[0])
   ;if (dimR gt 1) then return, ((*(self.pXYZValues))[dimR-1,*] - (*(self.pXYZValues))[0,*]) / (t[dimR-1,*] - t[0,*])
   if (dimR gt 1) then return, d
   return, [-1];return, [[-1],[-1],[-1]]
end


function C_sTrackObject::getALH

Dim=n_elements(*self.pTValues)
ALH=make_array(dim)
for p=0,dim-1 do begin
   array=[(*self.pXYZValues)[p,*],(self->getSpath())]
   
   
;   window,0
;   PLOT, array[*,0], array[*,1], PSYM = 6, SYMSIZE = 2,$ 
;   XRANGE = [min((*self.pXYZValues)[*,0]),max((*self.pXYZValues)[*,0])], YRANGE = [min( (*self.pXYZValues)[*,1]),max( (*self.pXYZValues)[*,1])],$ 
;   TITLE='Distance between each point' 
;   OPLOT, (*self.pXYZValues)[*,0], (*self.pXYZValues)[*,1], PSYM = 2, SYMSIZE = 2


   dis=DISTANCE_MEASURE(transpose(array)) 
   tot=(size(array,/dimensions))[0]
   H=[0,0,0]
   z=0
      for i=0,tot-2 do begin
      for j=1,tot-1 do begin
      if i lt j  then H=[[H],[i,j,z]]
      endfor
      endfor
   H=H[*,1:(size(H,/dimensions))[1]-1]
   H=H[*,0:dim-1]
   
;   for k=0,DIM-1 do BEGIN
;   plots,transpose(array[[H[0,k],H[1,k]],0:1]),/continue
;   ENDFOR

   cand=(self->getSpath())[[sort(dis[0:Dim-1])],*]
   
;   wdelete
;   PLOT, array[*,0], array[*,1], PSYM = 6, SYMSIZE = 2,$ 
;   XRANGE = [min((*self.pXYZValues)[*,0]),max((*self.pXYZValues)[*,0])], YRANGE = [min( (*self.pXYZValues)[*,1]),max( (*self.pXYZValues)[*,1])],$ 
;   TITLE='Distance between each point' 

   if(size(cand,/dimensions))[0] ge 2 then begin
   ALH[p]=PNT_line(transpose((*self.pXYZValues)[p,0:1]),transpose(cand[0,0:1]),transpose(cand[1,0:1]),PL) 
   
;   OPLOT, (*self.pXYZValues)[*,0], (*self.pXYZValues)[*,1], PSYM = 2, SYMSIZE = 2
;   oplot,[(*self.pXYZValues)[p,0],cand[0:1,0]],[(*self.pXYZValues)[p,1],cand[0:1,1]],PSYM = 4, SYMSIZE = 2
;   plots,transpose([(*self.pXYZValues)[p,0:1],cand[0:1,0:1]]),/continue

   endif else ALH[p]=0
    
endfor
   return, ALH
end


function C_sTrackObject::getXYZAceleration

   XYZVelocity = self->getXYZVelocity()
   lv=n_elements(XYZVelocity[*,0])
     if lv gt 1 then begin
        dt= self->getdTValues()
        dt=rebin(dt,lv,3) 
        dt2=shift(dt,-1,0)
        dt2=dt2[0:lv[0]-2,*]
        dv=shift(XYZVelocity,-1,0)-XYZVelocity
        dv=dv[0:lv-2,*]
        XYZaceleration=dv/dt2
        atotal=sqrt(XYZaceleration[*,0]^2+XYZaceleration[*,1]^2+XYZaceleration[*,2]^2)
   endif else XYZaceleration = [[-1],[-1],[-1]]
   return, XYZaceleration
end


function C_sTrackObject::getBCF
      ;  muy preliminar, camino por tensor y kronecker
   t=*self.pTValues
   if n_elements(t)gt 1 then begin
   
   x1=(self->getspath())[*,0]
   y1=(self->getspath())[*,1]
   x2=(*(self->getpXYZValues()))[*,0]
   y2=(*(self->getpXYZValues()))[*,1]
   dx1=shift(x1,-1)-x1
   dx2=shift(x2,-1)-x2
   dy1=shift(y1,-1)-y1
   dy2=shift(y2,-1)-y2
   
   S1=rebin(dx1*y1[0:n_elements(t)-2]-dy1*x1[0:n_elements(t)-2],1,n_elements(t)-1)
   S2=rebin(dx2*y2[0:n_elements(t)-2]-dy2*x2[0:n_elements(t)-2],1,n_elements(t)-1)
   
   tensor = kron(dx1,y2[0:n_elements(t)-2])-kron(dx1,y2[0:n_elements(t)-2])
   if C1 le 0 then C
   endif else BCF=0
   return,BCF
end


function C_sTrackObject::getBCFold
      ;  no funciona muy bien: revisar !!!!

spath=self->getspath()
xyz=*(self->getpXYZValues())
t=*self.pTValues
found= ['nan','nan']  
 
if n_elements(t)gt 1 then begin
  LCurv=make_array(2,n_elements(t)-1)
  LSmot=make_array(2,n_elements(t)-1)

  for p=0, n_elements(t)-2 do begin
    if abs(xyz[p,0]-xyz[p+1,0])ne 0 then LCurv[*,p] = cramer([[xyz[p,0],1.],[xyz[p+1,0],1.]],[xyz[p,1],xyz[p+1,1]],Zero=1.0e-5)
    if abs(spath[p,0]-spath[p+1,0])ne 0 then LSmot[*,p] = cramer([[spath[p,0],1.],[spath[p+1,0],1.]],[spath[p,1],spath[p+1,1]],Zero=1.0e-5)
  endfor

  count=0
  for i=0, (size(LSmot,/dimensions))[1]-1 do begin
    for j=0, (size(LCurv,/dimensions))[1]-1 do begin ;8 problemas
      if j eq 8 then begin
        print, "..."
      endif
      if abs(LCurv[0,i]-LSmot[0,j])ne 0 then Inter = cramer([[LSmot[0,i],-1.],[LCurv[0,j],-1.]],[LSmot[1,i],LCurv[1,j]],Zero=1.0e-5)
      
        if ((spath[i+1,0]-spath[i,0]) gt 0) and ((spath[i+1,1]-spath[i,1])gt 0) then direccion=1 else $;caso NorEste
        if ((spath[i+1,0]-spath[i,0]) lt 0) and ((spath[i+1,1]-spath[i,1])gt 0) then direccion=2 else $;caso NorOeste
        if ((spath[i+1,0]-spath[i,0]) lt 0) and ((spath[i+1,1]-spath[i,1])lt 0) then direccion=3 else $; caso SurOeste
        if ((spath[i+1,0]-spath[i,0]) gt 0) and ((spath[i+1,1]-spath[i,1])lt 0) then direccion=4 else $; caso SurEste
        if ((spath[i+1,0]-spath[i,0]) eq 0) and ((spath[i+1,1]-spath[i,1])gt 0) then direccion=5 else $;caso Norte
        if ((spath[i+1,0]-spath[i,0]) gt 0) and ((spath[i+1,1]-spath[i,1])eq 0) then direccion=6 else $;caso Este
        if ((spath[i+1,0]-spath[i,0]) eq 0) and ((spath[i+1,1]-spath[i,1])lt 0) then direccion=7 else $;caso Sur
        if ((spath[i+1,0]-spath[i,0]) lt 0) and ((spath[i+1,1]-spath[i,1])eq 0) then direccion=8 else $;caso Oeste
        if ((spath[i+1,0]-spath[i,0]) eq 0) and ((spath[i+1,1]-spath[i,1])eq 0) then direccion=9
        
        case direccion of 
          1: begin if (inter[0] gt spath[i,0]) and (inter[0] lt spath[i+1,0]) and (inter[1] gt spath[i,1]) and (inter[1] lt spath[i+1,1]) then count++
          endcase
          2:begin  if (inter[0] lt spath[i,0]) and (inter[0] gt spath[i+1,0]) and (inter[1] gt spath[i,1]) and (inter[1] lt spath[i+1,1]) then count++ 
          endcase
          3: begin if (inter[0] lt spath[i,0]) and (inter[0] gt spath[i+1,0]) and (inter[1] lt spath[i,1]) and (inter[1] gt spath[i+1,1]) then count++ 
          endcase
          4: begin if (inter[0] gt spath[i,0]) and (inter[0] lt spath[i+1,0]) and (inter[1] lt spath[i,1]) and (inter[1] gt spath[i+1,1]) then count++
          endcase
          5:begin  if (inter[1] gt spath[i,1]) and (inter[1] lt spath[i+1,1]) then count++
          endcase
          6: begin if (inter[0] gt spath[i,0]) and (inter[0] lt spath[i+1,0]) then  count++
          endcase
          7: begin if (inter[1] lt spath[i,1]) and (inter[1] gt spath[i+1,1]) then count++
          endcase
          8: begin if (inter[0] lt spath[i,0]) and (inter[0] gt spath[i+1,0]) then count++
          endcase
          else: print,'inmovil'
        endcase
    endfor
  endfor
endif

if (n_elements(found) gt 2) then found=found[*,1:(size(found,/dimensions))[1]-1]else found=found

  dim=size(transpose(found),/dimensions)
  if dim[1] gt 1 then aux=where(finite(found),count)
  
  if count ne 0 then found=reform(found[aux],2,dim[1]) else found=0
   print,found
   
  if (count ne 0) then BCF=(size(transpose(found),/dimensions))[0] else BCF=0
  return,BCF
End



function C_sTrackObject::getDPos
   dimR = n_elements(*(self.pTValues))
   if dimR gt 1 then return, [(*(self.pXYZValues))[dimR-1,*],(*(self.pXYZValues))[0,*]]
   return, [[-1],[-1],[-1]]
end


pro C_sTrackObject::setTValues, tValues
   *self.pTValues = tValues
end


function C_sTrackObject::getdTValues
ltt=n_elements(*self.pTValues)
if ltt gt 1 then begin
dt=shift(*(self.pTValues),-1)-(*(self.pTValues))
dTValues=dt[0:ltt-2]
endif else dtValues=1
   return, dTValues
end


function C_sTrackObject::getpTValues
   return, self.pTValues
end

function C_sTrackObject::getNumber
   return, *(*self.pParamStruct).pValues[(where(*(*self.pParamStruct).pNames eq 'Number'))[0]]
end

; MOR - 12Feb2011 - get concentration in time
function C_sTrackObject::getNumberConcentration
   return, self.pNumObjs
end

pro C_sTrackObject::cleanUp
   if ptr_valid(self.pParamStruct) then begin
      ptr_free, (*self.pParamStruct).pNames
      for i = 0, n_elements((*self.pParamStruct).pValues)-1 do ptr_free, (*self.pParamStruct).pValues[i]
      ptr_free, self.pParamStruct
   endif

   if ptr_valid(self.pXYZValues) then ptr_free, self.pXYZValues
   if ptr_valid(self.pTValues) then ptr_free, self.pTValues
end


function C_sTrackObject::init, number = number,$
                               extra = extra,$
                               tValues = tValues,$
                               XYZValues = XYZValues,$
                               numObjs = numObjs ; MOR - 12Feb2011 - added for displaying concentration on track motil
                              
   paramNameList = self->getParamNameList()
   paramStruct = {TrackObjParamStruct,$
                  pValues: ptrArr(n_elements(paramNameList), /allocate),$
                  pNames: ptr_new(paramNameList, /no_copy)}

   if (n_elements(number) eq 0) then number = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Number'))[0]] = number
   if (n_elements(extra) eq 0) then extra = -1
   *paramStruct.pValues[(where(*paramStruct.pNames eq 'Extra'))[0]] = extra

   self.pParamStruct = ptr_new(paramStruct, /no_copy)

   if (n_elements(tValues) ne 0) then self.pTValues = ptr_new(tValues, /no_copy)
   if (n_elements(XYZValues) ne 0) then begin
      dimXYZValues = size(XYZValues, /dim)
      case n_elements(dimXYZValues) of
      1: if (dimXYZValues eq 3) then self.pXYZValues = ptr_new(XYZValues, /no_copy)
      2: if (dimXYZValues[1] eq 3) then self.pXYZValues = ptr_new(XYZValues, /no_copy)
      else:
      endcase
   endif
    ; MOR - 12Feb2011 - added for displaying concentration on track motil
    if (n_elements(numObjs) ne 0) then self.pNumObjs = ptr_new(numObjs, /no_copy)
   return, 1
end

pro C_sTrackObject__define
   tmp = {C_sTrackObject, pParamStruct:ptr_new(),$
                          pXYZValues:ptr_new(),$
                          pTValues:ptr_new(),$
                          pdTValues:ptr_new(),$
                          pXVelocity:ptr_new(),$
                          pYVelocity:ptr_new(),$
                          pZVelocity:ptr_new(),$
                          pTotalVelocity:ptr_new(),$
                          pXAceleration:ptr_new(),$
                          pYAceleration:ptr_new(),$
                          pZAceleration:ptr_new(),$
                          pTotalAceleration:ptr_new(),$
                          pTotaldistance:ptr_new(),$;}
                          pNumObjs:ptr_new()}  ; MOR - 12Feb2011 - added for displaying concentration on track motil
end