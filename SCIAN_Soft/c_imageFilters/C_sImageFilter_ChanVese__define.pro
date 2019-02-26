;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ChanVese
;
; PURPOSE:
;       - Identity-Filter-Class. See Identity.pro
;
; AUTHOR:
;     Hector Moraga (2011)
;     e_mail: hector.moraga.aros@gmail.com
;
; BASED ON:
;   Active Contours with Chan-Vese Method
;   Implementation of Yue Wu (yue.wu@tufts.edu)
;   Tufts University
;   http://sites.google.com/site/rexstribeofimageprocessing/
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ChanVese')
;
; METHODS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ChanVese::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_ChanVese::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_ChanVese::init
   filterStruct = {Name:'C_ChanVese',$     ;  Filter Name.
                    pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Names.
                    pNames:ptr_new(),$     ; Pointer on Filter Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                    pMin:ptr_new(),$    ; Pointer on Filter Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                    pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(4, /string, value = 'widget_slider')
    filterParamNames = ['Mask', 'Iterations', 'mu', 'Method']
    filterParamActive = [1, 1, 1, 1]
    filterParamMin = [0, 100, 0.2, 0]
    filterParamMax = [2, 5000, 1.0, 0]
    filterParamValues = [0, 800, 0.5, 0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

function C_sImageFilter_ChanVese::apply, image = image
   if (size(image, /n_dim) ne 2) then return, image
   imageSize=size(image, /DIMENSIONS)
   
   res = MACHAR(/DOUBLE)
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Mask'))[0]
   mask =(*(*self.pParamStruct).pValues)[whParam]

   whParam = (where((*(*self.pParamStruct).pNames) eq 'Iterations'))[0]
   iters =(*(*self.pParamStruct).pValues)[whParam]
   
   whParam = (where((*(*self.pParamStruct).pNames) eq 'mu'))[0]
   mu =(*(*self.pParamStruct).pValues)[whParam]
    
   whParam = (where((*(*self.pParamStruct).pNames) eq 'Method'))[0]
   method =(*(*self.pParamStruct).pValues)[whParam]
   
   case (method) of
      ; 0 = chan-Vese original 
      0: begin
         case (mask) of
         ;0 = small
         ;1 = medium
         ;2 = large
         ;3 = whole
         0: begin
            A=self->maskcircle(img=image, tipo='small', umbral=0.4)
            end
         1: begin
            A=self->maskcircle(img=image, tipo='medium', umbral=0.4)
            end
         2: begin
            A=self->maskcircle(img=image, tipo='large', umbral=0.4)
            end
         3: begin
            A=self->maskcircle(img=image, tipo='whole', umbral=0.4)
            end
         else: begin
               print, 'Error! Mascara no reconocida!"
            end
         endcase  
         P = double(image)
         layer=1
         
         A=A(*,*,0)
         
         B=MAKE_ARRAY((size(A, /DIMENSIONS))[0]+2, (size(A, /DIMENSIONS))[1]+2, /DOUBLE, VALUE=0.0) ; necesarios para el MORPH_DISTANCE
         C=B ; necesarios para el MORPH_DISTANCE
         B(1:(size(A, /DIMENSIONS))[0],1:(size(A, /DIMENSIONS))[1])=A
         C(1:(size(A, /DIMENSIONS))[0],1:(size(A, /DIMENSIONS))[1])=1-A
         B=MORPH_DISTANCE(B, /BACKGROUND, NEIGHBOR_SAMPLING=3)
         C=MORPH_DISTANCE(C, /BACKGROUND, NEIGHBOR_SAMPLING=3)
         B=B(1:(size(A, /DIMENSIONS))[0],1:(size(A, /DIMENSIONS))[1])
         C=C(1:(size(A, /DIMENSIONS))[0],1:(size(A, /DIMENSIONS))[1])
         phi0 = B-C+double(A)-0.5
         force = res.eps
         
         contour, ROTATE(phi0,7), LEVELS=0.0, NLEVELS=1, C_COLORS=255, XSTYLE=4, YSTYLE=4, /CLOSED ;, PATH_XY=StartContour, PATH_INFO=info, /PATH_DATA_COORDS 
                 
         ;window, 31         
         ;for i = 0, (N_ELEMENTS(info) - 1 ) DO BEGIN 
         ;   S = [INDGEN(info(i).N), 0] 
         ;   PLOTS, StartContour(*,info(i).offset + S ), /NORM
         ;endfor
         
         ;-- Main loop
          for n=1,iters do begin
              print, 'iteracion=', n           
              inidx = where(phi0 ge 0); % frontground index
              outidx = where(phi0 lt 0); % background index
              force_image = make_array(size(P, /DIMENSIONS),VALUE=0) ; initial image force for each layer
              if (layer eq 1) then begin
                  L = double(P)
                  c1 = total(L*self->heaviside(z=phi0))/(n_elements(inidx)+res.eps); % average inside of Phi0
                  c2 = total(L*(1-self->heaviside(z=phi0)))/(n_elements(outidx)+res.eps); % verage outside of Phi0
                  force_image=-(L-c1)^2+(L-c2)^2+force_image                   
              endif else begin  
                  for i=0, layer-1 do begin
                     L = double(P(*,*,i)); % get one image component
                     c1 = total(L*self->heaviside(z=phi0))/(n_elements(inidx)+res.eps); % average inside of Phi0
                     c2 = total(L*(1-self->heaviside(z=phi0)))/(n_elements(outidx)+res.eps); % verage outside of Phi0
                     force_image=-(L-c1)^2+(L-c2)^2+force_image 
                     ; sum Image Force on all components (used for vector image)
                     ; if 'chan' is applied, this loop become one single code as a
                     ; result of layer = 1
                  endfor
              endelse

              ; calculate the external force of the image 
              force = mu*self->kappa(img=phi0)/max(abs(self->kappa(img=phi0)))+1/layer*force_image

              ; normalized the force
              force = force/(max(force, /ABSOLUTE));

              ; get stepsize dt
              dt=0.5
              
              ; get parameters for checking whether to stop
              old = phi0;
              phi0 = phi0+dt*force;
              new = phi0;
              indicator = self->checkstop(old=old,new=new,dt=dt)

              ; intermediate output
              if ((n mod 20) eq 0) then self->showphi, image, phi0, n    
              if indicator then begin
                  self->showphi, image, phi0, n
                  ;make mask from SDF
                  seg = (phi0 le 0); %-- Get mask from levelset
                  ;subplot(2,2,4); imshow(seg); title('Global Region-Based Segmentation');
                  return, seg
              endif
          endfor
          self->showphi, image, phi0, n;

          ;make mask from SDF
          seg = (phi0 le 0); %-- Get mask from levelset            
      end
      else: begin
      end
   endcase
   return, seg
end

function C_sImageFilter_ChanVese::heaviside, z=z, epsilon=epsilon
   if ((size(z, /n_dim) ne 2) or (max(z) eq 0)) then return, z
   if NOT(keyword_set(epsilon)) then epsilon=1e-5
   H=fltarr((size(z, /DIMENSIONS))[0],(size(z, /DIMENSIONS))[1])
   idx1=where(z gt epsilon)
   idx2=where(abs(z) lt epsilon)
   H(idx1)=1.0
   if (idx2 ne -1) then H(idx2)=1/2*(1+z(idx2)/epsilon+1/!DPI*sin(!DPI*z(idx2)/epsilon))
   return, H
end

function C_sImageFilter_ChanVese::kappa, img = img
   ;obtiene informacion de curvatura de la imagen de entrada
   ;entrada: es una imagen en 2D
   ;salida: matriz de curvatura KG
   res = MACHAR(/DOUBLE)     ;Floating-point relative accuracy (EPS de MATLAB)
   img = double(img)
   szI=size(img,/DIMENSIONS)
   P=MAKE_ARRAY(szI[0]+2, szI[1]+2, /DOUBLE, VALUE=1)
   P(1:szI[0],1:szI[1])=img
   dimP=size(P,/DIMENSIONS)
   
   ;calculo la diferencia central
   fy=P(1:szI[0],2:dimP[1]-1)-P(1:szI[0],0:szI[1]-1)
   fx=P(2:dimP[0]-1,1:szI[1])-P(0:szI[0]-1,1:szI[1])
   fyy=P(1:szI[0],2:dimP[1]-1)+P(1:szI[0],0:szI[1]-1)-2*img
   fxx=P(2:dimP[0]-1,1:szI[1])+P(0:szI[0]-1,1:szI[1])-2*img
   fxy=0.25*(P(2:dimP[0]-1,2:dimP[1]-1)-P(2:dimP[0]-1,0:szI[1]-1)+P(0:szI[0]-1,2:dimP[1]-1)-P(0:szI[0]-1,0:szI[1]-1))
   G = sqrt(fx*fx+fy*fy)
   K = (fxx*fy*fy-2*fxy*fx*fy+fyy*fx*fx)/((G*G+res.eps)^(1.5))
   KG=K*G
   szKG=size(KG,/DIMENSIONS)
   KG(0,*)=res.eps
   KG(szKG[0]-1,*)=res.eps
   KG(*,0)=res.eps
   KG(*,szKG[1]-1)=res.eps
   KG=KG/MAX(KG,/ABSOLUTE)
   return, KG
end

function C_sImageFilter_ChanVese::checkstop, old = old, new = new, dt = dt, umbral = umbral
   ; Indica si desarrollamos mas iteraciones o nos detenemos
   if NOT(keyword_set(umbral)) then umbral=0.5

   if (size(new, /N_DIMENSIONS) le 2) then layer=1 else layer = (size(new, /DIMENSIONS))[2]
   szOld = size(old, /DIMENSIONS)
   szNew = size(new, /DIMENSIONS)

   ptrold=ptrarr(layer)
   ptrnew=ptrarr(layer)

   for i = 0, layer-1 do begin
      ptrold[i] = ptr_new(old[*,*,i])
      ptrnew[i] = ptr_new(new[*,*,i])
   endfor

   if layer then begin
      ind = where(abs(new) le umbral)
      M = n_elements(ind)
      if (size(ind, /dimensions) eq 1) then begin
         if (ind eq -1) then Q=0 else Q=total(abs(new(ind)-old(ind)))/M
      endif else Q=total(abs(new(ind)-old(ind)))/M
      ; revisar en los papers de donde aparece este valor dt*0.18^2
      if (Q le dt*0.18^2) then indicator = 1 else indicator = 0
   endif else begin
      ind1 = where (abs(*(ptrold[0]))<1);
      ind2 = where (abs(*(ptrold[1]))<1);
      M1 = n_elements(ind1);
      M2 = n_elements(ind2);
      Q1 = total(abs(*(ptrnew[0])[ind1]-*(ptrold[0])[ind1]))/M1
      Q2 = total(abs(*(ptrnew[1])[ind2]-*(ptrold[1])[ind2]))/M2
      if ((Q1 le dt*0.18^2) AND (Q2 le dt*0.18^2)) then indicator = 1 else indicator = 0
   endelse
   return, indicator
end

function C_sImageFilter_ChanVese::maskcircle, img = img, tipo = tipo, umbral = umbral
; auto pick a circular mask for image I 
; built-in mask creation function
; Input: img : input image     
;        tipo: mask shape keywords
;        mask: user defined mask
; Output: m  : mask image
   
   if NOT(keyword_set(umbral)) then umbral=0.5
   temp = double(img)
   szTemp = size(temp, /DIMENSIONS)
   h = [[0, 1, 0],[1, -4, 1],[0, 1, 0]]
   T = convol(temp, h)
   T(0,*)=0;
   T((size(T))[0]-1,*)=0;
   T(*,0)=0;
   T(*,(size(T))[0]-1)=0;
   
   thre = max(abs(T))*umbral
   idx = where(abs(T) gt thre)
   szT = size(T, /DIMENSIONS)
   cx = floor(idx/szT[0])
   cy = idx-szT[0]*cx
   cx = round(mean(cx))
   cy = round(mean(cy))
   
   self->meshgrid, min([szT[0],szT[1]]), min([szT[0],szT[1]]), x, y
   
   m = MAKE_ARRAY(szTemp[0], szTemp[1], /DOUBLE, VALUE=0)
   q = szTemp[1] ; filas
   p = szTemp[0] ; columnas
   
   case strlowcase(tipo) of
      'small':begin
         r=10
         n=((x-cx)^2+(y-cy)^2 lt r^2)
         m[0:(size(n, /DIMENSIONS))[0]-1,0:(size(n, /DIMENSIONS))[1]-1] = n
          end
         else: begin
         end
   endcase
   
   tem = MAKE_ARRAY((size(m, /DIMENSIONS))[0],(size(m, /DIMENSIONS))[1],2, /DOUBLE, VALUE=0)
   tem[*,*,0] = m
   ma = MAKE_ARRAY((size(m, /DIMENSIONS))[0]+floor(2*r/3),(size(m, /DIMENSIONS))[1]+floor(2*r/3),/DOUBLE, VALUE=0)
   ma[0:(size(m, /DIMENSIONS))[0]-1,0:(size(m, /DIMENSIONS))[1]-1] = m
   szma = size(ma, /DIMENSIONS)
   tem[*,*,1] = ma [floor(2*r/3):szma[0]-1,floor(2*r/3):szma[1]-1]
   return, tem
end

pro C_sImageFilter_ChanVese::showphi, image, phi, n
; show curve evolution of phi
; se debe poner la imagen de fondo

  if (size(phi, /N_DIMENSIONS) eq 2) then begin
      contour, phi(*,*,0), LEVELS=0.0, NLEVELS=1, C_COLORS=[255, 0 ,0], XSTYLE=4, YSTYLE=4, PATH_XY=StartContour0, PATH_INFO=info0, C_THICK=4, /OVERPLOT
      contour, phi(*,*,0), LEVELS=0.0, NLEVELS=1, C_COLORS=[0, 255, 0], XSTYLE=4, YSTYLE=4, PATH_XY=StartContour1, PATH_INFO=info1, C_THICK=1.3, /OVERPLOT
      FOR I = 0, (N_ELEMENTS(info0) - 1 ) DO BEGIN 
         S = [INDGEN(info0(I).N), 0] 
         ; Plot the closed paths: 
         PLOTS, StartContour0(*,info0(I).OFFSET + S ), /NORM 
      ENDFOR
      FOR I = 0, (N_ELEMENTS(info1) - 1 ) DO BEGIN 
         S = [INDGEN(info1(I).N), 0] 
         ; Plot the closed paths: 
         PLOTS, StartContour1(*,info1(I).OFFSET + S ), /NORM 
      ENDFOR
  endif else begin
      contour, phi(*,*,0), LEVELS=0.0, NLEVELS=1, C_COLORS=[255, 0 ,0], XSTYLE=4, YSTYLE=4, PATH_XY=StartContour0, PATH_INFO=info0, C_THICK=4, /OVERPLOT
      contour, phi(*,*,0), LEVELS=0.0, NLEVELS=1, C_COLORS=[255, 0, 0], XSTYLE=4, YSTYLE=4, PATH_XY=StartContour1, PATH_INFO=info1, C_THICK=1.3, /OVERPLOT
      contour, phi(*,*,1), LEVELS=0.0, NLEVELS=1, C_COLORS=[0, 255, 0], XSTYLE=4, YSTYLE=4, PATH_XY=StartContour2, PATH_INFO=info2, C_THICK=4, /OVERPLOT
      contour, phi(*,*,1), LEVELS=0.0, NLEVELS=1, C_COLORS=[0, 255, 0], XSTYLE=4, YSTYLE=4, PATH_XY=StartContour3, PATH_INFO=info3, C_THICK=1.3
  endelse
end

pro C_sImageFilter_ChanVese::meshgrid, N, M, X, Y
;+
; PRO MESHGRID,N,M,X,Y calculates two arrays, X
; and Y that can be used to calculate and plot a 2D function.
;
; N and M can be either positive integers or vectors. If they are
; vectors then N is used as the rows of X and M is used as the columns of
; Y. If they are integers then the rows of X are IndGen(N) and the columns
; of Y are Indgen(M).
;
; Example 1
; MESHGRID,31,31,X,Y
; X=(X-15)/2.
; Y=(Y-11)/3.
; Z=EXP(-(X^2+Y^2))
; SURFACE,Z
;
; Example 2
; MESHGRID,[2,4,6],[3,5,7,9],X,Y
; creates two arrays of size 3x4 with the X array containg four rows
; with [2,4,6] and Y with columns [3,5,7,9]'.
;
; HISTORY:
; Originated by Harvey Rhody September 17, 1997.
; Revised April 2001 to accomodate vectors for N and M.
;-

IF N_ELEMENTS(N) EQ 1 THEN VN=indGen(N) ELSE VN=N
IF N_ELEMENTS(M) EQ 1 THEN VM=indGen(M) ELSE VM=M

LN=N_ELEMENTS(VN)
LM=N_ELEMENTS(VM)

X=VN#REPLICATE(1,LM)
Y=VM##REPLICATE(1,LN)
end

pro C_sImageFilter_ChanVese__define
   tmp = {C_sImageFilter_ChanVese, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
