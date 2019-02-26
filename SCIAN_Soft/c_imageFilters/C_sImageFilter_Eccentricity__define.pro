;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Eccentricity
;
; PURPOSE:
;       - Eccentricity-Filter-Class.
;
; AUTHOR:
;   Hector Moraga (2011)
;   e_mail: hmoraga@med.uchile.cl
;
; CALLING SEQUENCE:
;        result = obj_new('C_sImageFilter_Eccentricity' )
;
; METHODS:
; Filter used for eccentricity values between 0 to 1 (ellipses to circles)
;
;_____________________________IOISIOI____________________

function C_sImageFilter_Eccentricity::getImageFilterDevelopmentalState
    return, 'Release_Image_Filter_Method'
end


function C_sImageFilter_Eccentricity::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end


function C_sImageFilter_Eccentricity::apply, $
                     image = image,$
                     selectedStackObject = selectedStackObject,$
                     tPos = tPos,$
                     chPos = chPos,$
                     zPos = zPos,$
                     clusPos = clusPos,$
                     segPos = segPos,$
                     cut_x = cut_x,$
                     cut_y = cut_y,$
                     stack_tlb = stack_tlb
                     
    if (size(image, /n_dim) ne 2) then return, image
    
    dimI = size(image, /dim)
    width = dimI(0)
    height = dimI(1)
    
    ; conic type    ; HECTORIN ESTO NO FUNCIONAAAAAA EL LÍMITE SUPERIOR DEL ¿WIDGET? ES 2   EN TODO CASO NO ESTÁ IMPLEMENTADO NADA DISTINTO DE CERO,  SUSANA
    ; 0 = ellipse
    ; 1 = circle
    ; 2 = parabola
    ; 3 = hyperbola
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Conic Type'))[0]
    conic = (*(*self.pParamStruct).pValues)[whParam]
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Lim Inferior'))[0]
    liminf = (*(*self.pParamStruct).pValues)[whParam]
    
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Lim Superior'))[0]
    limsup = (*(*self.pParamStruct).pValues)[whParam]
    
    ;Calculate The compliment of the liminf<=Param<=limsup
    ; i.e. Param=[0,liminf[ u ]limsup,1] 
    whParam = (where((*(*self.pParamStruct).pNames) eq 'Negate'))[0]
    negate = (*(*self.pParamStruct).pValues)[whParam]
   
    if (liminf gt limsup) then return, image 
    
    ; create the ROI object to get the masks, images, ...
    oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [width, height, 1], ZSliceInterval = 1)

    oROI2DGroup->addROIObjectsFromSegImage, maskImage = image, intImage = image, objNumberVector = objNumberVector
    counter = oROI2DGroup->getObjectNumberVector()
    
    ; creation of an image copy
    ImageCopy = make_array(size(image,/DIMENSIONS),VALUE=0)
    
    if( counter[0] eq -1 ) then return, imageCopy

    case (conic) of
       '0':begin
       ;ellipse means 0<eccentricity<1 strictly
       for i=0, n_elements(counter)-1 do begin
            Obj=((oROI2DGroup->get(position = i))->getxyzPoints())[0:1,*]
            ;Obj=*((oROI2DGroup->get(position = i))->getpObjectBorderPolygonList())[0]
            ;calculos a mano de cada ROI (debe haber mejores opciones)
            sumx=total(Obj[0,*])
            sumy=total(Obj[1,*])
            sumxx=total(Obj[0,*]*Obj[0,*])
            sumyy=total(Obj[1,*]*Obj[1,*])
            sumxy=total(Obj[0,*]*Obj[1,*])
            n=n_elements(Obj[1,*])
            ;calculos de media, varianza y covarianza
            xmom=MOMENT(Obj[0,*])
            ymom=MOMENT(Obj[1,*])
            ;mean
            xbar=xmom[0]
            ybar=ymom[0]
            ;variance
            varx=xmom[1]
            vary=ymom[1]
            ;covariance
            covarxy=sumxy/n
            dx=Obj[0,*]-xbar
            dy=Obj[1,*]-ybar
            sumdxdx=total(dx*dx)
            sumdydy=total(dy*dy)
            sumdxdy=total(dx*dy)
            ;covarxy=sumdxdy/n; Susana inventando covarianza
            ;angulo de rotacion de la elipse
            theta=0.5*atan(2*sumdxdy/(sumdxdx*sumdydy))
            c = cos(theta)
            s = sin(theta)
            ;volvemos a dejar sin rotacion la elipse
            X = c*dx-s*dy
            Y = s*dx+c*dy
            sumXX=total(X*X)
            sumYY=total(Y*Y)
            varX=sumXX/n ;equivalentes a^2 y b^2, se debe comparar cual es mayor
            varY=sumYY/n ;equivalentes a^2 y b^2, se debe comparar cual es mayor
            ;calculo de excentricidad
            if (VarY gt VarX) then eccentricity=sqrt((VarY-VarX)/VarY) else eccentricity=sqrt((VarX-VarY)/VarX)

            ;repinto los pixeles de la imagen de acuerdo a los valores de excentricidad
            imageCopy(Obj[0,*],Obj[1,*])=ceil(100*eccentricity)
       endfor
       end
       '1':begin
       ;circle means eccentricity = 0
       end   
       else: begin ; HECTOR, ESTA LÍNEA NO LA ENTIENDO !!!   SUSANA
       endelse ; HECTOR, ESTA LÍNEA NO LA ENTIENDO !!!   SUSANA
    endcase
    return, imageCopy
end    

function C_sImageFilter_Eccentricity::init
    filterStruct = {Name: 'C_Eccentricity',$       ;  Filter Name.
                           pWidgetType:ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames:ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pActive:ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                           pMin:ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax:ptr_new(),$     ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()}         ; Pointer on Filter Parameter Values.

    ; Parameters of C_sImageFilter_Eccentricity.   
    filterParamWidgetType = make_array(4, /string, value = 'widget_slider')
    filterParamNames = ['Conic Type', 'Lim Inferior', 'Lim Superior', 'Negate']
    filterParamActive = [1, 1, 1, 1]
    filterParamMin = [0, 0., 0., 0]
    filterParamMax = [2, 1, 1, 1]
    filterParamValues = [0, 0.2, 0.6, 0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(filterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_Eccentricity__define
   tmp = {C_sImageFilter_Eccentricity, pParamStruct:ptr_new(), inherits C_sImageFilter}
end    