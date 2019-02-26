;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_EllipsenessEstimator
;
; PURPOSE:
;       - EllipsenessEstimator-Filter-Class.
;
; AUTHOR:
;   Hector Moraga (2012)
;   e_mail: hmoraga@med.uchile.cl
;
; CALLING SEQUENCE:
;        result = obj_new('C_sImageFilter_EllipsenessEstimator')
;
; METHODS:
; Filter used as ellipse estimator by means of (Real Area of ROI/Area Estimated Ellipse) ratio. 
; INPUT: IMAGE with binary ROIs 
;
; OUTPUT: IMAGE with the same ROIs but painted according their ratio (Real Area of ROI/Area Estimated Ellipse),
;         as an estimator of ellipse shape of each ROI        
;
;_____________________________IOISIOI____________________

function C_sImageFilter_EllipsenessEstimator::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    ;return, 'Alpha_Debugging_Filter_Method'
    return, 'Beta_Release_Filter_Method'
end


function C_sImageFilter_EllipsenessEstimator::getImageFilterType
  return, 'Single_Image_Filter_Method'
end


function C_sImageFilter_EllipsenessEstimator::apply, image = image

  if (size(image, /n_dim) ne 2) then return, image

  dimI = size(image, /dim)
  width = dimI(0)
  height = dimI(1)

    ; create the ROI object to get the masks, images, ...
  oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [width, height, 1], ZSliceInterval = 1)
  oROI2DGroup->addROIObjectsFromSegImage, maskImage = image, intImage = image, objNumberVector = objNumberVector
  counter = oROI2DGroup->getObjectNumberVector()

    ; creation of an image copy
  ImageCopy = make_array(size(image, /DIMENSIONS), VALUE = 0)
  
  if ((n_elements(counter) eq 1) and (counter[0] eq -1)) then return, image*0 ; Felipe, evalua el peligro de esta linea - susana, cuando no hay ROIs en imagen colapsa el for siguiente
    ;ellipse means 0<eccentricity<1 strictly
  for i=0, n_elements(counter)-1 do begin ; linea original
    Obj = ((oROI2DGroup->get(position = i))->getxyzPoints())[0:1,*]

      ;calculation by hand of each ROI (must be better options)
    sumx = total(Obj[0,*])
    sumy = total(Obj[1,*])
    sumxx = total(Obj[0,*] * Obj[0,*])
    sumyy = total(Obj[1,*] * Obj[1,*])
    sumxy = total(Obj[0,*] * Obj[1,*])
    n = n_elements(Obj[1,*])
    if( n le 1 ) then continue;
      ;calculation of mean, variance and covar
    xmom = moment(Obj[0,*])
    ymom = moment(Obj[1,*])
      ;mean
    xbar = xmom[0]
    ybar = ymom[0]
      ;variance
    varx = xmom[1]
    vary = ymom[1]
      ;covariance
    covarxy=sumxy/n
    dx = Obj[0,*] - xbar
    dy = Obj[1,*] - ybar
    sumdxdx = total(dx*dx)
    sumdydy = total(dy*dy)
    sumdxdy = total(dx*dy)
      ;Ellipse rotation calculation
    theta = 0.5 * atan(2*sumdxdy / (sumdxdx*sumdydy))
    c = cos(theta)
    s = sin(theta)
      ;Let the ellipse without rotation
    X = c*dx - s*dy
    Y = s*dx + c*dy
    sumXX = total(X*X)
    sumYY = total(Y*Y)
    varX = sumXX / n ;Equvalency between a^2 y b^2, must compare which is bigger
    varY = sumYY / n ;Equvalency between a^2 y b^2, must compare which is bigger

      ;Estimated Area of the Ellipse by means of 
    EstimatedArea = !PI*4*sqrt(VarX*VarY)

      ;Area Ratio Values
      ;Real Area of ROI
      ;just the number of pixels      
    ratio = n_elements(Obj[0,*])/EstimatedArea

      ;Overpainting of the image according Area Ratio Values
    imageCopy(Obj[0,*],Obj[1,*])=ceil(100*ratio)
  endfor
  return, imageCopy
end


function C_sImageFilter_EllipsenessEstimator::init
  filterStruct = {Name: 'C_EllipsenessEstimator',$    ; Filter Name.
                  pWidgetType: ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pNames:      ptr_new(),$ ; Pointer on Filter Parameter Names.
                  pActive:     ptr_new(),$ ; Pointer on Filter Parameter Active Bool.
                  pMin:        ptr_new(),$ ; Pointer on Filter Parameter Min_Values.
                  pMax:        ptr_new(),$ ; Pointer on Filter Parameter Max_Values.
                  pValues:     ptr_new()}  ; Pointer on Filter Parameter Values.

  ; Parameters of C_sImageFilter_Eccentricity.   
  filterParamWidgetType = make_array(1, /string, value = 'widget_slider')
  filterParamNames      = ['VOID']
  filterParamActive     = [1]
  filterParamMin        = [0]
  filterParamMax        = [1]
  filterParamValues     = [0]

  filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
  filterStruct.pNames      = ptr_new(filterParamNames, /no_copy)
  filterStruct.pActive     = ptr_new(filterParamActive, /no_copy)
  filterStruct.pMin        = ptr_new(filterParamMin, /no_copy)
  filterStruct.pMax        = ptr_new(filterParamMax, /no_copy)
  filterStruct.pValues     = ptr_new(filterParamValues, /no_copy)

  self.pParamStruct        = ptr_new(filterStruct, /no_copy)
  return, 1
end


pro C_sImageFilter_EllipsenessEstimator__define
  tmp = {C_sImageFilter_EllipsenessEstimator, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
