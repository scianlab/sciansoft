;_____________________________IOISIOI____________________
; NAME:
; C_sImageFilter_AC_TSnakes
;
; PURPOSE:
; - TSnakes-Filter-Class.
;
; CALLING SEQUENCE:
;
; result = obj_new('C_sImageFilter_AC_TSnakes')
;
; METHODS:
;
; function->apply, pImageData = pImageData ;pImageData Pointer on Image Data Matrix
; pro->set, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
; pro->get, pParamStruct = pParamStruct    ;pParamStruct Pointer on ImageFilterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_AC_TSnakes::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_AC_TSnakes::calcWinding, XCOORDS = xCoords, YCOORDS = yCoords
  area = 0
  for i = 0l, size(xCoords, /n_elements) - 2 do begin 
    area  = area + xCoords[i]*yCoords[i+1] - xCoords[i+1]*yCoords[i]
  endfor
  
  if area lt 0 then return, 1 else return, -1
end

function C_sImageFilter_AC_TSnakes::checkParamsApplied
; compares the parameter values between the current values and the values applied in the last segmentation
   if ptr_valid(self.pParamApplied) then begin
      if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
         *self.pParamApplied = *(*self.pParamStruct).pValues
         return, 1
      endif
   endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
   return, 0
end

function C_sImageFilter_AC_TSnakes::apply, $
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
     
   paramNames = *(*self.pParamStruct).pNames
   
   wherePlay  = (where(paramNames eq 'Play'))[0]
   play = (*(*self.pParamStruct).pValues)[wherePlay]
   if(play eq 0) then return, image
   if(play eq 1) then (*(*self.pParamStruct).pValues)[wherePlay] = 0
									   
   ; check the input image dimensions
   if (size(image, /n_dim) ne 2) or (max(image) eq 0) then begin 
     print, "Image did not satisfy dimension conditions, returning original image"
     return, image
   endif
   
   szI = size(image, /dim)
   
   if (szI[0] le 20) or (szI[1] le 20) then begin
     print, "Image did not satisfy size condition, please select a bigger rectangle, returning original image"
     return, image
   endif
   
   if (max(image) gt 255) or (min(image) lt 0) then begin  
     image = (image - min(image))
     image = fix((image / max(image) * 255))
   endif  

;   whParam = (where((*(*self.pParamStruct).pNames) eq 'Keep Segmented Image'))[0]
;   if (whParam eq -1) then fKeepSegImage = 0b else $
;      if not((*(*self.pParamStruct).pActive)[whParam]) then fKeepSegImage = 0b else fKeepSegImage = (*(*self.pParamStruct).pValues)[whParam]
;
;      ; if parameters/input image unchanged since the last segmentation appliance of this filter,
;      ; there's no need to spend time computing the contours again and return the previosly segmented image
;   if ( not(self->checkParamsApplied()) and ptr_valid(self.pSegImage) ) then begin
;      szSeg = size(*self.pSegImage, /dim)
;      if (n_elements(szSeg) gt 1) then if ((szI[0] eq szSeg[0]) and (szI[1] eq szSeg[1])) then return, *self.pSegImage
;   endif



   ; get parameters for the filter
   
   whereMode        = (where(paramNames eq 'Mode (0 Seed, 1 Bitmap, 2 Border)'))[0]
   whereAlpha       = (where(paramNames eq 'Alpha'))[0]
   whereElastFactor = (where(paramNames eq 'Elasticity Factor'))[0]
   whereRigFactor   = (where(paramNames eq 'Rigidity Factor'))[0]
   whereInt1Lft     = (where(paramNames eq 'Interior 1 Left Side'))[0]
   whereInt1Rgt     = (where(paramNames eq 'Interior 1 Right Side'))[0] 
   whereInt2Lft     = (where(paramNames eq 'Interior 2 Left Side'))[0]
   whereInt2Rgt     = (where(paramNames eq 'Interior 2 Right Side'))[0]    
   whereDefSteps    = (where(paramNames eq 'Deformation Steps'))[0]
   whereEvolStep    = (where(paramNames eq 'Evolution Step'))[0]
   whereEvolIts     = (where(paramNames eq 'Evolution Iterations'))[0]
   whereResFactor   = (where(paramNames eq 'Resolution Factor'))[0]
   whereGvfWeight   = (where(paramNames eq 'GVF Weight'))[0]
   whereGvfStep     = (where(paramNames eq 'GVF Step'))[0]
   whereGvfErr      = (where(paramNames eq 'GVF Error Tolerance'))[0]
   whereGvfMaxIt    = (where(paramNames eq 'GVF Maximum Iterations'))[0]
   whereGvfNoiseCut = (where(paramNames eq 'GVF Noise Cut Point'))[0]   
   whereGvfCutRange = (where(paramNames eq 'GVF Noise Range of Cut'))[0]
   whereInflWeight  = (where(paramNames eq 'Inflation Force Weigth'))[0]
   whereSeedRadius  = (where(paramNames eq 'Seed snake radius'))[0]
   whereSegPos      = (where((*(*self.pParamStruct).pNames) eq 'Segmentation Position'))[0]


;   whParam = (where((*(*self.pParamStruct).pNames) eq 'Display_Contour_Windows'))[0]
;   if (*(*self.pParamStruct).pActive)[whParam] then begin
;      displayWindows = round((*(*self.pParamStruct).pValues)[whParam])
;      (*(*self.pParamStruct).pValues)[whParam] = displayWindows
;   endif else displayWindows = -1


   ; determine the size ratio between image dims and real dims
   pParamStruct = selectedStackObject->getpParamStruct()
   whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [pixel]'))[0]
   if (whereDim ne -1) then pixelXDim = *(((*pParamStruct).pValues)[whereDim]) else pixelXDim = 1
   whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [pixel]'))[0]
   if (whereDim ne -1) then pixelYDim = *(((*pParamStruct).pValues)[whereDim]) else pixelYDim = 1
   whereDim = (where((*(*pParamStruct).pNames) eq 'x-Size [real]'))[0]
   if (whereDim ne -1) then realXDim = *(((*pParamStruct).pValues)[whereDim]) else realXDim = pixelXDIM
   whereDim = (where((*(*pParamStruct).pNames) eq 'y-Size [real]'))[0]
   if (whereDim ne -1) then realYDim = *(((*pParamStruct).pValues)[whereDim]) else realYDim = pixelYDIM
   whereDim = (where((*(*pParamStruct).pNames) eq 'z-Interval [real]'))[0]
   if (whereDim ne -1) then realZDim = *(((*pParamStruct).pValues)[whereDim]) else realZDim = 1

   xSizeRatio = pixelXdim / realXdim
   ySizeRatio = pixelYdim / realYdim

   ; create the ROI object to get the masks, images, ...
   oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [pixelXdim, pixelYdim, 1], ZSliceInterval = realZDim)

   oROI2DGroup->addROIObjectsFromSegImage, maskImage = image, intImage = image, objNumberVector = objNumberVector
   totalContours = oROI2DGroup->count()

   segPos = round((*(*self.pParamStruct).pValues)[whereSegPos]) < segPos
   (*(*self.pParamStruct).pValues)[whereSegPos] = segPos

   clusterPos = 4
   selectedStackObject->getSelectedClusterMask, mask = intImage, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusterPos
   if ((n_elements(intImage) eq 1) and (intImage[0] eq -1)) then begin
       oImage = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)
       intImage = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                            tPos = tPos,$
                                                            chPos = chPos,$
                                                            zPos = zPos,$
                                                            clusPos = clusterPos,$
                                                            segPos = segPos - 1, $
                                                            cut_x = cut_x, cut_y = cut_y)
   endif else intImage = intImage[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
   
   mode = (*(*self.pParamStruct).pValues)[whereMode]

   print, 'totalContours =', totalContours
      snake = obj_new(            $
                'C_sTSnakes',     $
                PIMAGE      = ptr_new(intImage),                                $
                PNPTS       = ptr_new(),                                        $
                PXCOORDS    = ptr_new(),                                        $
                PYCOORDS    = ptr_new(),                                        $
                PU          = ptr_new(),                                        $
                PV          = ptr_new(),                                        $
                ELASTFACTOR = (*(*self.pParamStruct).pValues)[whereElastFactor],$
                RIGFACTOR   = (*(*self.pParamStruct).pValues)[whereRigFactor],  $
                INT1LFTSIDE = (*(*self.pParamStruct).pValues)[whereInt1Lft],    $
                INT1RGTSIDE = (*(*self.pParamStruct).pValues)[whereInt1Rgt],    $
                INT2LFTSIDE = (*(*self.pParamStruct).pValues)[whereInt2Lft],    $
                INT2RGTSIDE = (*(*self.pParamStruct).pValues)[whereInt2Rgt],    $
                EVOLSTEP    = (*(*self.pParamStruct).pValues)[whereEvolStep],   $
                EVOLITS     = (*(*self.pParamStruct).pValues)[whereEvolIts],    $
                RESFACTOR   = (*(*self.pParamStruct).pValues)[whereResFactor],  $
                GVFWEIGHT   = (*(*self.pParamStruct).pValues)[whereGvfWeight],  $
                GVFSTEP     = (*(*self.pParamStruct).pValues)[whereGvfStep],    $
                GVFERROR    = (*(*self.pParamStruct).pValues)[whereGvfErr],     $
                GVFMAXIT    = (*(*self.pParamStruct).pValues)[whereGvfMaxIt],   $
                GVFNOISECUT = (*(*self.pParamStruct).pValues)[whereGvfNoiseCut],$
                GVFCUTRANGE = (*(*self.pParamStruct).pValues)[whereGvfCutRange],$
                INFLWEIGHT  = (*(*self.pParamStruct).pValues)[whereInflWeight], $
                SEEDRADIUS  = (*(*self.pParamStruct).pValues)[whereSeedRadius]  $
      )

      if not(obj_valid(snake)) then return, image
        
      snake->calcGVF

      ; create the mask image for this filter
      if ptr_valid(self.pSegImage) then *self.pSegImage = bytArr(szI[0], szI[1]) $
         else self.pSegImage = ptr_new(bytArr(szI[0], szI[1]), /no_copy)

      ; an arbitrary value to amplify the display image (only for this filter)
      imageArea = szI[0] * szI[1]
      if imageArea le 10000 then f = 7 else f = 3
      if imageArea gt 160000 then f = 1

         ; window dimensions
      xWinSize = szI[0] * f
      yWinSize = szI[1] * f

;      if (displayWindows gt -1)  then begin
;         winID = 0
;         window, winID, xpos=0, ypos=0, xsize = xWinSize, ysize = yWinSize
;         ;tv, congrid(intImage, xWinSize, yWinSize)
;      endif
     
;     if(totalContours ...)
      if(mode eq 0) then begin
        snake->makeContours, AUTOINIT = 1
      endif else begin 
        if mode eq 1 then begin 
          max_lenght = 0
          n_polygons = 0
          for i = 0, totalContours-1 do begin
            pBorderPolygon = (oROI2DGroup->get(position = i))->getpObjectBorderPolygonList()
            for j = 0, (size(pBorderPolygon, /dim))[0]-1 do begin
              current_x = transpose((*pBorderPolygon[j])[0,*])
              max_lenght = max_lenght > n_elements(current_x) ; Take the maximum
              print, "max = " , max_lenght
              n_polygons = n_polygons + 1
            endfor
          endfor
          
          print, "max_length =" ,  max_lenght
          print, "n_polygons =", n_polygons
          
          snakes = dblarr(max_lenght+1, 2*n_polygons)
          
          
          current_poly = 0
          for i = 0, totalContours-1 do begin
            pBorderPolygon = (oROI2DGroup->get(position = i))->getpObjectBorderPolygonList()
            for j = 0, (size(pBorderPolygon, /dim))[0]-1 do begin
              current_x = transpose((*pBorderPolygon[j])[0,*])
              current_y = transpose((*pBorderPolygon[j])[1,*])
              snakes[0,current_poly] = size(current_x, /N_ELEMENTS)
              snakes[0,current_poly+1] = size(current_y, /N_ELEMENTS)
              
              snakes[1:(size(current_x, /N_ELEMENTS)), current_poly] = current_x
              snakes[1:(size(current_y, /N_ELEMENTS)), current_poly+1] = current_y
              
              current_poly = current_poly+2
     
            endfor
          endfor
          
          
          print, snakes
          
          
          
          snake->makeContours, AUTOINIT = 0, INITSNAKES = snakes
        endif
       
      endelse
     
      snakes = snake->getSnakes()
        
      xCoords = snakes[0]
      yCoords = snakes[1]
        
      if(xCoords eq ptr_new()) then begin
        return, image
      endif else begin
        for i = 0l, size(*xCoords, /n_elements)-1 do begin
          winding = self->calcWinding(XCOORDS = *(*xCoords)[i], YCOORDS = *(*yCoords)[i])
          contourRoi = obj_new('IDLanROI', *(*xCoords)[i], *(*yCoords)[i])
          if(winding eq 1) then begin
            mask = (contourRoi->computeMask(mask_rule = maskRule, dimensions = szI[0:1]))
            *self.pSegImage  = *self.pSegImage * (1-mask/255)
          endif else begin
            *self.pSegImage = contourRoi->computeMask(mask_rule = 2, mask_in = *self.pSegImage)
          endelse
            
          obj_destroy, contourRoi
        endfor
      endelse
      
      


      ; iterate for each ROI object in the input mask to compute the contours
;      for i = 0, totalContours-1 do begin
;         pBorderPolygon = (oROI2DGroup->get(position = i))->getpObjectBorderPolygonList()
;         ;xPos = transpose((*pBorderPolygon[0])[0,*])
;         ;yPos = transpose((*pBorderPolygon[0])[1,*])
;         ;snake->setSnakeCoords, xPos, yPos
;
;            ; adjust contours for object (*pBorderPolygon[j = 0]) and holes (*pBorderPolygon[j > 0])
;         for j = 0, (size(pBorderPolygon, /dim))[0]-1 do begin
;
;            snake->setContour, transpose((*pBorderPolygon[j])[0,*]), transpose((*pBorderPolygon[j])[1,*])
;            npts = round((snake->getPerimeter()) * perimeterFactor) > 16
;            snake->arcSample, points = npts
;            ;intCoords = snake->getCoords()
;            ;xInt = intCoords[0]
;            ;xInt = intCoords[1]
;            result = snake->adjustContour(plotContour = (displayWindows gt -1))
;            x = snake->getXcoords()
;            y = snake->getYcoords()
;
;             ; plot the final contours
;            if (displayWindows gt -1) then begin
;              ;plot, 0.5 + xPos, 0.5 + yPos, xStyle = 1, yStyle = 1, xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 175, /noErase
;              ;plot, 0.5 + xInt, 0.5 + yInt, xStyle = 1, yStyle = 1, xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 175, /noErase
;               plot, [x,x[0]], [y,y[0]], xStyle = 1, yStyle = 1,xRange = [0, szI[0]], yRange = [0, szI[1]], position = [0, 0, xWinSize, yWinSize], color = 255,/noErase, thick = 3
;            endif
;
;               ; generate the ROI image
;            contourRoi = obj_new('IDLanROI', x, y)
;            if (j eq 0) then *self.pSegImage = contourRoi->computeMask(mask_rule = 2, mask_in = *self.pSegImage) gt 0 $
;               else *self.pSegImage -= ((contourRoi->computeMask(mask_rule = 2, dimensions = szI[0:1])) gt 0)
;            obj_destroy, contourROI
;
;            if ptr_valid(pBorderPolygon[j]) then ptr_free, pBorderPolygon[j]
;         endfor
;      endfor

;      if (displayWindows gt -1) then begin
;         pGGVF = snake->getpGGVF()
;         iimage, intImage
;         ivector, *pGGVF.pU, *pGGVF.pV, /overplot
;      endif

      ; display segmentation results
;      if (displayWindows eq 1) then begin
;         showImage = congrid(100*image, xWinSize, yWinSize)
;         showImage = tvRd()
;         wDelete, winID
;      endif

      if obj_valid(oROI2DGroup) then obj_destroy, oROI2DGroup
   obj_destroy, snake
;   if not(fKeepSegImage) then begin
;      segImage = *self.pSegImage
;      *self.pSegImage = [0]
;      return, segImage
;   endif else begin
;      return, *self.pSegImage
;   endelse
  return, *self.pSegImage
end


function C_sImageFilter_AC_TSnakes::init

   ImageFilterStruct = {Name:'C_AC_TSnakes',$ ; Filter Name.
                        pWidgetType:ptr_new(),$   ; Pointer on Filter Parameter Widget Type.
                        pNames:ptr_new(),$        ; Pointer on Filter Parameter Names.
                        pActive:ptr_new(),$       ; Pointer on Filter Parameter Active Bool.
                        pMin:ptr_new(),$          ; Pointer on Filter Parameter Min_Values.
                        pMax:ptr_new(),$          ; Pointer on Filter Parameter Max_Values.
                        pDeltas:ptr_new(),$       ; Pointer on Filter Parameter Max_Values.
                        pValues:ptr_new()}        ; Pointer on Filter Parameter Values.

      ; Parameters of TSnakes.
   FilterParamNames = [                         $
                       'Play',$
                       'Mode (0 Seed, 1 Bitmap, 2 Border)',$
                       'Seed snake radius',                $
                       'Segmentation Position',            $              
                       'Elasticity Factor',                $
                       'Rigidity Factor',                  $
                       'Interior 1 Left Side',             $
                       'Interior 1 Right Side',            $ 
                       'Interior 2 Left Side',             $
                       'Interior 2 Right Side',            $ 
                       'Deformation Steps',                $
                       'Evolution Step',                   $
                       'Evolution Iterations',             $
                       'Resolution Factor',                $
                       'GVF Weight',                       $
                       'GVF Step',                         $
                       'GVF Error Tolerance',              $
                       'GVF Maximum Iterations',           $
                       'GVF Noise Cut Point'   ,           $
                       'GVF Noise Range of Cut',           $
                       'Inflation Force Weigth'            $
                                
   ]
   FilterParamWidgetType = make_array(n_elements(FilterParamNames), /string, value = 'widget_slider')

   FilterParamValues = [       $      
                        0,      $; Run single op
                        1,      $; Mode (0 Seed, 1 Bitmap, 2 Border)
                        3.0D,   $; Seed snake radius
                        0b,     $; Segmentation Position
                        20.0D,  $; Elasticity Factor
                        5.0D,   $; Rigidity Factor
                        0,      $; Interior 1 Left Side
                        90,     $; Interior 1 Right Side
                        0,      $; Interior 2 Left Side
                        90,     $; Interior 2 Right Side
                        5,      $; Deformation Steps
                        0.001D, $; Evolution Step
                        40,     $; Evolution Iterations
                        0.4D,   $; Resolution Factor
                        50.0D,  $; GVF Weight
                        0.001D, $; GVF Step
                        0.001D, $; GVF Error Tolerance
                        30,     $; GVF Maximum Iterations
                        0.4D,   $; GVF Noise Cut Point
                        0.001D, $; GVF Noise Range of Cut
                        49.0D   $; Inflation Force Weight
                        
   ]

   FilterParamActive = [  $
                        1,$; 
                        1,$; Mode (0 Seed, 1 Bitmap, 2 Border)
                        1,$; Seed snake radius
                        1,$; Segmentation Position
                        1,$; Elasticity Factor
                        1,$; Rigidity Factor
                        1,$; Interior 1 Left Side
                        1,$; Interior 1 Right Side
                        1,$; Interior 2 Left Side
                        1,$; Interior 2 Right Side
                        1,$; Deformation Steps
                        1,$; Evolution Step
                        1,$; Evolution Iterations
                        1,$; Resolution Factor
                        1,$; GVF Weight
                        1,$; GVF Step
                        1,$; GVF Error Tolerance
                        1,$; GVF Maximum Iterations
                        1,$; GVF Noise Cut Point
                        1,$; GVF Noise Range of Cut
                        1 $; Inflation Force Weight
   ]

   FilterParamMin = [              $
                        0,         $;  not run
                        0,         $; Mode (0 Seed, 1 Bitmap, 2 Border)
                        0.1D,      $; Seed snake radius
                        0,         $; Segmentation Position
                        0.01,      $; Elasticity Factor
                        0.01,      $; Rigidity Factor
                        0,         $; Interior 1 Left Side
                        1,         $; Interior 1 Right Side
                        0,         $; Interior 2 Left Side
                        1,         $; Interior 2 Right Side
                        1,         $; Deformation Steps
                        0.000001D, $; Evolution Step
                        1,         $; Evolution Iterations
                        0.1D,      $; Resolution Factor
                        0.0D,      $; GVF Weight
                        0.000001D, $; GVF Step
                        0.00001D,  $; GVF Error Tolerance
                        1,         $; GVF Maximum Iterations
                        0.0000001D,$; GVF Noise Cut Point
                        0.000001D, $; GVF Noise Range of Cut
                        0.0D       $; Inflation Force Weight
   ]

   FilterParamMax = [             $
                        2,        $; 2 run ever... 1 run single
                        2,        $; Mode (0 Seed, 1 Bitmap, 2 Border)
                        100.0D,   $; Seed snake radius
                        20b,      $; Segmentation Position
                        90.0,     $; Elasticity Factor
                        89.0,     $; Rigidity Factor
                        254,      $; Interior 1 Left Side
                        255,      $; Interior 1 Right Side
                        254,      $; Interior 2 Left Side
                        255,      $; Interior 2 Right Side
                        20,       $; Deformation Steps
                        0.5D,     $; Evolution Step
                        2000,     $; Evolution Iterations
                        5.0D,     $; Resolution Factor
                        100.0D,   $; GVF Weight
                        0.00001D, $; GVF Step
                        0.00001D, $; GVF Error Tolerance
                        5000l,    $; GVF Maximum Iterations
                        0.99D,    $; GVF Noise Cut Point
                        0.5D,     $; GVF Noise Range of Cut
                        100.0D    $; Inflation Force Weight
   ]

   FilterParamDeltas = [          $
                        1,        $; 
                        1,        $; Mode (0 Seed, 1 Bitmap, 2 Border)
                        0.01,     $; Seed snake radius
                        0.01,     $; Segmentation Position
                        0.01,     $; Elasticity Factor
                        0.01,     $; Rigidity Factor
                        1,        $; Interior 1 Left Side
                        1,        $; Interior 1 Right Side
                        1,        $; Interior 2 Left Side
                        1,        $; Interior 2 Right Side
                        1,        $; Deformation Steps
                        0.001D,   $; Evolution Step
                        1,        $; Evolution Iterations
                        0.1D,     $; Resolution Factor
                        0.1D,     $; GVF Weight
                        0.001D,   $; GVF Step
                        0.001D,   $; GVF Error Tolerance
                        1l,       $; GVF Maximum Iterations
                        0.01D,    $; GVF Noise Cut Point
                        0.001D,   $; GVF Noise Range of Cut
                        0.01D     $; Inflation Force Weight
   ]

   ImageFilterStruct.pWidgetType = ptr_new(FilterParamWidgetType, /no_copy)
   ImageFilterStruct.pActive = ptr_new(FilterParamActive, /no_copy)
   ImageFilterStruct.pValues = ptr_new(FilterParamValues, /no_copy)
   ImageFilterStruct.pDeltas = ptr_new(FilterParamDeltas, /no_copy)
   ImageFilterStruct.pNames = ptr_new(FilterParamNames, /no_copy)
   ImageFilterStruct.pMin = ptr_new(FilterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(FilterParamMax, /no_copy)

   self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
   return, 1
end


pro C_sImageFilter_AC_TSnakes__define
   tmp = {C_sImageFilter_AC_TSnakes,$
          pParamStruct:ptr_new(),$
          pParamApplied:ptr_new(),$
          pSegImage:ptr_new(),$
          inherits C_sImageFilter}
end