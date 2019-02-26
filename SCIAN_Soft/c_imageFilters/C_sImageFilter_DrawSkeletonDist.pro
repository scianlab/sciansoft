;_____________________________IOISIOI____________________ (This is art, a friend of mine use to say ;)
; NAME:
;       C_sImageFilter_ThinningFrap
;
; PURPOSE:
;       - Thinning filter for binary images, for new measurements of FRAP experiments
;
; AUTHOR:
;     LBriones 
;
; CALLING SEQUENCE:
;     result = obj_new('C_sImageFilter_ThinningFrap')
; USE:
;     Cluster0 = Normal Segmentation for ER
;     Cluster1 = Normal Segmentation for ER + ThinningACL80l (Parameter to_Frap : 1)+ ThinningFrap
;_______________________IOISIOI____________________ 

function C_sImageFilter_DrawSkeletonDist::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_DrawSkeletonDist::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_DrawSkeletonDist::checkParamsApplied
    ; Compares parameter values between the current values and the values applied in the last segmentation.
    if ptr_valid(self.pParamApplied) then begin
        if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
            *self.pParamApplied = *(*self.pParamStruct).pValues
            return, 1
        endif
    endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
    return, 0
end


function C_sImageFilter_DrawSkeletonDist::apply, image = image,$
                        selectedStackObject = selectedStackObject,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos,$
                        chPos = chPos,$
                        zPos = zPos,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y
  
    wClus1 = (where((*(*self.pParamStruct).pNames) eq 'Cluster Segmentation'))[0]
    Clus1 = (*(*self.pParamStruct).pValues)[wClus1] 
     whR = (where((*(*self.pParamStruct).pNames) eq 'Radio'))[0]
    r = (*(*self.pParamStruct).pValues)[whR]
    whY = (where((*(*self.pParamStruct).pNames) eq 'Y'))[0]
    y = (*(*self.pParamStruct).pValues)[whY]
    whX = (where((*(*self.pParamStruct).pNames) eq 'X'))[0]
    x = (*(*self.pParamStruct).pValues)[whX]        
    whKeep = (where((*(*self.pParamStruct).pNames) eq 'Keep ROI Only'))[0]
    Keep = (*(*self.pParamStruct).pValues)[whKeep]   
   
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum
    pParamStruct = selectedStackObject->getpParamStruct()

    selectedStackObject->getSelectedClusterMask, mask = imageSegTemp, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = fix(Clus1)
   
    if (size(image, /n_dim) ne 2) then return, image

;     If the parameters/input image unchanged since the last segmentation appliance of this filter,
;     there's no need to spend time computing the contours again,
;     and the filter returns the previosly segmented image
    if ptr_valid(self.pImage) then begin
        if (min(*self.pImage eq image) eq 0) then *self.pImage = image $
        else if (not(self->checkParamsApplied()) and ptr_valid(self.pSegImage)) then return, *self.pSegImage
    endif else self.pImage = ptr_new(image)

    
    dimImage = size(image, /dim) 
    thinnedImage = make_array(dimImage, /BYTE, VALUE=0)
    imageSeg = make_array(dimImage, /BYTE, VALUE=0)
    tempImage = make_array(dimImage, /BYTE, VALUE=0)
    tempImageOld = (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = zPos))
    
    if Keep eq 1 then begin
      ;Imagen del ROI frap con las intensidades
      ;punto a b esta dentro si: (a - x)^2 + (b - y)^2 = (r)^2
      for i=x-r, x+r do begin
        for j=y-r, y+r do begin
          if ((i - x)^2 + (j - y)^2) le (r^2) then begin
            thinnedImage[i,j] = image[i,j]
            imageSeg[i,j] = imageSegTemp[i,j]
            tempImage[i,j] = tempImageOld[i,j]
          endif
        endfor
      endfor
    endif else begin
      thinnedImage = image
      imageSeg = imageSegTemp
      tempImage = tempImageOld
    endelse
    ; Normalize to 1
    imageSeg /= max(imageSeg)
    
    thinOnly = thin(thinnedImage, /NEIGHBOR_COUNT)
    
    sumIntensity=make_array(n_elements(where(thinOnly ne 0)))
    contSum=make_array(n_elements(where(thinOnly ne 0)))
    onlyRoiImage = ptrarr((n_elements(where(thinOnly eq 4))+n_elements(where(thinOnly eq 2))),/ALLOCATE_HEAP)
    onlyPosRoi = ptrarr((n_elements(where(thinOnly eq 4))+n_elements(where(thinOnly eq 2))),/ALLOCATE_HEAP)
       
      thinnedImageBackup = thinOnly
      
      vectorRoi = array_indices(dimImage,where(thinOnly ne 0),/DIMENSIONS)
      if n_elements(where(thinOnly ge 4)) eq 1 then begin
          if where(thinOnly ge 4) eq -1 then vectorFork = 0 else vectorFork = array_indices(dimImage,where(thinOnly ge 4),/DIMENSIONS)
      endif else begin
          vectorFork = array_indices(dimImage,where(thinOnly ge 4),/DIMENSIONS)
      endelse
      vectorEnd = array_indices(dimImage,where(thinOnly eq 2 or thinOnly eq 1),/DIMENSIONS)
      dimFork = size(vectorFork,/dim)
      if n_elements(dimFork) eq 1 then dimFork=[dimFork,1] 
      dimEnd = size(vectorEnd,/dim)
      if n_elements(dimEnd) eq 1 then dimEnd=[dimEnd,1] 
      tempImage = tempImage * imageSeg
      
      SegmentationImage = make_array(dimImage, /INT, VALUE=0)
;      vecColor=make_array(size(index,/dim), /INT, VALUE=0)
      if Keep eq 1 then begin
        ;mide distancias desde tempImage a thinnedImage, y asignar intensidad promedio a el pixel correspondiente
        for i=x-r, x+r do begin
          for j=y-r, y+r do begin
            if ((i - x)^2 + (j - y)^2) le (r^2) then begin
              if imageSeg[i,j] eq 1 then begin
                dis=sqrt((vectorRoi[0,*]-i)^2 + (vectorRoi[1,*]-j)^2)
                index=where(dis eq min(dis))
                SegmentationImage[i,j]=index
                contSum[index]++
              endif
            endif
          endfor
        endfor
      endif else begin
        ;mide distancias desde tempImage a thinnedImage, y asignar intensidad total a el pixel sw menos distancia del SKEL
        for i=0, dimImage[0]-1 do begin
          for j=0, dimImage[1]-1 do begin
             if imageSeg[i,j] eq 1 then begin
                dis=sqrt((vectorRoi[0,*]-i)^2 + (vectorRoi[1,*]-j)^2)
                index=where(dis eq min(dis))
                SegmentationImage[i,j]=(index mod 17)
                contSum[index]++
              endif
           endfor
        endfor
      endelse
      dim=size(vectorRoi[0,*],/dim)
      for i=0,dim[1]-1 do begin
        SegmentationImage[vectorRoi[0,i],vectorRoi[1,i]]=1
      endfor
    
    if (ptr_valid(self.pSegImage)) then *self.pSegImage = bytArr(dimImage[0], dimImage[1]) $
        else self.pSegImage = ptr_new(bytArr(dimImage[0], dimImage[1]), /no_copy)
    *self.pSegImage = SegmentationImage
    return, *self.pSegImage
end


function C_sImageFilter_DrawSkeletonDist::init
    filterStruct = { Name: ' C_DrawSkeletonDist',$ ; Filter Name.
                     pWidgetType: ptr_new(),$    ; Pointer on Filter Parameter Names.
                     pNames: ptr_new(),$         ; Pointer on Filter Parameter Names.
                     pActive: ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                     pMin: ptr_new(),$           ; Pointer on Filter Parameter Min_Values.
                     pMax: ptr_new(),$           ; Pointer on Filter Parameter Max_Values.
                     pValues:ptr_new()}        ; Pointer on Filter Parameter Values.

    ; Parameters
    filterParamWidgetType = make_array(5, /string, value = 'widget_slider')
    filterParamNames = ['X', 'Y', 'Radio', 'Keep ROI Only', 'Cluster Segmentation'] ; Not used yet
    filterParamActive = [1 , 1, 1, 0, 1]
    filterParamMin = [0., 0., 0., 0., 0.]
    filterParamMax = [10000., 10000., 10000., 1., 1.]
    filterParamValues = [100., 100., 10., 0., 0.]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_DrawSkeletonDist__define
    tmp = { C_sImageFilter_DrawSkeletonDist,$
            pParamStruct: ptr_new(),$         ; Pointers to keep the last processed image, saves some time by avoiding re-thinning
            pParamApplied: ptr_new(),$
            pSegImage: ptr_new(),$
            pIntImage: ptr_new(),$
            pImage: ptr_new(),$
            inherits C_sImageFilter }
end
