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

function C_sImageFilter_ThinningFrap::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_ThinningFrap::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ThinningFrap::checkParamsApplied
    ; Compares parameter values between the current values and the values applied in the last segmentation.
    if ptr_valid(self.pParamApplied) then begin
        if ((where(*self.pParamApplied ne *(*self.pParamStruct).pValues))[0] ne -1) then begin
            *self.pParamApplied = *(*self.pParamStruct).pValues
            return, 1
        endif
    endif else self.pParamApplied = ptr_new(*(*self.pParamStruct).pValues)
    return, 0
end


function C_sImageFilter_ThinningFrap::apply, image = image,$
                        selectedStackObject = selectedStackObject,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos,$
                        chPos = chPos,$
                        zPos = zPos,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y
  
    whR = (where((*(*self.pParamStruct).pNames) eq 'Radio'))[0]
    r = (*(*self.pParamStruct).pValues)[whR]
    whY = (where((*(*self.pParamStruct).pNames) eq 'Y'))[0]
    y = (*(*self.pParamStruct).pValues)[whY]
    whX = (where((*(*self.pParamStruct).pNames) eq 'X'))[0]
    x = (*(*self.pParamStruct).pValues)[whX]              
    whMk = (where((*(*self.pParamStruct).pNames) eq 'Edited Mask'))[0]
    Mk = (*(*self.pParamStruct).pValues)[whMk]     
    whKeep = (where((*(*self.pParamStruct).pNames) eq 'Keep ROI Only'))[0]
    Keep = (*(*self.pParamStruct).pValues)[whKeep]   
    wClus1 = (where((*(*self.pParamStruct).pNames) eq 'Cluster Segmentation'))[0]
    Clus1 = (*(*self.pParamStruct).pValues)[wClus1]   
   
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum
    pParamStruct = selectedStackObject->getpParamStruct()

    selectedStackObject->getSelectedClusterMask, mask = imageSegTemp, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = fix(Clus1)
   
    if (size(image, /n_dim) ne 2) then return, image

    ; If the parameters/input image unchanged since the last segmentation appliance of this filter,
    ; there's no need to spend time computing the contours again,
    ; and the filter returns the previosly segmented image
;    if ptr_valid(self.pImage) then begin
;        if (min(*self.pImage eq image) eq 0) then *self.pImage = image $
;        else if (not(self->checkParamsApplied()) and ptr_valid(self.pSegImage)) then return, *self.pSegImage
;    endif else self.pImage = ptr_new(image)

    
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
    
    if Mk eq 0 then begin 
      thinOnly = thin(thinnedImage, /NEIGHBOR_COUNT)
    endif else begin
      widget_control, stack_tlb, get_uValue = state, /no_copy
      (*state.pImageStackInfoObject)->get, pParamStruct = pParamStruct
      file = dialog_pickfile( /read, path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]], get_path = path, filter = '*.tif')                    
      thinOnly = READ_IMAGE(file)
    endelse
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
      
      if Keep eq 1 then begin
        ;mide distancias desde tempImage a thinnedImage, y asignar intensidad promedio a el pixel correspondiente
        for i=x-r, x+r do begin
          for j=y-r, y+r do begin
            if ((i - x)^2 + (j - y)^2) le (r^2) then begin
              if imageSeg[i,j] eq 1 then begin
                dis=sqrt((vectorRoi[0,*]-i)^2 + (vectorRoi[1,*]-j)^2)
                index=where(dis eq min(dis))
                sumIntensity[index] = sumIntensity[index]+tempImage[vectorRoi[0,index],vectorRoi[1,index]]
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
                sumIntensity[index] = sumIntensity[index]+tempImage[vectorRoi[0,index],vectorRoi[1,index]]
                contSum[index]++
              endif
           endfor
        endfor
      endelse
      ;tomo el primer pixel antes de un end point como punto de partida para agrupar los roi
      flagFork = 0
      flagEnd = 0

      onlyRoi=make_array(1)
      posRoi=make_array(1)
      nodoRoiEnd = vectorEnd[*,0]
      if thinnedImage[nodoRoiEnd[0]+1,nodoRoiEnd[1]]   eq 1 and thinOnly[nodoRoiEnd[0]+1,nodoRoiEnd[1]] ne 0 then nodoRoi = [nodoRoiEnd[0]+1,nodoRoiEnd[1]]
      if thinnedImage[nodoRoiEnd[0]-1,nodoRoiEnd[1]]   eq 1 and thinOnly[nodoRoiEnd[0]-1,nodoRoiEnd[1]] ne 0 then nodoRoi = [nodoRoiEnd[0]-1,nodoRoiEnd[1]]
      if thinnedImage[nodoRoiEnd[0]+1,nodoRoiEnd[1]+1] eq 1 and thinOnly[nodoRoiEnd[0]+1,nodoRoiEnd[1]+1] ne 0 then nodoRoi = [nodoRoiEnd[0]+1,nodoRoiEnd[1]+1]
      if thinnedImage[nodoRoiEnd[0],nodoRoiEnd[1]+1]   eq 1 and thinOnly[nodoRoiEnd[0],nodoRoiEnd[1]+1] ne 0 then nodoRoi = [nodoRoiEnd[0],nodoRoiEnd[1]+1]
      if thinnedImage[nodoRoiEnd[0],nodoRoiEnd[1]-1]   eq 1 and thinOnly[nodoRoiEnd[0],nodoRoiEnd[1]-1] ne 0 then nodoRoi = [nodoRoiEnd[0],nodoRoiEnd[1]-1]
      if thinnedImage[nodoRoiEnd[0]-1,nodoRoiEnd[1]-1] eq 1 and thinOnly[nodoRoiEnd[0]-1,nodoRoiEnd[1]-1] ne 0 then nodoRoi = [nodoRoiEnd[0]-1,nodoRoiEnd[1]-1]
      if thinnedImage[nodoRoiEnd[0]+1,nodoRoiEnd[1]-1] eq 1 and thinOnly[nodoRoiEnd[0]+1,nodoRoiEnd[1]-1] ne 0 then nodoRoi = [nodoRoiEnd[0]+1,nodoRoiEnd[1]-1]
      if thinnedImage[nodoRoiEnd[0]-1,nodoRoiEnd[1]+1] eq 1 and thinOnly[nodoRoiEnd[0]-1,nodoRoiEnd[1]+1] ne 0 then nodoRoi = [nodoRoiEnd[0]-1,nodoRoiEnd[1]+1]
      vectorRoi[*,where(vectorRoi[0,*] eq nodoRoiEnd[0] and vectorRoi[1,*] eq nodoRoiEnd[1])] = [-1,-1]
      vectorEnd[*,0]= [-1,-1]
      thinOnly[nodoRoiEnd[0],nodoRoiEnd[1]]=0
      contRoi = 0
      contPosRoi = 0
      contEnd = 1
      contFork = 0
      nPixelRoi = 0
      
      while n_elements(where(thinOnly ne 0)) gt 1 do begin
        
        ;Es un fork o un endpoint?
        if thinOnly[nodoRoi[0],nodoRoi[1]] eq 4 then flagFork = 1
        if thinOnly[nodoRoi[0],nodoRoi[1]] eq 2 then flagEnd = 1
        
        ;Agregar intensidad a Roi y eliminarla del vector
        if flagFork eq 0 and flagEnd eq 0 and n_elements(where(thinOnly ne 0)) gt 2 then begin 
          onlyRoi = [onlyRoi, sumIntensity[where(vectorRoi[0,*] eq nodoRoi[0] and vectorRoi[1,*] eq nodoRoi[1])] $
                                 / contSum[where(vectorRoi[0,*] eq nodoRoi[0] and vectorRoi[1,*] eq nodoRoi[1])]]
          posRoi = [posRoi, [vectorRoi[0,where(vectorRoi[0,*] eq nodoRoi[0] and vectorRoi[1,*] eq nodoRoi[1])], $
                             vectorRoi[1,where(vectorRoi[0,*] eq nodoRoi[0] and vectorRoi[1,*] eq nodoRoi[1])]]] 
          vectorRoi[*,where(vectorRoi[0,*] eq nodoRoi[0] and vectorRoi[1,*] eq nodoRoi[1])] = [-1,-1]
          thinOnly[nodoRoi[0],nodoRoi[1]]=0
          nodoDeleted = nodoRoi
          
          if thinOnly[nodoDeleted[0]+1,nodoDeleted[1]]   ne 0 then $ 
              nodoRoi = [nodoDeleted[0]+1,nodoDeleted[1]]
          if thinOnly[nodoDeleted[0]-1,nodoDeleted[1]]   ne 0 then $ 
              nodoRoi = [nodoDeleted[0]-1,nodoDeleted[1]] 
          if thinOnly[nodoDeleted[0]+1,nodoDeleted[1]+1] ne 0 then $ 
              nodoRoi = [nodoDeleted[0]+1,nodoDeleted[1]+1] 
          if thinOnly[nodoDeleted[0],nodoDeleted[1]+1]   ne 0 then $ 
              nodoRoi = [nodoDeleted[0],nodoDeleted[1]+1] 
          if thinOnly[nodoDeleted[0],nodoDeleted[1]-1]   ne 0 then $ 
              nodoRoi = [nodoDeleted[0],nodoDeleted[1]-1] 
          if thinOnly[nodoDeleted[0]-1,nodoDeleted[1]-1] ne 0 then $ 
              nodoRoi = [nodoDeleted[0]-1,nodoDeleted[1]-1] 
          if thinOnly[nodoDeleted[0]+1,nodoDeleted[1]-1] ne 0 then $ 
              nodoRoi = [nodoDeleted[0]+1,nodoDeleted[1]-1] 
          if thinOnly[nodoDeleted[0]-1,nodoDeleted[1]+1] ne 0 then $ 
              nodoRoi = [nodoDeleted[0]-1,nodoDeleted[1]+1] 
          
          flagFork = 0
          flagEnd = 0
        endif else begin
          ;Guardo cada valor de intensidad del ROI
          *onlyRoiImage[contRoi] = onlyRoi
          *onlyPosRoi[contPosRoi] = posRoi
          onlyRoi=0
          posRoi=0
          contRoi++
          contPosRoi++
                    
          k=0
          flagEnd = 0
          flagFork = 0
            if contEnd lt dimEnd[1] then begin
              ;TODO Este if nunca se ocupa... revisar
              if flagEnd eq 1 then begin
                vectorRoi[*,where(vectorRoi[0,*] eq nodoRoi[0] and vectorRoi[1,*] eq nodoRoi[1])] = [-1,-1]
                vectorEnd[*,where(vectorEnd[0,*] eq nodoRoi[0] and vectorEnd[1,*] eq nodoRoi[1])]= [-1,-1]
                thinOnly[nodoRoi[0],nodoRoi[1]]=0
              endif  
              noRepeat=0  
              while noRepeat eq 0 do begin
                while k lt dimEnd[1] and flagEnd eq 0 do begin
                  
                  if vectorEnd[0,k] ne -1 then begin 
                    nodoRoiEnd = vectorEnd[*,k]
                    flagEnd = 1
                  endif
                  k++
                endwhile    
              ;nuevo nodo para seguir guardando intensidadas
                if thinnedImage[nodoRoiEnd[0]+1,nodoRoiEnd[1]]   eq 1 and thinOnly[nodoRoiEnd[0]+1,nodoRoiEnd[1]] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0]+1,nodoRoiEnd[1]]
                noRepeat=1
                endif
                if thinnedImage[nodoRoiEnd[0]-1,nodoRoiEnd[1]]   eq 1 and thinOnly[nodoRoiEnd[0]-1,nodoRoiEnd[1]] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0]-1,nodoRoiEnd[1]]
                noRepeat=1
                endif
                if thinnedImage[nodoRoiEnd[0]+1,nodoRoiEnd[1]+1] eq 1 and thinOnly[nodoRoiEnd[0]+1,nodoRoiEnd[1]+1] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0]+1,nodoRoiEnd[1]+1]
                noRepeat=1
                endif
                if thinnedImage[nodoRoiEnd[0],nodoRoiEnd[1]+1]   eq 1 and thinOnly[nodoRoiEnd[0],nodoRoiEnd[1]+1] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0],nodoRoiEnd[1]+1]
                noRepeat=1
                endif
                if thinnedImage[nodoRoiEnd[0],nodoRoiEnd[1]-1]   eq 1 and thinOnly[nodoRoiEnd[0],nodoRoiEnd[1]-1] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0],nodoRoiEnd[1]-1]
                noRepeat=1
                endif
                if thinnedImage[nodoRoiEnd[0]-1,nodoRoiEnd[1]-1] eq 1 and thinOnly[nodoRoiEnd[0]-1,nodoRoiEnd[1]-1] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0]-1,nodoRoiEnd[1]-1]
                noRepeat=1
                endif
                if thinnedImage[nodoRoiEnd[0]+1,nodoRoiEnd[1]-1] eq 1 and thinOnly[nodoRoiEnd[0]+1,nodoRoiEnd[1]-1] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0]+1,nodoRoiEnd[1]-1]
                noRepeat=1
                endif
                if thinnedImage[nodoRoiEnd[0]-1,nodoRoiEnd[1]+1] eq 1 and thinOnly[nodoRoiEnd[0]-1,nodoRoiEnd[1]+1] ne 0 then begin 
                nodoRoi = [nodoRoiEnd[0]-1,nodoRoiEnd[1]+1]
                noRepeat=1
                endif
                vectorRoi[*,where(vectorRoi[0,*] eq nodoRoiEnd[0] and vectorRoi[1,*] eq nodoRoiEnd[1])] = [-1,-1]
                vectorEnd[*,where(vectorEnd[0,*] eq nodoRoiEnd[0] and vectorEnd[1,*] eq nodoRoiEnd[1])]= [-1,-1]
                thinOnly[nodoRoiEnd[0],nodoRoiEnd[1]]=0
                contEnd++
                flagEnd=0
              endwhile
            endif else begin
             
              while k lt dimFork[1] and flagFork eq 0 do begin
                
                if vectorFork[0,k] ne -1 then begin 
                  nodoRoiFork = vectorFork[*,k]
                  vectorRoi[*,where(vectorRoi[0,*] eq nodoRoiFork[0] and vectorRoi[1,*] eq nodoRoiFork[1])] = [-1,-1]
                  vectorFork[*,where(vectorFork[0,*] eq nodoRoiFork[0] and vectorFork[1,*] eq nodoRoiFork[1])]= [-1,-1]
                  thinOnly[nodoRoiFork[0],nodoRoiFork[1]]=0
                endif
                
                nodoRoi=0
                if thinOnly[nodoRoiFork[0]+1,nodoRoiFork[1]]   eq 3 then nodoRoi = [nodoRoiFork[0]+1,nodoRoiFork[1]]
                if thinOnly[nodoRoiFork[0]-1,nodoRoiFork[1]]   eq 3 then nodoRoi = [nodoRoiFork[0]-1,nodoRoiFork[1]]
                if thinOnly[nodoRoiFork[0]+1,nodoRoiFork[1]+1] eq 3 then nodoRoi = [nodoRoiFork[0]+1,nodoRoiFork[1]+1]
                if thinOnly[nodoRoiFork[0],nodoRoiFork[1]+1]   eq 3 then nodoRoi = [nodoRoiFork[0],nodoRoiFork[1]+1]
                if thinOnly[nodoRoiFork[0],nodoRoiFork[1]-1]   eq 3 then nodoRoi = [nodoRoiFork[0],nodoRoiFork[1]-1]
                if thinOnly[nodoRoiFork[0]-1,nodoRoiFork[1]-1] eq 3 then nodoRoi = [nodoRoiFork[0]-1,nodoRoiFork[1]-1]
                if thinOnly[nodoRoiFork[0]+1,nodoRoiFork[1]-1] eq 3 then nodoRoi = [nodoRoiFork[0]+1,nodoRoiFork[1]-1]
                if thinOnly[nodoRoiFork[0]-1,nodoRoiFork[1]+1] eq 3 then nodoRoi = [nodoRoiFork[0]-1,nodoRoiFork[1]+1]
                
                if n_elements(nodoRoi) gt 1 then flagFork = 1
                k++
             endwhile
             
           endelse
          
          flagFork = 0
          flagEnd = 0
        endelse
        
      endwhile
      sumIntensity[*] = 0.
      contSum[*] = 0.
    
    ;Guardar archivo con intensidades de cada Roi.
    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]
    file = strCompress(path + strcompress('T_' + string(tPos) + '_Intensity_x' + string(fix(x)) + '_y' + string(fix(y)) + '_r' + string(fix(r)) + '_mk1.dat', /rem))        
    
    get_lun, P
    openW, P, file
    for i=0, controi-1 do begin
      if n_elements((*onlyRoiImage[i])) gt 1 then temp=(*onlyRoiImage[i])[1:*]
      if n_elements((*onlyRoiImage[i])) gt 1 then printF,P, temp, format = strcompress('(',/rem) + strcompress(string(n_elements((*onlyRoiImage[i])[1:*])),/rem) + strcompress('F10.3',/rem) +strcompress(')' ,/rem)
    endfor
    close, P 
    free_lun, P
    
    if tPos eq 0 then begin
      ;Guardar archivo con intensidades de cada Roi.
      path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]
      file = strCompress(path + strcompress('coordinates_Roi_Intensity_x' + string(fix(x)) + '_y' + string(fix(y)) + '_r' + string(fix(r)) + '_mk1.dat', /rem))         
      
      get_lun, F
      openW, F, file
      for i=0, contPosRoi-1 do begin
        if n_elements((*onlyRoiImage[i])) gt 1 then temp=(*onlyPosRoi[i])[1:*]
        if n_elements((*onlyRoiImage[i])) gt 1 then printF,F, temp, format = strcompress('(',/rem) + strcompress(string(n_elements((*onlyPosRoi[i])[1:*])),/rem) + strcompress('F10.3',/rem) +strcompress(')' ,/rem)
      endfor
      close, F 
      free_lun, F
    endif
      

    ptr_free, onlyPosRoi
    ptr_free, onlyRoiImage
    
    segmentation = make_Array(dimImage, value=0)
    segmentation[where(thinnedImageBackup eq 3)] = thinnedImageBackup[where(thinnedImageBackup eq 3)]
    
    if (ptr_valid(self.pSegImage)) then *self.pSegImage = bytArr(dimImage[0], dimImage[1]) $
        else self.pSegImage = ptr_new(bytArr(dimImage[0], dimImage[1]), /no_copy)
    *self.pSegImage = segmentation
    return, *self.pSegImage
end


function C_sImageFilter_ThinningFrap::init
    filterStruct = { Name: ' C_ThinningFrap',$ ; Filter Name.
                     pWidgetType: ptr_new(),$    ; Pointer on Filter Parameter Names.
                     pNames: ptr_new(),$         ; Pointer on Filter Parameter Names.
                     pActive: ptr_new(),$        ; Pointer on Filter Parameter Active Bool.
                     pMin: ptr_new(),$           ; Pointer on Filter Parameter Min_Values.
                     pMax: ptr_new(),$           ; Pointer on Filter Parameter Max_Values.
                     pValues:ptr_new()}        ; Pointer on Filter Parameter Values.

    ; Parameters of C_ThinningACL80.
    filterParamWidgetType = make_array(6, /string, value = 'widget_slider')
    filterParamNames = ['X', 'Y', 'Radio', 'Edited Mask', 'Keep ROI Only', 'Cluster Segmentation'] ; Not used yet
    filterParamActive = [1 , 1, 1, 0, 1,1]
    filterParamMin = [0., 0., 0., 0., 0.,0]
    filterParamMax = [10000., 10000., 10000., 1., 1.,1000]
    filterParamValues = [100., 100., 10., 0., 0.,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ThinningFrap__define
    tmp = { C_sImageFilter_ThinningFrap,$
            pParamStruct: ptr_new(),$         ; Pointers to keep the last processed image, saves some time by avoiding re-thinning
            pParamApplied: ptr_new(),$
            pSegImage: ptr_new(),$
            pIntImage: ptr_new(),$
            pImage: ptr_new(),$
            inherits C_sImageFilter }
end
