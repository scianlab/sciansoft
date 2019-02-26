;_____________________________IOISIOI____________________
; NAME:
;      C_sFrapThinning
;
; PURPOSE:
;       - Frap Thinning
;
; AUTHOR:
;     lbriones
; 
;
; CALLING SEQUENCE:
;        result = Obj_New('C_sFrapThinning' )
;
; METHOHDS:
;   based on C_sCircle class
;  PRO               ->draw, pImageData = pImageData                       ;pImageData             Pointer on Image Data Matrix
;  PRO               ->SetProperty, x=x, y=y, r=r, color = color        ; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;  PRO               ->GetProperty, x=x, y=y, r=r, color = color        ; x: x-Position of centre,  y: y-Position of centre,  r: radius, color:colorindex
;_____________________________IOISIOI____________________


pro C_sFrapThinning::SetProperty, x=x, y=y, r=r, dr = dr, color = color
   if (n_elements(x) ne 0) then self.x = x
   if (n_elements(y) ne 0) then self.y = y
   if (n_elements(r) ne 0) then self.r = r
   if (n_elements(dr) ne 0) then self.dr = dr
   if (n_elements(color) ne 0) then self.color = color
end

pro C_sFrapThinning::GetProperty, x=x, y=y, r=r, dr = dr, color = color
    x= self.x
    y= self.y
    r= self.r
    dr= self.dr
    color = self.color
end


pro C_sFrapThinning::getSkeletonIntensities, image = image,$
                                                path = path,$
                                                stack_tlb = stack_tlb,$
                                                tPos = tPos,$
                                                chPos = chPos,$
                                                zPos = zPos,$
                                                Clus1 = Clus1,$
                                                Clus2 = Clus2,$
                                                r = r, x = x, y = y,$
                                                Keep = Keep

   
       
    dimImage = size(image, /dim) 
    thinnedImage = make_array(dimImage, /BYTE, VALUE=0)
    imageSeg = make_array(dimImage, /BYTE, VALUE=0)
    tempImage = make_array(dimImage, /BYTE, VALUE=0)
    tempImageOld = image
    
    imageSegTemp=Clus1
    image=Clus2
    
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
    thinnedImage /= max(thinnedImage)
    
    Mk = 0
    if Mk eq 0 then begin 
      thinOnly = thin(thinnedImage, /NEIGHBOR_COUNT)
    endif else begin
      file = dialog_pickfile( /read, path = path, get_path = path, filter = '*.tif')                    
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
        for i=x-r, dimImage[0]-1 do begin
          for j=y-r, dimImage[1]-1 do begin
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
    file = strCompress(path + strcompress('T_' + string(tPos) + '_Intensity_modeSkeleton_x' + string(fix(x)) + '_y' + string(fix(y)) + '_r' + string(fix(r)) + '_mk1.dat', /rem))        
    
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
      file = strCompress(path + strcompress('coordinates_Roi_Intensity_modeSkeleton_x' + string(fix(x)) + '_y' + string(fix(y)) + '_r' + string(fix(r)) + '_mk1.dat', /rem))         
      
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
    
      
end


pro C_sFrapThinning::cleanup
end


function C_sFrapThinning::init, x=x, y=y, r=r, dr = dr, color = color
    if keyword_set(x) then self.x = x
    if keyword_set(y) then self.y = y
    if keyword_set(r) then self.r = r
    if keyword_set(dr) then self.dr = dr
    if keyword_set(color) then self.color = color
    if keyword_set(anywhere) then self.anywhere = 1
    return, 1
end

pro C_sFrapThinning__define
   tmp = {C_sFrapThinning, x:0., y:0., r:0., dr:0., color:0, anywhere:1}
end

