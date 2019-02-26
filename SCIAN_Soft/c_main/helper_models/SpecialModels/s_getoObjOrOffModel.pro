; Load Obj or Off model file... 
; for now i use the object 0 for load file and others object keep for compatibality... 
pro s_getoObjOrOffMeshModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference
   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   relacionXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]
   
   nObj = oGroupReference->count()
   for i = 0, nObj-1 do begin
      ; Iniciando
       if (i eq 0) then begin
            filters = [['*.off', '*.obj'], $ 
                        ['OFF model File', 'OBJ model File']] 
            ;file = dialog_pickfile( /read, /must_exist, path = state.path, get_path = path, group = ev.top, filter = filters, /multiple_files)
            file = dialog_pickfile( /read, /must_exist, path = path, get_path = path, filter = filters)
            if (file[0] ne '') then begin
                archivo = file

                result = strPos(archivo[0], '.off', /reverse_search)
                if (result ne -1) then begin
                endif

                result = strPos(archivo[0], '.obj', /reverse_search)
                if (result ne -1) then begin
                    numLines = 0.0d
                    numFaces = 0.0d
                    numVertices = 0.0d
                    maxLines = 0.0d
                    maxFaces = 0.0d
                   ; Open the file test.lis: 
                    OPENR, 1, archivo[0]
                    ; Define a string variable: 
                    A = '' 
                    ; Loop until EOF is found: 
                    WHILE ~ EOF(1) DO BEGIN 
                       ; Read a line of text: 
                       READF, 1, A
                       if (strMid(A, 0,1) eq 'v' || strMid(A, 0,1) eq 'V') then begin 
                           numVertices = numVertices + 1.0d;
                       endif
                       if (strMid(A, 0,1) eq 'F' || strMid(A, 0,1) eq 'f') then begin 
                           numFaces = numFaces + 1.0d;
                           
                       endif
                       if (strMid(A, 0,1) eq 'l' || strMid(A, 0,1) eq 'L') then begin 
                           numLines = numLines + 1.0d;
                       endif
                    ENDWHILE 
                    ; Close the file: 
                    close, /all
                    
                    if(numVertices gt 0.0d) then begin
                    
                        vertices = MAKE_ARRAY(3,numVertices,/float, Value = 0)
                        
                        ;if(numFaces gt 0) then caras = MAKE_ARRAY(3,numFaces,/float, Value = 0)
                        ;if(numLines gt 0) then lineas = MAKE_ARRAY(3,numLines,/float, Value = 0)    
                        OPENR, 1, archivo[0]
                        ; Loop until EOF is found:
                        indexF = 0.0d
                        indexL = 0.0d
                        indexV = 0.0d
                        WHILE ~ EOF(1) DO BEGIN 
                           ; Read a line of text: 
                           READF, 1, A
                           if (strMid(A, 0,1) eq 'v' || strMid(A, 0,1) eq 'V') then begin
                              A = strMid(A, 1,strlen(A)-1)
                              bRun = 1b
                              tempIndex = 0.0d
                              tempP = 0.0d
                              while(bRun eq 1b) do begin
                                 tempIndex = STREGEX(A, ' [0|1|2|3|4|5|6|7|8|9|.|,|-]+', /FOLD_CASE)
                                 if(tempIndex[0] ne -1) then begin
                                     tempString = STREGEX(A,' [0|1|2|3|4|5|6|7|8|9|.|,|-]+', /EXTRACT, /FOLD_CASE)
                                      
                                     ; Actualizacion Strings
                                     posBase = tempIndex + strlen(tempString)
                                     lenTemp = double(strlen(A)) -double(strlen(tempString)) 
                                     A =  strMid(A, posBase,lenTemp)
                                     tempString = strMid(tempString, 1,strlen(tempString)-1) 

                                     vertices[tempP,indexV] = double(tempString)
                                     tempP = tempP + 1.0d
                                 endif else bRun = 0b
                              endwhile
                              indexV = indexV + 1.0d
                           endif
                           if (strMid(A, 0,1) eq 'F' || strMid(A, 0,1) eq 'f') then begin 
                              A = strMid(A, 1,strlen(A)-1)
                              bRun = 1b
                              tempIndex = 0.0d
                              tempP = 0.0d
                              while(bRun eq 1b) do begin
                                 tempIndex = STREGEX(A, ' [0|1|2|3|4|5|6|7|8|9|.|,|-]+', /FOLD_CASE)
                                 if(tempIndex[0] ne -1) then begin
                                     tempString = STREGEX(A,' [0|1|2|3|4|5|6|7|8|9|.|,|-]+', /EXTRACT, /FOLD_CASE)
                                      
                                     ; Actualizacion Strings
                                     posBase = tempIndex + strlen(tempString)
                                     lenTemp = double(strlen(A)) -double(strlen(tempString)) 
                                     A =  strMid(A, posBase,lenTemp)
                                     tempString = strMid(tempString, 1,strlen(tempString)-1) 

                                     if(tempP eq 0.0d) then begin
                                        tempCaras = [double(tempString)-1]
                                     endif else tempCaras = [tempCaras, uint(tempString)-1]
                                     tempP = tempP + 1.0d
                                 endif else bRun = 0b
                              endwhile
                              
                              if(indexF eq 0.0d) then begin
                                caras = [N_ELEMENTS(tempCaras) , tempCaras]                              
                                indexF = 1.0d
                              endif else caras = [caras,N_ELEMENTS(tempCaras) , tempCaras] 
                           endif
                           if (strMid(A, 0,1) eq 'l' || strMid(A, 0,1) eq 'L') then begin
                              A = strMid(A, 1,strlen(A)-1)
                              bRun = 1b
                              tempIndex = 0.0d
                              while(bRun eq 1b) do begin
                                 tempIndex = STREGEX(A, ' [0|1|2|3|4|5|6|7|8|9|.|,|-]+', /FOLD_CASE)
                                 if(tempIndex[0] ne -1) then begin
                                     tempString = STREGEX(A,' [0|1|2|3|4|5|6|7|8|9|.|,|-]+', /EXTRACT, /FOLD_CASE)
                                      
                                     ; Actualizacion Strings
                                     posBase = tempIndex + strlen(tempString)
                                     lenTemp = double(strlen(A)) -double(strlen(tempString)) 
                                     A =  strMid(A, posBase,lenTemp)
                                     tempString = strMid(tempString, 1,strlen(tempString)-1) 

                                     if(tempIndex eq 0.0d) then begin
                                        tempCaras = [double(tempString)-1]
                                     endif else tempCaras = [tempCaras, uint(tempString)-1]
                                     tempIndex = tempIndex + 1.0d
                                 endif else bRun = 0b
                              endwhile
                              
                              if(indexL eq 0.0d) then begin
                                lineas = [N_ELEMENTS(tempCaras) , tempCaras]                              
                                indexL = 1.0d
                              endif else lineas = [lineas,N_ELEMENTS(tempCaras) , tempCaras] 
                           endif
                        ENDWHILE 
                        close, /all
                        minX = min(vertices[0,*], max = maxX)
                        minY = min(vertices[1,*], max = maxY)
                        minZ = min(vertices[2,*], max = maxZ)
                        
                        factor = 1.0d /2.0d
                        vertices[0,*] = (factor * (0.5d*xyzDim[0]/(maxX - minX)) * (vertices[0,*] - minX)) ;+ (xyzDim[0]/2.0d)
                        vertices[1,*] = (factor * (0.5d*xyzDim[1]/(maxY - minY)) * (vertices[1,*] - minY)) ;+ (xyzDim[1]/2.0d)
                        vertices[2,*] = (factor * (0.5d*xyzDim[2]/(maxZ - minZ)) * (vertices[2,*] - minZ)) ;+ (xyzDim[2]/2.0d)
                        
                        if(N_ELEMENTS(caras) gt 0) then begin
                            (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
                            objNumber = (oGroupReference->get(position = i))->getNumber()
                            oObjectModel->add, obj_new('IDLgrPolygon', data = vertices,$
                                                                       poly = caras,$
                                                                       ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                                       name = strCompress('3DLoadedModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
                        endif
                        if(N_ELEMENTS(lineas) gt 0) then begin
                            (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
                            objNumber = (oGroupReference->get(position = i))->getNumber()
                            oObjectModel->add, obj_new('IDLgrPolyline', data = vertices,$
                                                                       POLYLINES = caras,$
                                                                       ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                                       xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                                       name = strCompress('3DLoadedModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
                        endif
                    endif          
                endif
            endif
       endif else begin
          (oGroupReference->get(position = i))->getProperty, color = color, alpha_channel = alpha
          objNumber = (oGroupReference->get(position = i))->getNumber()
          oObjectModel->add, obj_new('IDLgrPolygon',$
                                                     ;poly = poligonosFinales,$
                                                     ambient = [0,0,0], bottom = color, color = color, alpha_channel = alpha, shininess = 128., shading = 1,$
                                                     xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                     name = strCompress('3DLoadedModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
       endelse
   endfor
end
