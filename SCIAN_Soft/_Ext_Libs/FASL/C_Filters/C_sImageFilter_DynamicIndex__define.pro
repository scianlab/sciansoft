;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_DynamicIndex
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;   Dr. Steffen Härtel (2001)
;   e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;        result = obj_new('C_sImageFilter_DynamicIndex' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_DynamicIndex::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_DynamicIndex::apply, image = image,$
                        selectedStackObject = selectedStackObject ,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos ,$
                        chPos = chPos ,$
                        zPos = zPos ,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y
   pParamStruct = selectedStackObject->getpParamStruct()
   path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Image Path'))[0]]   
   
   if(FILE_TEST(strCompress(path+'_DynamicsIndexs'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(path+'_DynamicsIndexs'+ path_sep())
   if(FILE_TEST(strCompress(path+'_DynamicsIndexs'+ path_sep()+'_Images'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(path+'_DynamicsIndexs'+ path_sep()+'_Images'+ path_sep())
   if(FILE_TEST(strCompress(path+'_DynamicsIndexs'+ path_sep()+'_Data'+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(path+'_DynamicsIndexs'+ path_sep()+'_Data'+ path_sep())
     
   totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]

   whNext = (where((*(*self.pParamStruct).pNames) eq '2nd_Image_TimeStep'))[0]
   (*(*self.pParamStruct).pMax)[whNext] = totalTimes - 1  
   if((*(*self.pParamStruct).pActive)[whNext]) then  stepNext = (((*(*self.pParamStruct).pValues)[whNext]) >0) < (totalTimes) $
     else stepNext = 1

   
   oImage  = selectedStackObject->getSelectedImageObject(tPos = tPos, chPos = chPos, zPos = zPos)

   whOp = (where((*(*self.pParamStruct).pNames) eq 'Operation:(Diff/Corr)'))[0]
   if ( (*(*self.pParamStruct).pActive)[whOp]) then _Op = (*(*self.pParamStruct).pValues)[whOp]  $
     else _Op = 0.

   whGridSubs = (where((*(*self.pParamStruct).pNames) eq 'Grid SubDivisions'))[0]
   if ( (*(*self.pParamStruct).pActive)[whGridSubs]) then _GridSubs = (*(*self.pParamStruct).pValues)[whGridSubs]  $
     else _GridSubs = 1.

   whVis = (where((*(*self.pParamStruct).pNames) eq 'Visualization:(Squares/ZProy/SegActual/SegNext)'))[0]
   if ( (*(*self.pParamStruct).pActive)[whVis]) then _Vis = (*(*self.pParamStruct).pValues)[whVis]  $
     else _Vis = 0.

   whPond = (where((*(*self.pParamStruct).pNames) eq 'Pond:(Mean,Total,Max)'))[0]
   if ( (*(*self.pParamStruct).pActive)[whPond]) then _Pond = (*(*self.pParamStruct).pValues)[whPond]  $
     else _Pond = 0.

   whNor = (where((*(*self.pParamStruct).pNames) eq 'Normalize'))[0]
   if ( (*(*self.pParamStruct).pActive)[whNor]) then _Nor = (*(*self.pParamStruct).pValues)[whNor]  $
     else _Nor = 0.
   whSave = (where((*(*self.pParamStruct).pNames) eq 'Save: 0_NOSAVE,1_SaveCurrent,2_SaveAll'))[0]
   if ( (*(*self.pParamStruct).pActive)[whSave]) then _Save = (*(*self.pParamStruct).pValues)[whSave]  $
     else _Save = 0.
     (*(*self.pParamStruct).pValues)[whSave] = 0 ; always reset the save value
     
   ;if (whMinMaxRange eq 0.) then image = ((1. * image_2) - image) + whZeroToInt else $
   ;  image = ((((1. * image_2) - image) + whZeroToInt) < (whZeroToInt+whMinMaxRange)) > (whZeroToInt-whMinMaxRange)
   ;image[0,0] = (whZeroToInt+whMinMaxRange)
   ;image[1,0] = (whZeroToInt-whMinMaxRange)

   limiteTimes = 1
   limiteTimes = (totalTimes - 2) - (stepNext-1) + 1
   
   stepX = 1.0
   stepY = 1.0
   
   dimI = size (image, /dim)
   
   floorX = floor(1.0*dimI[0]/_GridSubs)
   floorY = floor(1.0*dimI[1]/_GridSubs)
   if(_GridSubs gt 1) then begin
      stepX = floorX
      stepY = floorY
   endif  
   
   meanArea = stepX * stepY
   
   floorX = floor(1.0*dimI[0]/stepX)
   ceilX  = ceil(1.0*dimI[0]/stepX)
   floorY = floor(1.0*dimI[1]/stepY)
   ceilY  = ceil(1.0*dimI[1]/stepY)
   
   numDivX = floorX + 1.0*(ceilX - floorX)
   numDivY = floorY + 1.0*(ceilY - floorY)
   numRois  = numDivX * numDivY 

   vectorData         = make_array(numRois,limiteTimes, value = -1.)
   vectorOriginalData = make_array(numRois,totalTimes, value = -1.)
   tProy = image ; Global mask of interest zone for all times

   indiceRoi = 0
   areaPerRoi = make_array(numRois, value = 0.)
       if(_GridSubs gt 1) then begin
          for i = 0, dimI[0]-1,stepX do begin
            limitX = ((i + stepX - 1) gt dimI[0]-1)? (dimI[0]-1) : (i + stepX - 1)  
            for j = 0, dimI[1]-1,stepY do begin
                limitY = ((j + stepY - 1) gt dimI[1]-1)? (dimI[1]-1) : (j + stepY - 1)
    
                areaPerRoi[indiceRoi] = total(tProy[i:limitX,j:limitY])
                indiceRoi++
            endfor
          endfor
       endif


     imagenAcumulada = image * 0.0
     
     for indexTime = 0, totalTimes-1 do begin
         valido = (indexTime + stepNext) le (totalTimes-1)
         tNext = (indexTime + stepNext) < (totalTimes-1)
         myOriginalImage   = (selectedStackObject->getSelectedImage(tPos = indexTime, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
         nextOriginalImage = (selectedStackObject->getSelectedImage(tPos = tNext, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]   
      
         analysisImage1 = tProy * myOriginalImage * 1.0   ; ZPRoyection * Raw1  
         analysisImage2 = tProy * nextOriginalImage * 1.0; ZPRoyection * RawNext
         
         whereImage1 = where(analysisImage1 gt 40, /L64, COMPLEMENT = whereNoImage1)
         whereImage2 = where(analysisImage2 gt 40, /L64, COMPLEMENT = whereNoImage2)
      
         ; Apply Op
         if(_Op eq 0) then begin
                      image = analysisImage2 - analysisImage1  ;? abs()
         endif else   image = analysisImage2 * analysisImage1  ;? abs()
      
         ; Apply Confine
         image = abs(image) ; ??
         
         indiceRoi = 0
         if(_GridSubs gt 1) then begin
            for i = 0, dimI[0]-1,stepX do begin
              limitX = ((i + stepX - 1) gt dimI[0]-1)? (dimI[0]-1) : (i + stepX - 1)  
              for j = 0, dimI[1]-1,stepY do begin
                  limitY = ((j + stepY - 1) gt dimI[1]-1)? (dimI[1]-1) : (j + stepY - 1)
      
                  case _Pond of
                       0: begin 
                          dummy = mean(image[i:limitX,j:limitY])
                          image[i:limitX,j:limitY] = dummy
                       endcase 
                       1: begin
                          dummy = total(image[i:limitX,j:limitY])
                          image[i:limitX,j:limitY] = dummy
                       endcase 
                       2: begin
                          dummy = max(image[i:limitX,j:limitY])
                          image[i:limitX,j:limitY] = dummy                    
                       endcase
                       else: begin
                          dummy = mean(image[i:limitX,j:limitY])
                          image[i:limitX,j:limitY] = dummy
                       endcase   
                  endcase
    
                  if(indexTime le (limiteTimes-1)) then begin
                    vectorData[indiceRoi,indexTime] = dummy
                  endif
                  
                  vectorOriginalData[indiceRoi,indexTime] = total(analysisImage1[i:limitX,j:limitY])
                  
                  indiceRoi++            
              endfor
            endfor
         endif

               case _Vis of
                     0: begin ;Squares
                              ; all is fine XD   
                     endcase 
                     1: begin ;ZProy
                        image = image * tProy ; remember... image_2 is zproy (image too ... but only at the init of code)
                     endcase
                     2: begin ;SegActual
                        image[whereNoImage1] = 0  
                     endcase
                     3: begin ;SegNext
                        image[whereNoImage2] = 0
                     endcase
                     else: begin
                     endcase   
               endcase            
            
          if((_Save eq 2) or ((_Save eq 1) and (indexTime eq tPos)))then begin
              name = strCompress('savedDynamicRoi' + string(indexTime) + '.tiff' , /rem) 
              filename = strCompress('_DynamicsIndexs'+ path_sep()+'_Images'+ path_sep()+ name,/rem)
              write_tiff, path + filename, image
          endif  
          
          if(valido) then begin
            imagenAcumulada = imagenAcumulada +  image
          endif             
                 
     endfor

     imagenAcumulada =  bytScl(imagenAcumulada)
     if(_Save eq 2) then begin
       name = strCompress('savedDynamicRoi' + 'Acumulada' + '.tiff' , /rem) 
       filename = strCompress('_DynamicsIndexs'+ path_sep()+'_Images'+ path_sep()+ name,/rem)
       write_tiff, path + filename, imagenAcumulada

       name = strCompress('meanIntensity_AllTIME_GridDiv_' + string(_GridSubs) + '_stepTime_'+ string(stepNext) + '_nRois_' + string(numRois) + '.dat', /rem)
       SaveData,data = vectorData, name = name, tiempos = limiteTimes, area = areaPerRoi, path = path

       name = strCompress('individualTimeROIINTENSITY_GridDiv_' + string(_GridSubs) + '_stepTime_'+ string(stepNext) + '_nRois_' + string(numRois) + '.dat', /rem)
       SaveData,data = vectorOriginalData, name = name, tiempos = totalTimes, area = areaPerRoi, path = path
     end

    return, imagenAcumulada
end

pro SaveData, data = data, name = name, tiempos = tiempos, area = area, path = path
;function C_sImageFilter_DynamicIndex::SaveData, data = data, name = name, tiempos = tiempos
      ; Parameter_Save
      filename = strCompress(path+'_DynamicsIndexs'+ path_sep()+'_Data'+ path_sep()+ name,/rem)      
      get_lun, U
                
      openW, U, filename
      ; get dimensions of meanFrapPlot
      dims = size(data, /dim)
      numElemData = N_ELEMENTS(dims)
      if(numElemData eq 1) then begin
        temp = make_array(dims[0],2, value = -1.)
        temp[*,0] = area
        ; writing to file incorrectly...maybe need to tranpose data
        temp[*,1] = data
      endif else begin
        temp = make_array(dims[0],dims[1]+1, value = -1.)
        temp[*,0] = area
        ; writing to file incorrectly...maybe need to tranpose data
        temp[*,1:tiempos] = data
      endelse

      temp = TRANSPOSE(temp)        
      cols = tiempos+1
      printF,U, temp, format = strcompress('(',/rem) + strcompress(string(cols),/rem) + strcompress('F10.3',/rem) +strcompress(')' ,/rem)
      close, U 
      free_lun, U
end

function C_sImageFilter_DynamicIndex::init
   ImageFilterStruct = {Name: 'C_DynamicIndex',$    ;  Filter Name.
               pWidgetType: ptr_new(),$    ; Pointer on Filter Parameter Names.
               pNames: ptr_new(),$     ; Pointer on Filter Parameter Names.
               pActive: ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
               pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
               pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
               pValues: ptr_new()}       ; Pointer on Filter Parameter Values.

     ; Parameters of C_ImageColocalization.
   filterParamWidgetType = make_array(7, /string, value = 'widget_slider')
   filterParamNames = ['2nd_Image_TimeStep', 'Operation:(Diff/Corr)', 'Grid SubDivisions','Visualization:(Squares/ZProy/SegActual/SegNext)','Pond:(Mean,Total,Max)','Normalize','Save: 0_NOSAVE,1_SaveCurrent,2_SaveAll']
   filterParamActive = [1, 1, 1,1,1, 1, 1]
   filterParamMin = [1, 0, 1,0,0, 0, 0]
   filterParamMax = [100, 1, 2000,3,2, 1, 2] ; 0 .. dont save..... 1 : save current.... 2: save for all
   filterParamValues = [0, 0, 1,0,0, 0, 0]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_DynamicIndex__define
   tmp = {C_sImageFilter_DynamicIndex, pParamStruct: ptr_new(), inherits C_sImageFilter}
end