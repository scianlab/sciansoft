;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjColocalization
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjColocalization' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjColocalization::apply, C_sROIGroupObj = C_sROIGroupObj, position = position, stack_tlb = stack_tlb

   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do  whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

    ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1:if (position[0] eq -1) then return else  whParamActive[position] = 1
      else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase
   wherePA = where(whParamActive eq 1)

      ; check Pointers
   if not(ptr_valid((*self.pParamStruct).pROINumberVect )) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect )) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

   nObjects = C_sROIGroupObj->count()
   if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

        ; set Object Number Vector
      *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

         ; define background correction
       if (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Background Value Ch1'))[0]] then $
         backGroundCh1 = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Background Value Ch1'))[0]] else $
         backGroundCh1 = 0
       if (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Background Value Ch2'))[0]] then $
         backGroundCh2 = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Background Value Ch2'))[0]] else $
         backGroundCh2 = 0

         ; get 1st IntImage
       mask_1 = C_sROIGroupObj->getGroupMask()
       dimImage = size(mask_1, /dim)
       int_1 = make_array(dimImage[0], dimImage[1], /long) + (C_sROIGroupObj->getGroupMaskIntensity() - backGroundCh1)

       if whParamActive[10] then begin
          ; get Number of Random Frames
         if (*(*(*self.pValueStruct)[whParam[10]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[10]]).pNames eq 'Number of Random Frames'))[0]] then $
          nRandomFrames = (*(*(*self.pValueStruct)[whParam[10]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[10]]).pNames eq 'Number of Random Frames'))[0]] $
          else nRandomFrames = 1
         if (*(*(*self.pValueStruct)[whParam[10]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[10]]).pNames eq 'View Random Masks'))[0]] then $
          fViewRandomMasks = 1b else nRandomFrames = 0
         if (fViewRandomMasks) then window, xSize = dimImage[0], ySize = dimImage[1]
       endif

         ; get 2nd Object
       s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos
       if (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Time'))[0]] then $
         tPos = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Time'))[0]] else $
         tPos = tPos
       if not (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Channel'))[0]] then begin
         case chPos of
          0:chPos = 1
          else: chPos -= chPos
         endcase
       endif else chPos = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Channel'))[0]]
       if (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Z-Slice'))[0]] then $
         zPos = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Z-Slice'))[0]] else $
         zPos = zPos
       if (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Cluster'))[0]] then $
         clusPos = (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Cluster'))[0]] else $
         clusPos = clusPos
       oROIGroup = s_ISegM_GetROI2DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, clusPos = clusPos)
       if obj_valid(oROIGroup) then begin
          mask_2 = oROIGroup->getGroupMask()
          int_2 = make_array(dimImage[0], dimImage[1], /long) + (oROIGroup->getGroupMaskIntensity() - backGroundCh2)
         endif else begin
          mask_2 = C_sROIGroupObj->getGroupMask() * 0
          int_2 = make_array(dimImage[0], dimImage[1], /long) + (C_sROIGroupObj->getGroupMask() * 0)
         endelse

         ; Object-Loop
       paramArray = fltArr(10, nObjects)
       for i = 0, nObjects-1 do begin
         pWhereObj = (C_sROIGroupObj->get(position = i))->getpWherePoints()
         whereColoc = where(mask_2[*pWhereObj] gt 0)
         if (whereColoc[0] ne -1) then begin
          sortOverlap = (mask_2[(*pWhereObj)[whereColoc]])[uniq(mask_2[(*pWhereObj)[whereColoc]], sort(mask_2[(*pWhereObj)[whereColoc]]))]
          paramArray[0, i] = n_elements(sortOverlap)
          paramArray[1, i] = 100. * n_elements(whereColoc) / n_elements(*pWhereObj)
          paramArray[2, i] = correlate(int_1[(*pWhereObj)[whereColoc]], int_2[(*pWhereObj)[whereColoc]], /double)

          totalS1S2 = total((int_1[(*pWhereObj)[whereColoc]]) * (int_2[(*pWhereObj)[whereColoc]]))
          paramArray[3, i] = totalS1S2 / (sqrt( total((int_1[(*pWhereObj)[whereColoc]])^2) * total((int_2[(*pWhereObj)[whereColoc]])^2) ) > 1)
          paramArray[4, i] = totalS1S2 / ( total((int_1[(*pWhereObj)[whereColoc]])^2))
          paramArray[5, i] = totalS1S2 / ( total((int_2[(*pWhereObj)[whereColoc]])^2))
          whereS2 = where(int_2[(*pWhereObj)[whereColoc]] gt 0)
;          if (whereS2[0] eq -1) then paramArray[6, i] = 1. / (total(int_1[(*pWhereObj)[whereColoc]]) > 1.) else $
;             paramArray[6, i] = total(int_1[(*pWhereObj)[whereColoc[whereS2]]]) / (total(int_1[(*pWhereObj)[whereColoc]]) > 1.)
          if (whereS2[0] eq -1) then paramArray[6, i] = 1. / (total(int_1[(*pWhereObj)]) > 1.) else $
              paramArray[6, i] = total(int_1[(*pWhereObj)[whereColoc[whereS2]]]) / (total(int_1[(*pWhereObj)]) > 1.)

          paramArray[7, i] = total(int_1[(*pWhereObj)[whereColoc]]) / (total(int_2[*pWhereObj]) > 1.)

          centerCoord_1 = (C_sROIGroupObj->get(position = i))->getCenterXYZ()
          distance = 0.
          nCount = 0
          for j = 0, n_elements(sortOverlap)-1 do begin
              centerCoord_2 = (oROIGroup->get(position = sortOverlap[j]-1))->getCenterXYZ()
              distance = distance + sqrt((centerCoord_1[1]-centerCoord_2[1])^2 + (centerCoord_1[0]-centerCoord_2[0])^2)
              whereHit = where(*pWhereObj eq (round(centerCoord_2[1])*dimImage[0] + round(centerCoord_2[0])))
              if (whereHit[0] ne -1) then nCount = nCount + 1
          endfor

          paramArray[8, i] = distance / n_elements(sortOverlap)
          paramArray[9, i] = nCount
         endif
       endfor

         ; Object Colocalization Frequency [N]
       if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = fltArr(nObjects) + paramArray[0, *]
         ; Object Colocalization Area [%]
       if whParamActive[1] then *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = fltArr(nObjects) + paramArray[1, *]
         ; Pearsons Coefficient
       whereColoc = where(finite(paramArray[2, *]) eq 0)
       if (whereColoc[0] ne -1) then paramArray[2, whereColoc] = 0.
       if whParamActive[2] then *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = fltArr(nObjects) + paramArray[2, *]
         ; Overlap Coefficient
       if whParamActive[3] then *(*(*self.pValueStruct)[whParam[3]]).pROIParamVect = fltArr(nObjects) + paramArray[3, *]
         ; Overlap Coefficient [k1]
       if whParamActive[4] then *(*(*self.pValueStruct)[whParam[4]]).pROIParamVect = fltArr(nObjects) + paramArray[4, *]
         ; Overlap Coefficient [k2]
       if whParamActive[5] then *(*(*self.pValueStruct)[whParam[5]]).pROIParamVect = fltArr(nObjects) + paramArray[5, *]
         ; Overlap Coefficient [m1]
       if whParamActive[6] then *(*(*self.pValueStruct)[whParam[6]]).pROIParamVect = fltArr(nObjects) + paramArray[6, *]
         ; Overlap Coefficient [M1]
       if whParamActive[7] then *(*(*self.pValueStruct)[whParam[7]]).pROIParamVect = fltArr(nObjects) + paramArray[7, *]
         ; Object Center of Gravity Distance
       if whParamActive[8] then *(*(*self.pValueStruct)[whParam[8]]).pROIParamVect = fltArr(nObjects) + paramArray[8, *]
         ; Object Center of Gravity Colocalization
       if whParamActive[9] then *(*(*self.pValueStruct)[whParam[9]]).pROIParamVect = fltArr(nObjects) + paramArray[9, *]

         ; Object Center of Gravity Colocalization
       randomParamArray = fltArr(20, 10, nObjects)
       if whParamActive[10] then begin
         for k = 0, nRandomFrames-1 do begin
             ; make random-picture 1
          mask_1 *= 0
          int_1 *= 0
          for i = 0, nObjects-1 do begin
              objArray = C_sROIGroupObj->getObjectInArray(objNumber = i)
              whereObj = where(objArray.mask gt 0)
              dimArray = size(objArray.mask, /dim)
              j = 0
              repeat begin
                 j += 1
                 posX = (round(randomu(stack_tlb + n_elements(whereObj)+i+j+k) * (dimImage[0] - dimArray[0] - 2)) > 0)
                 posY = (round(randomu(stack_tlb + whereObj[0]+j+i+k-1) * (dimImage[1] - dimArray[1] - 2)) > 0)
              endrep until (((where(mask_1[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] gt 0))[0] eq -1) or (j gt 20))
              mask_1[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] = mask_1[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] > (objArray.mask * (i+1))
              int_1[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] = int_1[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] > objArray.intValues
          endfor

              ; make random-picture 2
          mask_2 *= 0
          int_2 *= 0
          for i = 0, oROIGroup->count()-1 do begin
              objArray = oROIGroup->getObjectInArray(objNumber = i)
              whereObj = where(objArray.mask gt 0)
              dimArray = size(objArray.mask, /dim)
              j = 0
              repeat begin
                 j += 1
                 posX = (round(randomu(stack_tlb + n_elements(whereObj)+i+j+k) * (dimImage[0] - dimArray[0] - 2)) > 0)
                 posY = (round(randomu(stack_tlb + whereObj[0]+j+i+k-1) * (dimImage[1] - dimArray[1] - 2)) > 0)
              endrep until (((where(mask_2[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] gt 0))[0] eq -1) or (j gt 20))
              mask_2[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] = mask_2[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] > (objArray.mask * (i+1))
              int_2[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] = int_2[posX:posX + dimArray[0] - 1, posY:posY + dimArray[1] - 1] > objArray.intValues
          endfor
          if fViewRandomMasks then tvScl, mask_1+ mask_2

          for i = 0, nObjects-1 do begin
              whereObj = where(mask_1 eq (i + 1))
              if (whereObj[0] ne -1) then begin
                 whereColoc = where(mask_2[whereObj] gt 0)
                 if (whereColoc[0] ne -1) then begin

                   sortOverlap = (mask_2[(whereObj)[whereColoc]])[uniq(mask_2[(whereObj)[whereColoc]], sort(mask_2[(whereObj)[whereColoc]]))]
                   randomParamArray[k,0, i] = n_elements(sortOverlap)
                   randomParamArray[k,1, i] = 100. * n_elements(whereColoc) / n_elements(whereObj)
                   randomParamArray[k,2, i] = correlate(int_1[(whereObj)[whereColoc]], int_2[(whereObj)[whereColoc]], /double)

                   totalS1S2 = total((int_1[(whereObj)[whereColoc]]) * (int_2[(whereObj)[whereColoc]]))
                   randomParamArray[k,3, i] = totalS1S2 / (sqrt( total((int_1[(whereObj)[whereColoc]])^2) * total((int_2[(whereObj)[whereColoc]])^2) ) > 1)
                   randomParamArray[k,4, i] = totalS1S2 / ( total((int_1[(whereObj)[whereColoc]])^2))
                   randomParamArray[k,5, i] = totalS1S2 / ( total((int_2[(whereObj)[whereColoc]])^2))
                   whereS2 = where(int_2[(whereObj)[whereColoc]] gt 0)
                   if (whereS2[0] eq -1) then randomParamArray[k,6, i] = 1. / (total(int_1[(whereObj)]) > 1.) else $
                      randomParamArray[k,6, i] = total(int_1[(whereObj)[whereColoc[whereS2]]]) / (total(int_1[(whereObj)]) > 1.)

                   randomParamArray[k,7, i] = total(int_1[(whereObj)[whereColoc]]) / (total(int_2[whereObj]) > 1.)

                   centerCoord_1 = [total(whereObj mod dimImage[0]), total(floor(1.*whereObj / dimImage[0])) ] / (1.* n_elements(whereObj))
                   distance = 0.
                   nCount = 0
                   for j = 0, n_elements(sortOverlap)-1 do begin
                      whereObj = where(mask_2 eq sortOverlap[j])
                      if (whereObj[0] ne -1) then begin
                          centerCoord_2 = [total(whereObj mod dimImage[0]), total(floor(1.*whereObj / dimImage[0])) ] / (1.* n_elements(whereObj))
                          distance = distance + sqrt((centerCoord_1[1]-centerCoord_2[1])^2 + (centerCoord_1[0]-centerCoord_2[0])^2)
                          whereHit = where(whereObj eq (round(centerCoord_2[1])*dimImage[0] + round(centerCoord_2[0])))
                          if (whereHit[0] ne -1) then nCount = nCount + 1
                      endif
                   endfor
                   randomParamArray[k,8, i] = distance / n_elements(sortOverlap)
                   randomParamArray[k,9, i] = nCount
                 endif
              endif
          endfor
         endfor

         print, 'Number of Random Frames:', nRandomFrames
          ; Object Colocalization Frequency [N]
         print, 'Mean +- Variance'
         print, 'Random Object Colocalization Frequency [N]', (moment(randomParamArray[*, 0, *]))[0:1]
         print, 'Object Colocalization Frequency [N]', (moment(paramArray[0, *]))[0:1]
          ; Object Colocalization Area [%]
         print, 'Mean +- Variance'
         print, 'Random Object Colocalization Area [%]', (moment(randomParamArray[*, 1, *]))[0:1]
         print, 'Object Colocalization Area [%]', (moment(paramArray[1, *]))[0:1]
          ; Pearsons Coefficient
         print, 'Mean +- Variance'
         whereColoc = where(finite(randomParamArray[*]) eq 0)
         if (whereColoc[0] ne -1) then randomParamArray[whereColoc] = 0.
         print, 'Random Pearsons Coefficient', (moment(randomParamArray[*, 2, *]))[0:1]
         print, 'Pearsons Coefficient', (moment(paramArray[2, *]))[0:1]
          ; Overlap Coefficient
         print, 'Mean +- Variance'
         print, 'Random Overlap Coefficient', (moment(randomParamArray[*, 3, *]))[0:1]
         print, 'Overlap Coefficient', (moment(paramArray[3, *]))[0:1]
          ; Overlap Coefficient [k1]
         print, 'Mean +- Variance'
         print, 'Random Overlap Coefficient [k1]', (moment(randomParamArray[*, 4, *]))[0:1]
         print, 'Overlap Coefficient [k1]', (moment(paramArray[4, *]))[0:1]
          ; Overlap Coefficient [k2]
         print, 'Mean +- Variance'
         print, 'Random Overlap Coefficient [k1]', (moment(randomParamArray[*, 5, *]))[0:1]
         print, 'Overlap Coefficient [k1]', (moment(paramArray[5, *]))[0:1]
          ; Overlap Coefficient [m1]
         print, 'Mean +- Variance'
         print, 'Random Overlap Coefficient [m1]', (moment(randomParamArray[*, 6, *]))[0:1]
         print, 'Overlap Coefficient [m1]', (moment(paramArray[6, *]))[0:1]
          ; Overlap Coefficient [M1]
         print, 'Mean +- Variance'
         print, 'Random Overlap Coefficient [M1]', (moment(randomParamArray[*, 7, *]))[0:1]
         print, 'Overlap Coefficient [M1]', (moment(paramArray[7, *]))[0:1]
          ; Object Center of Gravity Distance
         print, 'Mean +- Variance'
         print, 'Random Object Center of Gravity Distance', (moment(randomParamArray[*, 8, *]))[0:1]
         print, 'Object Center of Gravity Distance', (moment(paramArray[8, *]))[0:1]
          ; Object Center of Gravity Colocalization
         print, 'Mean +- Variance'
         print, 'Random Object Center of Gravity Colocalization', (moment(randomParamArray[*, 9, *]))[0:1]
         print, 'Object Center of Gravity Colocalization', (moment(paramArray[9, *]))[0:1]

         columnNames = ['Mean', 'Variance']
         rowNames = ['Random Object Colocalization Frequency [N]',$
          'Object Colocalization Frequency [N]',$
          'Random Object Colocalization Area [%]',$
          'Object Colocalization Area [%]',$
          'Random Pearsons Coefficient',$
          'Pearsons Coefficient',$
          'Random Overlap Coefficient',$
          'Overlap Coefficient',$
          'Random Overlap Coefficient [k1]',$
          'Overlap Coefficient [k1]',$
          'Random Overlap Coefficient [k1]',$
          'Overlap Coefficient [k1]',$
          'Random Overlap Coefficient [m1]',$
          'Overlap Coefficient [m1]',$
          'Random Overlap Coefficient [M1]',$
          'Overlap Coefficient [M1]',$
          'Random Object Center of Gravity Distance',$
          'Object Center of Gravity Distance',$
          'Random Object Center of Gravity Colocalization',$
          'Object Center of Gravity Colocalization']

         paramValues = fltArr(2,20)
         paramValues[*,0] = (moment(randomParamArray[*, 0, *]))[0:1]
         paramValues[*,1] = (moment(paramArray[0, *]))[0:1]
         paramValues[*,2] = (moment(randomParamArray[*, 1, *]))[0:1]
         paramValues[*,3] = (moment(paramArray[1, *]))[0:1]
         paramValues[*,4] = (moment(randomParamArray[*, 2, *]))[0:1]
         paramValues[*,5] = (moment(paramArray[2, *]))[0:1]
         paramValues[*,6] = (moment(randomParamArray[*, 3, *]))[0:1]
         paramValues[*,7] = (moment(paramArray[3, *]))[0:1]
         paramValues[*,8] = (moment(randomParamArray[*, 4, *]))[0:1]
         paramValues[*,9] = (moment(paramArray[4, *]))[0:1]
         paramValues[*,10] = (moment(randomParamArray[*, 5, *]))[0:1]
         paramValues[*,11] = (moment(paramArray[5, *]))[0:1]
         paramValues[*,12] = (moment(randomParamArray[*, 6, *]))[0:1]
         paramValues[*,13] = (moment(paramArray[6, *]))[0:1]
         paramValues[*,14] = (moment(randomParamArray[*, 7, *]))[0:1]
         paramValues[*,15] = (moment(paramArray[7, *]))[0:1]
         paramValues[*,16] = (moment(randomParamArray[*, 8, *]))[0:1]
         paramValues[*,17] = (moment(paramArray[8, *]))[0:1]
         paramValues[*,18] = (moment(randomParamArray[*, 9, *]))[0:1]
         paramValues[*,19] = (moment(paramArray[9, *]))[0:1]

         paramTableuValue = {groupLeader:-1,$
                                 paramRowNames:rowNames,$
                                 paramColumnNames:columnNames,$
                                 param:paramValues,$
                                 name:'Random Colocalization Values',$
                                 wTopBase:-1l}
         s_ObjParamTableWidget, paramTableuValue = paramTableuValue
       endif
    endelse
end


function C_sROIParam_ObjColocalization::init
    ROIParamStruct = {name:'Object Colocalization',$     ;  ROI Name.
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$  ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$         ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$      ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(11))
    ROIParamWidgetType = make_array(11, /string, value = 'widget_slider')
    ROIParamNames = ['Object Colocalization Frequency [N]',$
                    'Object Colocalization Area [%]',$
                    'Pearsons Coefficient',$
                    'Overlap Coefficient',$
                    'Overlap Coefficient [k1]',$
                    'Overlap Coefficient [k2]',$
                    'Overlap Coefficient [m1]',$
                    'Overlap Coefficient [M1]',$
                    'Object Center of Gravity Distance',$
                    'Object Center of Gravity Colocalization',$
                    'Simulate Random xy-Distribution']
    ROIParamActive = [1,1,1,1,1,1,1,1,1,1,1]
    ROIParamMin = [0,0,0,0,0,0,0,0,0,0,0]
    ROIParamMax = [0,0,0,0,0,0,0,0,0,0,0]
    ROIParamValues = [0,0,0,0,0,0,0,0,0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:'Object Colocalization Frequency [N]',$
                      type:'Single ROI-Parameter-Method',$
                      pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                      pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                      pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                      pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                      pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                      pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                      pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIThresValueWidgetType = make_array(12, /string, value = 'widget_slider')
    ROIThresValueNames = ['Time', 'Channel',$
                          'Z-Slice', 'Cluster',$
                          'Background Value Ch1', 'Background Value Ch2',$
                          'Threshold_1a', 'Threshold_1b',$
                          'Threshold_2a', 'Threshold_2b',$
                          'Threshold_3a', 'Threshold_3b',$
                          'Threshold_4a', 'Threshold_4b']
    ROIThresValueActive = [0,0,$
                          0,0,$
                          0,0,$
                          0,0,$
                          0,0,$
                          0,0,$
                          0,0]
    ROIThresValueMin = [0., 0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0.,$
                        0.,0. ]
    ROIThresValueMax = [1000, 1000,$
                        1000,1000,$
                        0.,0.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1.,$
                        1.,1. ]
    ROIThresValueValues = [0., 0.,$
                        0.,0.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIThresValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIThresValueNames)
    ROIValueStruct.pActive = ptr_new(ROIThresValueActive)
    ROIValueStruct.pMin = ptr_new(ROIThresValueMin)
    ROIValueStruct.pMax = ptr_new(ROIThresValueMax)
    ROIValueStruct.pValues = ptr_new(ROIThresValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Colocalization Area [%]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$     ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = [ROIThresValueWidgetType[6:*]]
    ROIValueNames = [ROIThresValueNames[6:*]]
    ROIValueActive = [ROIThresValueActive[6:*]]
    ROIValueMin = [ROIThresValueMin[6:*]]
    ROIValueMax = [ROIThresValueMax[6:*]]
    ROIValueValues = [ROIThresValueValues[6:*]]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Pearsons Coefficient',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Overlap Coefficient [Manders]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Overlap Coefficient [k1]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[4] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Overlap Coefficient [k2]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[5] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Overlap Coefficient [m1]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[6] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Overlap Coefficient [M1]',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[7] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Center of Gravity Distance',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[8] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Object Center of Gravity Colocalization',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ROIThresValueWidgetType
    ROIValueNames = ROIThresValueNames
    ROIValueActive = ROIThresValueActive
    ROIValueMin = ROIThresValueMin
    ROIValueMax = ROIThresValueMax
    ROIValueValues = ROIThresValueValues

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[9] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'Simulate Random xy-Distribution',$
                    type:'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ['widget_slider', 'widget_slider', ROIThresValueWidgetType]
    ROIValueNames = ['Number of Random Frames', 'View Random Masks', ROIThresValueNames]
    ROIValueActive = [1, 1, ROIThresValueActive]
    ROIValueMin = [1, 1, ROIThresValueMin]
    ROIValueMax = [30, 1, ROIThresValueMax]
    ROIValueValues = [20, 1, ROIThresValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[10] = ptr_new(ROIValueStruct, /no_copy)
    return, 1
end

pro C_sROIParam_ObjColocalization__define
   tmp = {C_sROIParam_ObjColocalization, pParamStruct:ptr_new(),$
                                  pValueStruct:ptr_new(),$
                                  inherits C_sROIParam }
end