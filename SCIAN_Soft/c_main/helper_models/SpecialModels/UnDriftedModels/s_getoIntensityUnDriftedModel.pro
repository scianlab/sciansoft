; FASL get Balls Model

pro s_getoIntensityUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel, fROIInt = fROIInt
   if (n_elements(fROIInt) gt 0) then fROIInt = 1b else fROIInt = 0b
   oObjectModel->getProperty, name = modelName
   modelName1 = strCompress(modelName + '1', /rem)
   modelName2 = strCompress(modelName + '2', /rem)

   xyzDim = oGroupReference->getxyzDim()

      ; get 1st  object and object parameters
   volData1 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   if (xyzDim[2] eq 1) then volData1 = reform(volData1, xyzDim[0], xyzDim[1], xyzDim[2])
   if not(fROIInt) then begin
      s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos
      widget_control, stack_tlb, get_uValue = stackState, /no_copy
         imageStackInfoObject = *stackState.pImageStackInfoObject
      widget_control, stack_tlb, set_uValue = stackState, /no_copy
      for z = 0, xyzDim[2]-1 do begin
         image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
         szI = size(image, /dim)
         if ((szI[0] eq xyzDim[0]) and (szI[1] eq xyzDim[1])) then volData1[*,*,z] = image; bytScl(image, min = 0, top = 255)
      endFor
   endif else for i = 0, (oGroupReference->count()-1) do volData1[*((oGroupReference->get(position = i))->getpWherePoints())] = *((oGroupReference->get(position = i))->getpPointValues())

   rgb_table0 = bytArr(256,3) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,*,*])
   whereNo = where(volData1 lt ((*((oGroupReference->getpVolState()))).bottomValues[0]))
   if (whereNo[0] ne -1) then volData1[whereNo] = 0
   whereNo = where(volData1 gt ((*((oGroupReference->getpVolState()))).topValues[0]))
   if (whereNo[0] ne -1) then volData1[whereNo] = 0

   case (*((oGroupReference->getpVolState()))).opacFlag[0] of
      0: begin
         whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
            *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
            opacVect_0 = make_array(256, /byte, value = opacVal)
            opacVect_0[0 : ((*((oGroupReference->getpVolState()))).bottomValues[0])] = 0
            if (((*((oGroupReference->getpVolState()))).topValues[0]) le 254) then opacVect_0[((*((oGroupReference->getpVolState()))).topValues[0])+1 : 255] = 0
         endif
      endcase
      else: opacVect_0 = bytArr(256) + (*((oGroupReference->getpVolState()))).opacValues[0,*]
   endcase

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Cutting Plane x'))[0]
   if (whParam ne -1) then begin
      x1 = fix((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = x1
      if (x1 le 0) then factorX1 = 1 else factorX1 = -1
   endif else x1 = 0

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Cutting Plane y'))[0]
   if (whParam ne -1) then begin
      y1 = fix((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = y1
      if (y1 le 0) then factorY1 = 1 else factorY1 = -1
   endif else y1 = 0

   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Cutting Plane z'))[0]
   if (whParam ne -1) then begin
      z1 = fix((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = z1
      if (z1 le 0) then factorZ1 = 1 else factorZ1 = -1
   endif else z1 = 0

      ; get 2nd object and object Params
   whereName = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Volume Object'))[0]
   if (whereName ne -1) then begin
      strSelect = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Volume Object'))[0]]
      case 1 of
      ((strPos(strSelect, 'Channel') ne -1) or (strPos(strSelect, 'channel') ne -1)): begin
         strNum = s_getRightNumberFromString(strSelect)
         clusPos = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Cluster Position'))[0]]
         tPos = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Time Position'))[0]]
         oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = strNum, clusPos = clusPos, fileName = fileName)
      endcase

      ((strPos(strSelect, 'Time') ne -1) or (strPos(strSelect, 'time') ne -1)): begin
         strNum = s_getRightNumberFromString(strSelect)
         clusPos = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Cluster Position'))[0]]
         chPos = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Channel Position'))[0]]
         oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = strNum, chPos = chPos, clusPos = clusPos, fileName = fileName)
      endcase

      ((strPos(strSelect, 'Cluster') ne -1) or (strPos(strSelect, 'cluster') ne -1)): begin
         strNum = s_getRightNumberFromString(strSelect)
         clusPos = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Cluster Position'))[0]]
         tPos = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Time Position'))[0]]
         chPos = *(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Channel Position'))[0]]
         chPos = 1
         oROI3DGroup = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, clusPos = strNum, fileName = fileName)
      endcase
      else:
      endcase
   endif

   if obj_valid(oROI3DGroup) then begin
      volData2 = bytArr(xyzDim[0], xyzDim[1], xyzDim[2])
      for i = 0, (oROI3DGroup->count()-1) do volData2[*((oROI3DGroup->get(position = i))->getpWherePoints())] = *((oROI3DGroup->get(position = i))->getpPointValues())
      rgb_table1 = (bytArr(256,3) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,*,*]))

      whereNo = where(volData2 lt ((*((oGroupReference->getpVolState()))).bottomValues[1]))
      if (whereNo[0] ne -1) then volData2[whereNo] = 0
      whereNo = where(volData2 gt ((*((oGroupReference->getpVolState()))).topValues[1]))
      if (whereNo[0] ne -1) then volData2[whereNo] = 0

      case (*((oGroupReference->getpVolState()))).opacFlag[1] of
      0: begin
         whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
            *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
            opacVect_1 = make_array(256, /byte, value = opacVal)
            opacVect_1[0 : ((*((oGroupReference->getpVolState()))).bottomValues[1])] = 0
            if (((*((oGroupReference->getpVolState()))).topValues[1]) le 254) then opacVect_1[((*((oGroupReference->getpVolState()))).topValues[1])+1 : 255] = 0
         endif
      endcase
      else: opacVect_1 = bytArr(256) + (*((oGroupReference->getpVolState()))).opacValues[1,*]
      endcase

      whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Cutting Plane x'))[0]
      if (whParam ne -1) then begin
         x2 = fix((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[0]+1)) < (xyzDim[0]-1))
         *(*(oGroupReference->getpParamStruct())).pValues[whParam] = x2
         if (x2 le 0) then factorX2 = 1 else factorX2 = -1
      endif else x2 = 0

      whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Cutting Plane y'))[0]
      if (whParam ne -1) then begin
         y2 = fix((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[1]+1)) < (xyzDim[1]-1))
         *(*(oGroupReference->getpParamStruct())).pValues[whParam] = y2
         if (y2 le 0) then factorY2 = 1 else factorY2 = -1
      endif else y2 = 0

      whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Cutting Plane z'))[0]
      if (whParam ne -1) then begin
         z2 = fix((*(*(oGroupReference->getpParamStruct())).pValues[whParam] > (-xyzDim[2]+1)) < (xyzDim[2]-1))
         *(*(oGroupReference->getpParamStruct())).pValues[whParam] = z2
         if (z2 le 0) then factorZ2 = 1 else factorZ2 = -1
      endif else z2 = 0
    endif

   oGroupReference->getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv

   case (*(*(oGroupReference->getpParamStruct())).pValues[(where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Merge Volumes'))[0]]) of
   'off': begin
         
      oObjectModel->add, obj_new('IDLgrVolume', data0 = volData1,$
                                                opacity_table0 = opacVect_0,$
                                                rgb_table0 = rgb_table0,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                cutting_plane = [[factorX1,0,0, x1], [0,factorY1,0, y1], [0,0,factorZ1, z1]],$
                                                ambient = [255, 255, 255])

      if obj_valid(oROI3DGroup) then $
         oObjectModel->add, obj_new('IDLgrVolume', data0 = volData2,$
                                                   opacity_table0 = opacVect_1,$
                                                   rgb_table0 = rgb_table1,$
                                                   /interpolate, uValue = modelName2, /zero,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   cutting_plane = [[factorX2,0,0, x2], [0,factorY2,0, y2], [0,0,factorZ2, z2]],$
                                                   ambient = [255, 255, 255],lighting_model = 1)
   endcase
             
   'on': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0
      
      volData_0 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,0,*]))[volData1]
      volData_1 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,1,*]))[volData1]
      volData_2 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,2,*]))[volData1]

      case ((*((oGroupReference->getpVolState()))).opacFlag[0]) of
      0: begin
         whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
            *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
            volData_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
            whereI = where(volData1 le (*((oGroupReference->getpVolState()))).bottomValues[0])
            if (whereI[0] ne -1) then volData_3[whereI] = 0
            if (((*((oGroupReference->getpVolState()))).topValues[0]) le 254) then begin
               whereI = where(volData1 ge (*((oGroupReference->getpVolState()))).topValues[0])
               if (whereI[0] ne -1) then volData_3[whereI] = 0
            endif
         endif
      endcase

      else: volData_3 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).opacValues[0,*]))[volData1]

      endcase

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData2[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData2[x2:*,*,*] = 0
         if (y2 lt 0) then volData2[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData2[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData_0 = byte((fix(temporary(volData_0) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,0,*]))[volData2])) < 255)
         volData_1 = byte((fix(temporary(volData_1) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,1,*]))[volData2])) < 255)
         volData_2 = byte((fix(temporary(volData_2) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,2,*]))[volData2])) < 255)

         case (*((oGroupReference->getpVolState()))).opacFlag[1] of

         0: begin
            whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Volume Opacity'))[0]
            if (whParam ne -1) then begin
                opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
                *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
                volData2_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
                whereI = where(volData2 le (*((oGroupReference->getpVolState()))).bottomValues[1])
                if (whereI[0] ne -1) then volData2_3[whereI] = 0
                if (((*((oGroupReference->getpVolState()))).topValues[1]) le 254) then begin
                   whereI = where(volData2 ge (*((oGroupReference->getpVolState()))).topValues[1])
                   if (whereI[0] ne -1) then volData2_3[whereI] = 0
                endif
             endif
          endcase

          else: volData2_3 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).opacValues[1,*]))[volData2]

          endcase

         volData_3 >= volData2_3
      endif

      oObjectModel->add, obj_new('IDLgrVolume', volume_select = 2,$
                                                data0 = volData_0,$
                                                data1 = volData_1,$
                                                data2 = volData_2,$
                                                data3 = volData_3,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                ambient = [255, 255, 255], lighting_model = 1)
   endcase

   'on_1': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData_0 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,0,*]))[volData1]
      volData_1 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,1,*]))[volData1]
      volData_2 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,2,*]))[volData1]

      case ((*((oGroupReference->getpVolState()))).opacFlag[0]) of
      0: begin
         whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
            *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
            volData_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
            whereI = where(volData1 le (*((oGroupReference->getpVolState()))).bottomValues[0])
            if (whereI[0] ne -1) then volData_3[whereI] = 0
            if (((*((oGroupReference->getpVolState()))).topValues[0]) le 254) then begin
               whereI = where(volData1 ge (*((oGroupReference->getpVolState()))).topValues[0])
               if (whereI[0] ne -1) then volData_3[whereI] = 0
            endif
         endif
      endcase

      else: volData_3 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).opacValues[0,*]))[volData1]
      endcase

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData1[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData1[x2:*,*,*] = 0
         if (y2 lt 0) then volData1[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData1[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData_0 = (fix(temporary(volData_0) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,0,*]))[volData2])) < 255
         volData_1 = (fix(temporary(volData_1) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,1,*]))[volData2])) < 255
         volData_2 = (fix(temporary(volData_2) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,2,*]))[volData2])) < 255

         case ((*((oGroupReference->getpVolState()))).opacFlag[1]) of
         0: begin
            whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Volume Opacity'))[0]
            if (whParam ne -1) then begin
               opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
               *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
               volData2_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
               whereI = where(volData2 le (*((oGroupReference->getpVolState()))).bottomValues[1])
               if (whereI[0] ne -1) then volData2_3[whereI] = 0
               if (((*((oGroupReference->getpVolState()))).topValues[1]) le 254) then begin
                  whereI = where(volData2 ge (*((oGroupReference->getpVolState()))).topValues[1])
                  if (whereI[0] ne -1) then volData2_3[whereI] = 0
               endif
            endif
         endcase

         else: volData2_3 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).opacValues[1,*]))[volData2]
         endcase

         rgb_table0 >= rgb_table1
      endif

      oObjectModel->add, obj_new('IDLgrVolume', volume_select = 2,$
                                                data0 = volData_0,$
                                                data1 = volData_1,$
                                                data2 = volData_2,$
                                                data3 = volData_3,$
                                                opacity_table0 = make_array(256, /byte, /index),$
                                                rgb_table0 = rgb_table0,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                ambient = [255, 255, 255], lighting_model = 1)
   endcase

   'on_2': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData_0 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,0,*]))[volData1]
      volData_1 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,1,*]))[volData1]
      volData_2 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,2,*]))[volData1]

      case (*((oGroupReference->getpVolState()))).opacFlag[0] of
      0: begin
         whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '1st Volume Opacity'))[0]
         if (whParam ne -1) then begin
            opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
            *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
            volData_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
            whereI = where(volData1 le (*((oGroupReference->getpVolState()))).bottomValues[0])
            if (whereI[0] ne -1) then volData_3[whereI] = 0
            if (((*((oGroupReference->getpVolState()))).topValues[0]) le 254) then begin
               whereI = where(volData1 ge (*((oGroupReference->getpVolState()))).topValues[0])
               if (whereI[0] ne -1) then volData_3[whereI] = 0
            endif
         endif
      endcase

      else: volData_3 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).opacValues[0,*]))[volData1]
      endcase

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData1[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData1[x2:*,*,*] = 0
         if (y2 lt 0) then volData1[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData1[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData_0 = (fix(temporary(volData_0) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,0,*]))[volData2])) < 255
         volData_1 = (fix(temporary(volData_1) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,1,*]))[volData2])) < 255
         volData_2 = (fix(temporary(volData_2) + (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).rgbValues[1,2,*]))[volData2])) < 255

         case (*((oGroupReference->getpVolState()))).opacFlag[1] of

         0: begin
            whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq '2nd Volume Opacity'))[0]
            if (whParam ne -1) then begin
               opacVal = (*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 0) < 255
               *(*(oGroupReference->getpParamStruct())).pValues[whParam] = opacVal
               volData2_3 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte, value = opacVal)
               whereI = where(volData2 le (*((oGroupReference->getpVolState()))).bottomValues[1])
               if (whereI[0] ne -1) then volData2_3[whereI] = 0
               if (((*((oGroupReference->getpVolState()))).topValues[1]) le 254) then begin
                  whereI = where(volData2 ge (*((oGroupReference->getpVolState()))).topValues[1])
                  if (whereI[0] ne -1) then volData2_3[whereI] = 0
               endif
            endif
         endcase

         else: volData2_3 = (bytArr(256) + transpose((*((oGroupReference->getpVolState()))).opacValues[1,*]))[volData2]
         endcase

         volData_3 = volData2_3
         rgb_table0 >= rgb_table1
      endif

      oObjectModel->add, obj_new('IDLgrVolume', volume_select = 2,$
                                                data0 = volData_0,$
                                                data1 = volData_1,$
                                                data2 = volData_2,$
                                                data3 = volData_3,$
                                                opacity_table0 = make_array(256, /byte, /index),$
                                                rgb_table0 = rgb_table0,$
                                                /interpolate, uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                cutting_plane = [[factorX2,0,0, x2], [0,factorY2,0, y2], [0,0,factorZ2, z2]],$
                                                ambient = [255, 255, 255], lighting_model = 1)
   endcase

   'one_in_two': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData1 /= 2
      rgb_table0 = byte(congrid(rgb_table0, 128, 3))
      opacVect_0 = byte(congrid(opacVect_0, 128))

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData2[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData2[x2:*,*,*] = 0
         if (y2 lt 0) then volData2[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData2[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData2 = temporary(volData2) / 2 + 128b
         rgb_table0 = [rgb_table0, byte(congrid(rgb_table1, 128, 3))]
         opacVect_0 = [opacVect_0, byte(congrid(opacVect_1, 128))]

         whereObj = where(volData1 ne 0)
         if (whereObj[0] ne -1) then volData2[whereObj] = volData1[whereObj]
         oObjectModel->add, obj_new('IDLgrVolume', data0 = volData2,$
                                                   opacity_table0 = opacVect_0,$
                                                   rgb_table0 = rgb_table0,$
                                                   uValue = modelName1, /zero,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   ambient = [255, 255, 255], lighting_model = 1)
      endif else begin
         rgb_table0 = [rgb_table0, bytArr(128,3)]
         opacVect_0 = [opacVect_0, bytArr(128)]
         oObjectModel->add, obj_new('IDLgrVolume', data0 = volData1,$
                                                   opacity_table0 = opacVect_0,$
                                                   rgb_table0 = rgb_table0,$
                                                   uValue = modelName1, /zero,$
                                                   xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                   ambient = [255, 255, 255], lighting_model = 1)
      endelse
   endcase

   'two_in_one': begin
      if (x1 lt 0) then volData1[0: -(x1+1),*,*] = 0
      if (x1 gt 0) then volData1[x1:*,*,*] = 0
      if (y1 lt 0) then volData1[*, 0: -(y1+1),*] = 0
      if (y1 gt 0) then volData1[*, y1:*,*] = 0
      if (z1 lt 0) then volData1[*,*, 0: -(z1+1)] = 0
      if (z1 gt 0) then volData1[*,*, z1:*] = 0

      volData1 /= 2
      rgb_table0 = byte(congrid(rgb_table0, 128, 3))
      opacVect_0 = byte(congrid(opacVect_0, 128))

      if obj_valid(oROI3DGroup) then begin
         if (x2 lt 0) then volData2[0: -(x2+1),*,*] = 0
         if (x2 gt 0) then volData2[x2:*,*,*] = 0
         if (y2 lt 0) then volData2[*, 0: -(y2+1),*] = 0
         if (y2 gt 0) then volData2[*, y2:*,*] = 0
         if (z2 lt 0) then volData2[*,*, 0: -(z2+1)] = 0
         if (z2 gt 0) then volData2[*,*, z2:*] = 0

         volData2 /= 2
         rgb_table0 = [rgb_table0, byte(congrid(rgb_table1, 128, 3))]
         opacVect_0 = [opacVect_0, byte(congrid(opacVect_1, 128))]

         whereObj = where(volData2 gt 0)
         if (whereObj[0] ne -1) then volData1[whereObj] = (volData2[whereObj] + 128b)

      endif else begin
         rgb_table0 = [rgb_table0, bytArr(128,3)]
         opacVect_0 = [opacVect_0, bytArr(128)]
      endelse

      oObjectModel->add, obj_new('IDLgrVolume', data0 = volData1,$
                                                opacity_table0 = opacVect_0,$
                                                rgb_table0 = rgb_table0,$
                                                uValue = modelName1, /zero,$
                                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv)
   endcase
   else:
   endcase

   clusPos  = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Cluster Position'))[0]]    
   chPos    = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Channel Position'))[0]]
   tPos     = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Time Position'))[0]] 

   oObjectModel->getproperty, TRANSFORM = matrixModel
   matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
   ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
   oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix
   if obj_valid(oROI3DGroup) then obj_destroy, oROI3DGroup 
end

pro s_getoSurfaceIntensityUnDriftedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel
   ; INFO POSICION ACTUAL
   clusPos  = *(((*((oGroupReference->getpParamStruct())))).pValues)[(where(*(((*((oGroupReference->getpParamStruct())))).pNames) eq 'Cluster Position'))[0]]    
   chPos    = *(((*((oGroupReference->getpParamStruct())))).pValues)[(where(*(((*((oGroupReference->getpParamStruct())))).pNames) eq 'Channel Position'))[0]]
   tPos     = *(((*((oGroupReference->getpParamStruct())))).pValues)[(where(*(((*((oGroupReference->getpParamStruct())))).pNames) eq 'Time Position'))[0]] 

   xyzDim = oGroupReference->getxyzDim()
      ; get 1st  object and object parameters
   volData1 = make_array(xyzDim[0], xyzDim[1], xyzDim[2], /byte)
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos
   widget_control, stack_tlb, get_uValue = stackState, /no_copy
      imageStackInfoObject = *stackState.pImageStackInfoObject
   widget_control, stack_tlb, set_uValue = stackState, /no_copy
   for z = 0, xyzDim[2]-1 do begin
      image = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
      szI = size(image, /dim)
      if ((szI[0] eq xyzDim[0]) and (szI[1] eq xyzDim[1])) then volData1[*,*,z] = image
   endFor
   image = 0

   if (n_elements(fUpDateROI3DGroupProperties) eq 0) then fUpDateROI3DGroupProperties = 0b
   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()
   oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oSurfModel)) then oSurfModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oSurfModel) then fSurf = 1b else fSurf = 0b

   fCalib = 0b
   minInt = 0
   maxInt = 0
   for i = 0, (oGroupReference->count())-1 do begin

      if fSurf then begin
         oPoly = oSurfModel->get(position = i)
         if obj_isa(oPoly, 'IDLGRPOLYGON') then oPoly->getProperty, polygons = polygons, data = vertices
      endif

      if (n_elements(vertices) le 0) then vertices = -1
      if (vertices[0] eq -1) then begin
         obj = (oGroupReference->get(position = i))->makePixelObjectInVoxel(/all)
         shade_volume, obj.obj, 0, vertices, polygons, /low
;         normals = COMPUTE_MESH_NORMALS(vertices, polygons)*rebin([1,1,1],3,(size(vertices))[2])
;         vertices = round(temporary(vertices) - normals)
         vertices[0,*] += (obj.minX - obj.pixelFrame)
         vertices[1,*] += (obj.minY - obj.pixelFrame)
         vertices[2,*] += (obj.minZ - obj.pixelFrame)
      endif

      intVec = surf_intensity_r(vertices = vertices, volume = volData1, xyz_size_per_pixel = xyzSizePerPixel, r_pixels = 1, pts_factor = 2)

      vertices[0,*] *= xyzSizePerPixel[0]
      vertices[1,*] *= xyzSizePerPixel[1]
      vertices[2,*] *= xyzSizePerPixel[2]

;      maxInt = 50
;      minInt = 0
;      intVec -= minInt
;      intVec /= ((maxInt - minInt) / 255.)
      
      vert_color = make_array(4, (size(vertices, /dim))[1], /byte)
      rgb_table0 = bytArr(256,3) + transpose((*((oGroupReference->getpVolState()))).rgbValues[0,*,*])
      opacVect = oGroupReference->getOpacVect('1st Volume Opacity')

      vert_color[0,*] = (rgb_table0[*,0])[intVec]
      vert_color[1,*] = (rgb_table0[*,1])[intVec]
      vert_color[2,*] = (rgb_table0[*,2])[intVec]
      vert_color[3,*] = 255 ;opacVect[intVec]

      objNumber = (oGroupReference->get(position = i))->getNumber()
      if fUpDateROI3DGroupProperties then begin
         oObj = oObjectModel->GetByName (strCompress('3DSurfIntUnDriftedModel:'+string(objNumber),/rem))
         if obj_valid(oObj) then oObj->setProperty, vert_colors = vert_color
      endif else begin

        oObjectModel->add, obj_new('IDLgrPolygon', data = vertices, poly = polygons, vert_colors = vert_color, shininess = 128., shading = 1,$
                                                 xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                                 name = strCompress('3DSurfIntUnDriftedModel:'+string(objNumber),/rem), uvalue = 'ObjInOriginalPosition')
      endelse
      vertices = -1
   endfor

      oObjectModel->getproperty, TRANSFORM = matrixModel
      matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oGroupReference, actual3DGroupObject = oGroupReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
      ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
      oObjectModel->setproperty, TRANSFORM = matrixModel # matrixMagix
end
