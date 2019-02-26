;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_Image_TChZ_Projection
;
; PURPOSE:
;       - ImageDifference-Filter-Class.
;
; AUTHOR:
;   Dr. Steffen Härtel (2009)
;   e_mail:shartel@med.uchile.cl
;
; CALLING SEQUENCE:
;   result = obj_new('C_sImageFilter_Image_TChZ_Projection')
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_Image_TChZ_Projection::getImageFilterType
   return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_Image_TChZ_Projection::apply, image = image,$
                        selectedStackObject = selectedStackObject,$
                        stack_tlb = stack_tlb,$
                        tPos = tPos,$
                        chPos = chPos,$
                        zPos = zPos,$
                        clusPos = clusPos,$
                        segPos = segPos,$
                        cut_x = cut_x, cut_y = cut_y

   whPos = (where((*(*self.pParamStruct).pNames) eq 'Max-Value'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then maxP = 1b else maxP = 0b

   whPos = (where((*(*self.pParamStruct).pNames) eq 'Mean-Value'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then maxP = 0b else maxP = 1b

   fProj = 0
   whPos = (where((*(*self.pParamStruct).pNames) eq 'T Projection'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fProj = 1
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Ch Projection'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fProj = 2
   whPos = (where((*(*self.pParamStruct).pNames) eq 'Z Projection'))[0]
   if (*(*self.pParamStruct).pActive)[whPos] then fProj = 3

   whPos = (where((*(*self.pParamStruct).pNames) eq 'Plot Window'))[0]
   case whPos of
   -1: fPlotWin = 0b
   else: if (*(*self.pParamStruct).pActive)[whPos] then fPlotWin = 1b else fPlotWin = 0b
   endcase

   dimI = size(image, /dim)
   s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos, zPos = zPos, totalTNum = totalTNum, totalChNum = totalChNum, totalZNum = totalZNum
   if fPlotWin then xyzCM = make_array(3,totalTNum, /float)

   case fProj of
   1: begin
      case maxP of
      0b: image = ((selectedStackObject->getSelectedImage(tPos = 0, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalTNum))
      1b: image = (selectedStackObject->getSelectedImage(tPos = 0, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
      endcase
      if fPlotWin then begin
         zStack = make_array([dimI,totalZNum], /float)
         for j = 0, totalZNum-1 do zStack[*,*,j] = (selectedStackObject->getSelectedImage(tPos = 0, chPos = chPos, zPos = j))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
         
         xSum = make_array(dimI[0],type = long)
         for j = 0, dimI[0]-1 do xSum[j] = total(zStack[j,*,*])
         xyzCM[0,0] = total(xSum * make_array(dimI[0], /index)) / total(xSum)
         
         ySum = make_array(dimI[1],type = long)
         for j = 0, dimI[1]-1 do ySum[j] = total(zStack[*,j,*])
         xyzCM[1,0] = total(ySum * make_array(dimI[1], /index)) / total(ySum)
         
         zSum = make_array(totalZNum,type = long)
         for j = 0, totalZNum-1 do zSum[j] = total(zStack[*,*,j])
         xyzCM[2,0] = total(zSum * make_array(totalZNum, /index)) / total(zSum)
      endif
      
      for i = 1, totalTNum - 1 do begin
         case maxP of
         0b: image += ( (selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalTNum))
         1b: image >= (selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
         endcase
         if fPlotWin then begin
            zStack = make_array([dimI,totalZNum], /float)
            for j = 0, totalZNum-1 do zStack[*,*,j] = (selectedStackObject->getSelectedImage(tPos = i, chPos = chPos, zPos = j))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
            
            xSum = make_array(dimI[0],type = long)
            for j = 0, dimI[0]-1 do xSum[j] = total(zStack[j,*,*])
            xyzCM[0,i] = total(xSum * make_array(dimI[0], /index)) / total(xSum)
            
            ySum = make_array(dimI[1],type = long)
            for j = 0, dimI[1]-1 do ySum[j] = total(zStack[*,j,*])
            xyzCM[1,i] = total(ySum * make_array(dimI[1], /index)) / total(ySum)
            
            zSum = make_array(totalZNum,type = long)
            for j = 0, totalZNum-1 do zSum[j] = total(zStack[*,*,j])
            xyzCM[2,i] = total(zSum * make_array(totalZNum, /index)) / total(zSum)
         endif
      endfor
   endcase
   2: begin
      case maxP of
      0b: image = ((selectedStackObject->getSelectedImage(tPos = tPos, chPos = 0, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalChNum))
      1b: image = (selectedStackObject->getSelectedImage(tPos = tPos, chPos = 0, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
      endcase
      for i = 1, totalChNum - 1 do begin
         case maxP of
         0b: image += ( (selectedStackObject->getSelectedImage(tPos = tPos, chPos = i, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalChNum))
         1b: image >= (selectedStackObject->getSelectedImage(tPos = tPos, chPos = i, zPos = zPos))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
         endcase
      endfor
   endcase
   3: begin
      case maxP of
      0b: image = ((selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = 0))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalZNum))
      1b: image = (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = 0))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
      endcase
      for i = 1, totalZNum - 1 do begin
         case maxP of
         0b: image += ( (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]] / (1.*totalZNum))
         1b: image >= (selectedStackObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = i))[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
         endcase
      endfor
   endcase
   else:
   endcase
   
   if fPlotWin then begin
      iplot, xyzCM[0,*], title='Center of Mass - X', xtitle='Time', ytitle='CM - X';, xrange
      iplot, xyzCM[1,*], title='Center of Mass - Y', xtitle='Time', ytitle='CM - Y'
      iplot, xyzCM[2,*], title='Center of Mass - Z', xtitle='Time', ytitle='CM - Z'
      
      openW,2, 'c:\RSI\xyzDrift.dat'
      printF, 2, ['xDrift', 'yDrift', 'zDrift']
      printF, 2, xyzCM
      close, 2
      
   endif
   return, image
end


function C_sImageFilter_Image_TChZ_Projection::init
   ImageFilterStruct = {Name:'C_Image_TChZ_Projection',$    ;  Filter Name.
               pWidgetType:ptr_new(),$    ; Pointer on Filter Parameter Names.
               pNames:ptr_new(),$     ; Pointer on Filter Parameter Names.
               pActive:ptr_new(),$    ; Pointer on Filter Parameter Active Bool.
               pMin:ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
               pMax:ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
               pValues:ptr_new()}       ; Pointer on Filter Parameter Values.

     ; Filer-Parameters
   filterParamWidgetType = make_array(6, /string, value = 'widget_slider')
   filterParamNames = ['T Projection','Ch Projection','Z Projection','Max-Value', 'Mean-Value', 'Plot Window']
   filterParamActive = [0, 0, 1, 1, 0, 0]
   filterParamMin = [0, 0, 0, 0, 0, 0]
   filterParamMax = [1, 1, 1, 1, 1, 1]
   filterParamValues = [0, 0, 1, 1, 0, 0]

   ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
   ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
   ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
   ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
   ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
   ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

   self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
   return, 1
end

pro C_sImageFilter_Image_TChZ_Projection__define
   tmp = {C_sImageFilter_Image_TChZ_Projection, pParamStruct:ptr_new(), inherits C_sImageFilter}
end