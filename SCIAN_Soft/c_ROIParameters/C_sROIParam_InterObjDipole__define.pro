;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_InterObjDipole
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;   Dr. Steffen Härtel (2003)
;   e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;        result = obj_new('C_sROIParam_InterObjDipole' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_InterObjDipole::apply, mask = mask, xySizePerPixel = xySizePerPixel, position = position, C_sROIGroupObj = C_sROIGroupObj, stack_tlb = stack_tlb

   nParams = n_elements((*(*self.pParamStruct).pNames))
   whParam = (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[0]))[0]
   for i = 1, nParams-1 do  whParam = [whParam, (where( *(*self.pParamStruct).pNames eq (*(*self.pParamStruct).pNames)[i]))[0] ]

      ; check Active Parameter
   whParamActive = whParam * 0
   case (n_elements(position) gt 0) of
      1: if (position[0] eq -1) then return else whParamActive[position] = 1
      else: whParamActive = (*(*self.pParamStruct).pActive)[whParam]
   endcase

      ; check Pointers
   wherePA = where(whParamActive eq 1)
   if (wherePA[0] eq -1) then return
   for i = 0, n_elements(wherePA)-1 do if not(ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect)) then (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new(-1, /no_copy)

   dimI = size(mask, /dim)
   nObjects = C_sROIGroupObj->count()
   if (nObjects le 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
   endif else begin

            ; set Object Number Vector
      *(*self.pParamStruct).pROINumberVect = C_sROIGroupObj->getObjectNumberVector()

      fSaveResult = (*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Save Results & Image'))[0]] eq 1

         ; set xy-Distance
      if (not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] and $
          not(*(*(*self.pValueStruct)[whParam[0]]).pActive)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] ) then xySizePerPixel = [1.,1.]
      (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real x-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[0]
      (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Use Real y-Size (On) or Unit Size (off)'))[0]] = xySizePerPixel[1]

           ; Get xy-objInd -> objInd[objInd[i]:objInd[i+1]-1] contain postition of ith object in xyPoints!
      xyPoints = ((C_sROIGroupObj->get(position = 0))->getxyzPoints())[0:1,*]
      nPoints = (size(xyPoints, /dim))[1]
      xPoints = reform(xyPoints[0,*], nPoints)
      yPoints = reform(xyPoints[1,*], nPoints)
      objInd = [0, nPoints]
      for i = 1, nObjects-1 do begin
         xyPoints = ((C_sROIGroupObj->get(position = i))->getxyzPoints())[0:1,*]
         nPoints = (size(xyPoints, /dim))[1]
         xPoints = [xPoints, reform(xyPoints[0,*], nPoints)]
         yPoints = [yPoints, reform(xyPoints[1,*], nPoints)]
         objInd = [objInd, objInd[i] + nPoints]
      endfor

         ; set Pixel-Positions [m] = [µm] * 1.E-6
      xPos = xPoints * (xySizePerPixel[0] * 1.E-6)
      yPos = yPoints * (xySizePerPixel[1] * 1.E-6)

         ; get Dipole Density Difference µ [D/Angström²]
      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Dipole Density Difference [µ]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then mu = (*(*(*self.pValueStruct)[0]).pValues)[whParam] else mu = 1.

         ; get Dielectric Constant [eps]
      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Dielectric Constant [eps]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then eps = ((*(*(*self.pValueStruct)[0]).pValues)[whParam]) else eps = 7.

         ; set dielectric constant [C/Vm]
      eps0 = 8.85E-12

         ; [Debye/Anström²] *1.E20 -> [D/m²], [D/m²] *3.34E-30 -> [C/m] (1 D = 3.34E-30 Cm)
      mu = mu * double(1.E20 * 3.34E-30)

         ; calculate dipole moment of one pixel [Cm], [m²] = [µm²] * 1.E-12
      d = mu * xySizePerPixel[0] * xySizePerPixel[1] * 1.E-12

         ; calculate unity dipole field for one pixel [Vm²] = [Cm] / [C/Vm]
      dipoleField = d / (4*!pi*eps*eps0)

         ; calculate unity dipole energy for one pixel [VCm³] = [C²m²] / [C/Vm]
      dipoleEnergy = d*d / (4*!pi*eps*eps0)

         ; xy-Position of Electrode
      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Electrode | x-Position [pixel]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then elecXYPos = [ ((*(*(*self.pValueStruct)[0]).pValues)[whParam]), 1] else elecXYPos = floor(dimI / 2.)

      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Electrode | y-Position [pixel]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then elecXYPos[1] = ((*(*(*self.pValueStruct)[0]).pValues)[whParam]) else elecXYPos[1] = floor(dimI[1] / 2.)

      elecXYPos >= [1,1]
      elecXYPos <= (dimI - 1)
      (*(*(*self.pValueStruct)[0]).pValues)[whParam-1: whParam] = elecXYPos
      print, 'Electrode | xy-Position [pixel]', elecXYPos
      elecXYPos *= (xySizePerPixel[0:1] * 1.E-6)
      print, 'Electrode | xy-Position [x]', elecXYPos

         ; Electric field of electrode
      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Electrode | Hight above Surface [m]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then elecHight = ((*(*(*self.pValueStruct)[0]).pValues)[whParam]) else elecHight = 0.0002

      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Electrode | Potential [V]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then elecPot = ((*(*(*self.pValueStruct)[0]).pValues)[whParam]) else elecPot = 300.

      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Electrode | Length [m]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then elecLength = ((*(*(*self.pValueStruct)[0]).pValues)[whParam]) else elecLength = 0.02

      whParam = (where( *(*(*self.pValueStruct)[0]).pNames eq 'Electrode | Diameter [m]'))[0]
      if (*(*(*self.pValueStruct)[0]).pActive)[whParam] then elecDiameter = ((*(*(*self.pValueStruct)[0]).pValues)[whParam]) else elecDiameter = 0.00003

         ; calculate inter domain dipole energy [J]
      if whParamActive[0] then begin
            ; calculate inter pixel energy [VC or J] = [VCm³] * [1/m³]
         dipoleImage = mask * 0.
         dimI = size(dipoleImage, /dim)
         window, 10, xSize = dimI[0], ySize = dimI[1]
         for j = objInd[0], objInd[1]-1 do $
            dipoleImage[xPoints[j],yPoints[j]] = total(dipoleEnergy $
                                                       /    (sqrt( (xPos[objInd[1]:*] - xPos[j])^2 + $
                                                                   (yPos[objInd[1]:*] - yPos[j])^2 ) )^3 )
         for i = 1, nObjects-2 do begin
            for j = objInd[i], objInd[i+1]-1 do begin
               dipoleImage[xPoints[j],yPoints[j]] = total(dipoleEnergy $
                                                          /    (sqrt( ([xPos[0:objInd[i]-1], xPos[objInd[i+1]:*] ] - xPos[j])^2 + $
                                                                      ([yPos[0:objInd[i]-1], yPos[objInd[i+1]:*] ] - yPos[j])^2 ) )^3 )
            endfor
            tvscl, dipoleImage
         endfor
         for j = objInd[nObjects-1], objInd[nObjects]-1 do $
            dipoleImage[xPoints[j],yPoints[j]] = total(dipoleEnergy $
                                                          /    (sqrt( (xPos[0:objInd[nObjects-1]-1] - xPos[j])^2 + $
                                                                      (yPos[0:objInd[nObjects-1]-1] - yPos[j])^2 ) )^3 )
         paramVect = fltArr(nObjects)
         for i = 0, nObjects-1 do paramVect[i] = total(dipoleImage[xPoints[objInd[i]: objInd[i+1]-1], yPoints[objInd[i]: objInd[i+1]-1]])
         *(*(*self.pValueStruct)[wherePA[0]]).pROIParamVect = paramVect

         minDI = min(dipoleImage[xPoints[*], yPoints[*]], max = maxDI)
         print,'InterDomainDipoleEnergy Min [J]', minDI
         print,'InterDomainDipoleEnergy Max [J]', maxDI
         dipoleImage[xPoints[*], yPoints[*]] = (dipoleImage[xPoints[*], yPoints[*]] - minDI) / ((maxDI - minDI) / double(254.)) + 1
         iImage, dipoleImage

         if fSaveResult then begin
            s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
            widget_control, stateObj_tlb, get_uValue = state, /no_copy
               fileName = state.currROI2DGroupFileName
            widget_control, stateObj_tlb, set_uValue = state, /no_copy
            fileName = strMid(fileName, 0, strPos(fileName,'.', /reverse_s))
            write_tiff, strCompress(fileName+strcompress('InterDomainDipoleEnergy'+'.tif', /rem)), dipoleImage, 1
            openW,2, strCompress(fileName+strcompress('InterDomainDipoleEnergy'+'.dat', /rem))
               printF, 2, ['InterDomainDipoleEnergy: ','Min', strCompress(minDI), ' [J] and Max', strCompress(maxDI),'[J]']
            close, 2
         endif
      endif

         ; calculate inner domain dipole energy [J]
      if whParamActive[1] then begin

            ; calculate inner pixel energy [VC or J] = [VCm³] * [1/m³]
         dipoleImage = mask * 0.

         for i = 0, nObjects-1 do begin

            case ((objInd[i+1]-1) - objInd[i]) of
               1:
               2:begin
                    dipoleImage[xPoints[objInd[i]],yPoints[objInd[i]]] = total(dipoleEnergy $
                                                                /    (sqrt( (xPos[objInd[i+1]-1] - xPos[objInd[i]])^2 + $
                                                                            (yPos[objInd[i+1]-1] - yPos[objInd[i]])^2 ) )^3 )
                    dipoleImage[xPoints[objInd[i+1]-1],yPoints[objInd[i+1]-1]] = dipoleImage[xPoints[objInd[i]],yPoints[objInd[i]]]
               endcase
               else:begin
                  for j = objInd[i], objInd[i+1]-1 do begin
                     case j of
                        objInd[i]:begin
                           dipoleImage[xPoints[j],yPoints[j]] = total(dipoleEnergy $
                                                                /    (sqrt( (xPos[j+1:objInd[i+1]-1] - xPos[j])^2 + $
                                                                            (yPos[j+1:objInd[i+1]-1] - yPos[j])^2 ) )^3 )
                        endcase
                        objInd[i+1]-1:begin
                           dipoleImage[xPoints[j],yPoints[j]] = total(dipoleEnergy $
                                                                /    (sqrt( (xPos[objInd[i]:objInd[i+1]-2] - xPos[j])^2 + $
                                                                            (yPos[objInd[i]:objInd[i+1]-2] - yPos[j])^2 ) )^3 )
                        endcase
                        else:begin
                           dipoleImage[xPoints[j],yPoints[j]] = total(dipoleEnergy $
                                                                /    (sqrt( ([xPos[objInd[i]:j-1], xPos[j+1:objInd[i+1]-1]] - xPos[j])^2 + $
                                                                            ([yPos[objInd[i]:j-1], yPos[j+1:objInd[i+1]-1]] - yPos[j])^2 ) )^3 )
                        endcase
                     endcase
                  endfor
               endcase
            endcase
         endfor

         paramVect = fltArr(nObjects)
         for i = 0, nObjects-1 do paramVect[i] = total(dipoleImage[xPoints[objInd[i]: objInd[i+1]-1], yPoints[objInd[i]: objInd[i+1]-1]])
         *(*(*self.pValueStruct)[whParamActive[1]]).pROIParamVect = paramVect

         minDI = min(dipoleImage[xPoints[*], yPoints[*]], max = maxDI)
         print,'InnerDomainDipoleEnergy Min [J]', minDI
         print,'InnerDomainDipoleEnergy Max [J]', maxDI
         dipoleImage[xPoints[*], yPoints[*]] = (dipoleImage[xPoints[*], yPoints[*]] - minDI) / ((maxDI - minDI) / double(254.)) + 1
         iImage, dipoleImage

         if fSaveResult then begin
            s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
            widget_control, stateObj_tlb, get_uValue = state, /no_copy
               fileName = state.currROI2DGroupFileName
            widget_control, stateObj_tlb, set_uValue = state, /no_copy
            fileName = strMid(fileName, 0, strPos(fileName,'.', /reverse_s))
            write_tiff, strCompress(fileName+strcompress('InnerDomainDipoleEnergy'+'.tif', /rem)), dipoleImage, 1
            openW,2, strCompress(fileName+strcompress('InnerDomainDipoleEnergy'+'.dat', /rem))
               printF, 2, ['InnerDomainDipoleEnergy: ','Min', strCompress(minDI), ' [J] and Max', strCompress(maxDI),'[J]']
            close, 2
         endif
      endif

          ; calculate Electric Dipole Field [VC/m or J/m] = [VCm²] * [1/m³]
      if whParamActive[2] then begin
         dipoleImage[*] = 0
         wZero = where(mask eq 0)
         if (wZero[0] ne -1 ) then begin
           xZPos = (wZero mod dimI[0]) * (xySizePerPixel[0] * 1.E-6)
           yZPos = floor(wZero / (1.*dimI[0])) * (xySizePerPixel[1] * 1.E-6)
           for i = 0l, n_elements(wZero)-1 do dipoleImage[wZero[i]] = total(dipoleField $
                                                                      / (sqrt( (xZPos[i] - xPos[*])^2 + $
                                                                               (yZPos[i] - yPos[*])^2 ) )^3 )
         endif
         *(*(*self.pValueStruct)[whParamActive[2]]).pROIParamVect = paramVect

         minDI = min(dipoleImage[wZero], max = maxDI)
         print,'ElectricDipoleField Min [J]', minDI
         print,'ElectricDipoleField Max [J]', maxDI
         dipoleImage[wZero] = (dipoleImage[wZero] - minDI) / ((maxDI - minDI) / double(254.)) + 1
         iImage, dipoleImage
         if fSaveResult then begin
            s_ISM_getProjectInfo, stack_tlb = stack_tlb, stateObj_tlb = stateObj_tlb
            widget_control, stateObj_tlb, get_uValue = state, /no_copy
               fileName = state.currROI2DGroupFileName
            widget_control, stateObj_tlb, set_uValue = state, /no_copy
            fileName = strMid(fileName, 0, strPos(fileName,'.', /reverse_s))
            write_tiff, strCompress(fileName+strcompress('ElectricDipoleField'+'.tif', /rem)), dipoleImage, 1
            openW,2, strCompress(fileName+strcompress('ElectricDipoleField'+'.dat', /rem))
               printF, 2, ['ElectricDipoleField: ','Min', strCompress(minDI), ' [J] and Max', strCompress(maxDI),'[J]']
            close, 2
         endif

       endif
       if whParamActive[2] then *(*(*self.pValueStruct)[2]).pROIParamVect = *(*(*self.pValueStruct)[0]).pROIParamVect

       if whParamActive[3] then begin

         ; check influence of electrode hight/diameter/length
;      print, '(1 / .136) = ', alog(4.* elecLength / elecDiameter * sqrt( (2.*elecHight + elecLength)/(2.*elecHight + 3.*elecLength) ))
;      dummy = elecDiameter
;      elecDiameter *= (make_array(100, /float, /ind) / 10.)
;      iPlot, elecDiameter, alog(4.* elecLength / elecDiameter * sqrt( (2.*elecHight + elecLength)/(2.*elecHight + 3.*elecLength) )), title = 'elecDiameter'
;      elecDiameter = dummy
;      dummy = elecDiameter
;      elecHight *= (make_array(100, /float, /ind) / 10.)
;      iPlot, elecHight, alog(4.* elecLength / elecDiameter * sqrt( (2.*elecHight + elecLength)/(2.*elecHight + 3.*elecLength) )), title = 'elecHight'
;      elecDiameter = dummy
;      dummy = elecLength
;      elecLength *= (make_array(100, /float, /ind) / 10.)
;      iPlot, elecLength, alog(4.* elecLength / elecDiameter * sqrt( (2.*elecHight + elecLength)/(2.*elecHight + 3.*elecLength) )), title = 'Length'
;      elecLength = dummy

       constLn = alog(4.* elecLength / elecDiameter * sqrt( (2.*elecHight + elecLength)/(2.*elecHight + 3.*elecLength) ))

          elecFieldXGrad = make_array(dimI, /float)
          elecFieldYGrad = make_array(dimI, /float)
          elecFieldGrad = make_array(dimI, /float)

             ; calculate distance of each pixel to electrode
          elecFieldXGrad[*] = (make_array(dimI[0] * dimI[1], /float, /ind) mod dimI[0] - elecXYPos[0])
          elecFieldYGrad[*] = (floor( make_array(dimI[0] * dimI[1], /float, /ind) / (1.* dimI[0])) - elecXYPos[1])

          elecFieldGrad = sqrt(elecFieldXGrad^2 + elecFieldYGrad^2)

          iImage, (-1.* elecPot / (constLn*eps)) * elecFieldGrad * ( (1./ (elecFieldGrad^2 + (2 * elecLength + elecHight)^2)^1.5 ) )
          iImage, (-1.* elecPot / (constLn*eps)) * elecFieldGrad * ( (-1./ (elecFieldGrad^2 + (elecHight)^2)^1.5) )
          iImage, (-1.* elecPot / (constLn*eps)) * elecFieldGrad * ( (1./ (elecFieldGrad^2 + (2 * elecLength + elecHight)^2)^1.5 ) - (1./ (elecFieldGrad^2 + (elecHight)^2)^1.5) )

          ivector, elecFieldXGrad, elecFieldYGrad

             ; calculate distance of each pixel to electrode
          iImage, elecFieldGrad
          *(*(*self.pValueStruct)[3]).pROIParamVect = *(*(*self.pValueStruct)[3]).pROIParamVect
       endif

   endelse
end


function C_sROIParam_InterObjDipole::init

   ROIParamStruct = {name:'Inter-Object Dipole Interactions',$     ;  ROI Name.
                     type:'Inter ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$   ; Pointer on ROI-Obj Parameter Names.
                     pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                     pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                     pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                     pMax:ptr_new(),$      ; Pointer on ROI-Obj Parameter Max_Values.
                     pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                     pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

   self.pValueStruct = ptr_new(ptrArr(4))
   ROIParamWidgetType = make_array(4, /string, value = 'widget_slider')
   ROIParamNames = ['Inter Domain Dipole Energy [J]',$
                    'Inner Domain Dipole Energy [J]',$
                    'Electric Dipole Field [Jm-1]',$
                    'Electrode | Field Gradient']
   ROIParamActive = [0, 0, 0, 1]
   ROIParamMin = [0, 0, 0, 0]
   ROIParamMax = [0, 0, 0, 0]
   ROIParamValues = [0, 0, 0, 0]
   pROINumberVect = [-1]

   ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
   ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
   ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
   ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
   ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
   ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
   ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)
   self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

   ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[0],$
                 type:'Inter ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$   ; Pointer on ROI-Obj Parameter WidgetType.
                 pNames:ptr_new(),$    ; Pointer on ROI-Obj Parameter Names.
                 pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                 pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                 pMax:ptr_new(),$      ; Pointer on ROI-Obj Parameter Max_Values.
                 pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                 pROIParamVect:ptr_new()}    ; Pointer on ROI-Obj Parameter Vector.

   ROIValueNames = ['Use Real x-Size (On) or Unit Size (off)', 'Use Real y-Size (On) or Unit Size (off)',$
                  'Dipole Density Difference [µ]', 'Dielectric Constant [eps]', 'Save Results & Image',$
                  'Electrode | x-Position [pixel]', 'Electrode | y-Position [pixel]',$
                  'Electrode | Hight above Surface [m]', 'Electrode | Potential [V]', 'Electrode | Length [m]', 'Electrode | Diameter [m]',$
                  'Threshold_1a', 'Threshold_1b',$
                  'Threshold_2a', 'Threshold_2b',$
                  'Threshold_3a', 'Threshold_3b',$
                  'Threshold_4a', 'Threshold_4b']
   ROIValueActive = [1, 1, 1, 1, 1, 0, 0, 0.,0.,0, 0, 0., 0, 0, 0, 0, 0, 0, 0]
   ROIValueMin = [0.,0., 0.,0.,0., 0.,0.,0.,0.,0.,0.,0., 0., 0.,0., 0.,0., 0.,0.]
   ROIValueMax = [100.,100., 1.,10.,1., 1000., 1000. ,1., 1., 1000., 1., 1.,1., 1., 1., 1.,1., 1.,1.]
   ROIValueValues = [1.,1.,6.E-3, 7., 0., 20, 20, .0002, 300., .02, .00003, 1., 1., 0.,1., 0.,1., 0.,1.]
   ROIValueWidgetType = make_array(n_elements(ROIValueNames), /string, value = 'widget_slider')

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new([-1], /no_copy)
   (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[1],$
                     type:'Inter ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$
                     pNames:ptr_new(),$
                     pActive:ptr_new(),$
                     pMin:ptr_new(),$
                     pMax:ptr_new(),$
                     pValues:ptr_new(),$
                     pROIParamVect:ptr_new()}

   ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
   ROIValueNames = ['Threshold_1a', 'Threshold_1b',$
                 'Threshold_2a', 'Threshold_2b',$
                 'Threshold_3a', 'Threshold_3b',$
                 'Threshold_4a', 'Threshold_4b']
   ROIValueActive = make_array(8, /byte, value = 0)
   ROIValueActive[0] = 1
   ROIValueMin = make_array(8, /float, value = 0)
   ROIValueMax = make_array(8, /float, value = 1)
   ROIValueValues =[0., 1., 0.,1., 0.,1., 0.,1.]
   pROIParamVect = [-1]

   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[2],$
                     type:'Inter ROI-Parameter-Method',$
                     pWidgetType:ptr_new(),$
                     pNames:ptr_new(),$
                     pActive:ptr_new(),$
                     pMin:ptr_new(),$
                     pMax:ptr_new(),$
                     pValues:ptr_new(),$
                     pROIParamVect:ptr_new()}

   ROIValueWidgetType = [ROIValueWidgetType]
   ROIValueNames = [ROIValueNames]
   ROIValueActive = [ROIValueActive]
   ROIValueMin = [ROIValueMin]
   ROIValueMax = [ROIValueMax]
   ROIValueValues =[ROIValueValues]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
   ROIValueStruct.pNames = ptr_new(ROIValueNames)
   ROIValueStruct.pActive = ptr_new(ROIValueActive)
   ROIValueStruct.pMin = ptr_new(ROIValueMin)
   ROIValueStruct.pMax = ptr_new(ROIValueMax)
   ROIValueStruct.pValues = ptr_new(ROIValueValues)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)

   ROIValueStruct = {name:(*(*self.pParamStruct).pNames)[3],$
                 type:'Inter ROI-Parameter-Method',$
                 pWidgetType:ptr_new(),$
                 pNames:ptr_new(),$
                 pActive:ptr_new(),$
                 pMin:ptr_new(),$
                 pMax:ptr_new(),$
                 pValues:ptr_new(),$
                 pROIParamVect:ptr_new()}

   ROIValueWidgetType = [ROIValueWidgetType]
   ROIValueNames = [ROIValueNames]
   ROIValueActive = [ROIValueActive]
   ROIValueMin = [ROIValueMin]
   ROIValueMax = [ROIValueMax]
   ROIValueValues =[ROIValueValues]

   pROIParamVect = [-1]
   ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
   ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
   ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
   ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
   ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
   ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
   ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)
   (*self.pValueStruct)[3] = ptr_new(ROIValueStruct, /no_copy)

   return, 1
end


pro C_sROIParam_InterObjDipole__define
    tmp = {C_sROIParam_InterObjDipole, pParamStruct:ptr_new(),$
                                       pValueStruct:ptr_new(),$
                                       inherits C_sROIParam}
end