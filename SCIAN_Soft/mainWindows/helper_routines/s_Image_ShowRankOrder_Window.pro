;_____________________________IOISIOI____________________
; NAME:
;       s_Image_ShowRankOrder_Window
;
; PURPOSE:
;       Statistics for Colocalization
;
; AUTHOR:
;     Dr. Steffen Härtel (2005)
;     e_mail: haertel@cecs.cl
;
; CALLING SEQUENCE:
;       s_Image_ShowRankOrder_Window, images, application_tlb = application_tlb, groupLeader = groupLeader, stack_tlb = stack_tlb
;
;  KEYWORD PARAMETERS:
;_____________________________IOISIOI____________________


pro s_Image_SRO_mergeColorTables, ev
end


pro s_Image_SRO_getParamValues, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy

     for i = 0, n_elements(state.paramRowNames)-1 do begin
        state.SROParams[0,i] = (*state.pSclArr[i]).minMaxS[0]
        state.SROParams[1,i] = (*state.pSclArr[i]).minMaxS[1]
        state.SROParams[2,i] = (*state.pSclArr[i]).selectS
        state.SROParams[3,i] = (*state.pSclArr[i]).fractS * 100.
     endfor

   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Image_SRO_upDateAlphaImage, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      oImage = (state.thisView->get(position = 0))->getByName('Alpha Image')

      oImage->getProperty, data = thisData, /no_copy
      for i = 0, state.dimI[0]-1 do begin

        yPos = state.dimAlpha[2] - [49 + i * (state.yScale + 10) + state.yScale, 50 + i * (state.yScale + 10)]

        xPos = [30, 29 + round(state.dimI[1]/state.dimI[2]*50)]
        thisData[0,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[0,*])[(*state.pSclArr[i]).thumbC]
        thisData[1,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[1,*])[(*state.pSclArr[i]).thumbC]
        thisData[2,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[2,*])[(*state.pSclArr[i]).thumbC]
        thisData[3,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[3,*])[(*state.pSclArr[i]).thumbC]

        xPos = [90, 89 + state.intScale]
        thisData[0,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[0,*])[state.colorBar]
        thisData[1,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[1,*])[state.colorBar]
        thisData[2,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[2,*])[state.colorBar]
        thisData[3,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[3,*])[state.colorBar]

        xPos = [356, 355 + state.intScale]
        thisData[0,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[0,*])[(*state.pSclArr[i]).barS]
        thisData[1,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[1,*])[(*state.pSclArr[i]).barS]
        thisData[2,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[2,*])[(*state.pSclArr[i]).barS]
        thisData[3,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[3,*])[(*state.pSclArr[i]).barS]

        xPos = [620, 619 + round(state.dimI[1]/state.dimI[2]*50)]
        maskVal = (*state.pSclArr[i]).thumbC gt (*state.pSclArr[i]).selectS
        thisData[0,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[0,*])[((*state.pSclArr[i]).thumbC ge (*state.pSclArr[i]).selectC) * 255]
        thisData[1,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[1,*])[((*state.pSclArr[i]).thumbC ge (*state.pSclArr[i]).selectC) * 255]
        thisData[2,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[2,*])[((*state.pSclArr[i]).thumbC ge (*state.pSclArr[i]).selectC) * 255]
        thisData[3,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ((*state.pClrArr[i]).rgboValues[3,*])[((*state.pSclArr[i]).thumbC ge (*state.pSclArr[i]).selectC) * 255]

      endfor
      oImage->setProperty, data = thisData, /no_copy

      widget_control, state.wDraw, get_value = thisWindow
      thisWindow->draw, state.thisView
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Image_SRO_calcMandersCoeff, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      whereMandersStr = (where(state.paramColumnNames eq 'Manders Coeff'))[0]

      state.SROParams[whereMandersStr[0],0:1] = 0.
      whereSignalChO = where((*state.pSclArr[0]).outS ge (*state.pSclArr[0]).selectS, count)
      if (count gt 0) then begin
         whereSignalCh1 = where( ((*state.pSclArr[1]).outS)[whereSignalChO] gt 1, count)
         if (count gt 0) then begin
           state.SROParams[whereMandersStr[0],0] = total((((*state.pSclArr[1]).outS)[whereSignalChO])[whereSignalCh1]) / total(((*state.pSclArr[0]).outS)[whereSignalChO])
         endif
      endif
      whereSignalCh1 = where((*state.pSclArr[1]).outS ge (*state.pSclArr[1]).selectS, count)
      if (count gt 0) then begin
         whereSignalCh0 = where( ((*state.pSclArr[0]).outS)[whereSignalCh1] gt 1, count)
         if (count gt 0) then begin
           state.SROParams[whereMandersStr[0],1] = total((((*state.pSclArr[0]).outS)[whereSignalCh1])[whereSignalCh0]) / total(((*state.pSclArr[1]).outS)[whereSignalCh1])
         endif
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Image_SRO_sortIndexTable, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy

        ; get intensity threshold
      whereSignal = where( (*state.pSclArr[state.fActiveFromTo[2]]).outS ge (*state.pSclArr[state.fActiveFromTo[2]]).selectS, count )
;      live_image, ((*state.pSclArr[state.fActiveFromTo[2]]).outS ge (*state.pSclArr[state.fActiveFromTo[2]]).selectS) * 255

      window, 11
      whereColocStr = (where(state.paramColumnNames eq 'Coloc Area [%]'))[0]

      mask = bytArr(state.dimI[1], state.dimI[2])
      mask[whereSignal] = 1b
      fActiveFromTo = state.fActiveFromTo[2]
      state.fSilent = 1b
      for i = 0, state.dimI[0]-1 do begin

         if (i ne fActiveFromTo) then begin
            state.fActiveFromTo[[0,2]] = [state.fActiveFromTo[2],i]
            (*state.pSclArr[i]).selectS = min((*state.pSclArr[i]).outS[whereSignal])
            (*state.pSclArr[i]).selectC = ((*state.pSclArr[i]).selectS - (*state.pSclArr[i]).minMaxS[0]) / (((*state.pSclArr[i]).minMaxS[1]-(*state.pSclArr[i]).minMaxS[0])/(state.intScale-1.))
            whereCoSignal = where( (*state.pSclArr[i]).outS ge (*state.pSclArr[i]).selectS, countCoSignal)
            (*state.pSclArr[i]).fractS = 1.* countCoSignal / (state.dimI[1]*state.dimI[2])

            whereColoc = where(mask[whereCoSignal] eq 1, countColoc)
            state.SROParams[whereColocStr[0],i] = 100. * countColoc / countCoSignal

            widget_control, state.wSliderArr[i,0], set_value = (*state.pSclArr[i]).selectS, /upDate
            state.xyFromTo[i,0,*] = [state.xyFromTo[i,0,2], state.xyFromTo[i,0,3], (*state.pSclArr[i]).selectC + 90, state.dimAlpha[2] - 49 + i * (state.yScale + 10) + state.yScale]
            widget_control, wTopBase, set_uValue = state, /no_copy
               s_Image_SRO_upDateBetaImage, wTopBase
            widget_control, wTopBase, get_uValue = state, /no_copy
         endif

         plot, (*state.pSclArr[i]).outS[whereSignal]
      endfor
      state.fActiveFromTo[[0,2]] = [state.fActiveFromTo[2], fActiveFromTo]

      widget_control, wTopBase, set_uValue = state, /no_copy
         s_Image_SRO_upDateBetaImage, wTopBase
      widget_control, wTopBase, get_uValue = state, /no_copy
      state.fSilent = 0b

   widget_control, wTopBase, set_uValue = state, /no_copy
   s_Image_SRO_calcMandersCoeff, wTopBase
end


pro s_Image_SRO_colorTables, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
     for i = 0, state.dimI[0]-1 do begin
        dummy = (congrid((*state.pSclArr[i]).outS[(*state.pSclArr[i]).sortS], state.intScale, /minus) - (*state.pSclArr[i]).minMaxS[0]) / $
                (((*state.pSclArr[i]).minMaxS[1] - (*state.pSclArr[i]).minMaxS[0]) * (1./(state.intScale-1.)))
        for j = 0, state.yScale-1 do (*state.pSclArr[i]).barS[*,j] = dummy
     endfor
   widget_control, wTopBase, set_uValue = state, /no_copy
   s_Image_SRO_upDateAlphaImage, wTopBase
end


pro s_Image_SRO_sortColorTables, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      for i = 0, state.dimI[0]-1 do begin
         dummy = (congrid((*state.pSclArr[i]).outS[(*state.pSclArr[state.fActiveFromTo[2]]).sortS], state.intScale, /minus) - (*state.pSclArr[i]).minMaxS[0]) / $
                 (((*state.pSclArr[i]).minMaxS[1] - (*state.pSclArr[i]).minMaxS[0]) * (1./(state.intScale-1.)))
         for j = 0, state.yScale-1 do (*state.pSclArr[i]).barS[*,j] = dummy
      endfor
   widget_control, wTopBase, set_uValue = state, /no_copy
   s_Image_SRO_sortIndexTable, wTopBase
   s_Image_SRO_upDateAlphaImage, wTopBase
end


pro s_Image_SRO_echoBetaImage, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      oImage = (state.thisView->get(position = 0))->getByName('Beta Image')
      oImage->getProperty, data = thisData, /no_copy

      yPos = state.dimAlpha[2] - [49 + state.fActiveFromTo[2] * (state.yScale + 10) + state.yScale, 50 + state.fActiveFromTo[2] * (state.yScale + 10)]
        ; update Active Bar Marker
      case state.fActiveFromTo[3] of
        0: begin
            sortS = where( (*state.pSclArr[state.fActiveFromTo[2]]).outS ge (*state.pSclArr[state.fActiveFromTo[2]]).selectS, count)
            (*state.pSclArr[state.fActiveFromTo[2]]).fractS = 1.* count / (state.dimI[1]*state.dimI[2])
            state.xyFromTo[state.fActiveFromTo[2], 1, [0,2]] = round([ state.xyFromTo[state.fActiveFromTo[2], 1, 2], 355 + 256 - (*state.pSclArr[state.fActiveFromTo[2]]).fractS * 255.])
         endcase
        1: begin
            state.xyFromTo[state.fActiveFromTo[2], 0, [0,2]] = round( [ state.xyFromTo[state.fActiveFromTo[2], 0, 2], 90 + (*state.pSclArr[state.fActiveFromTo[2]]).barS[state.xyFromTo[state.fActiveFromTo[2], 1, 2]-356, 0]])
            (*state.pSclArr[state.fActiveFromTo[2]]).selectS = (state.xyFromTo[state.fActiveFromTo[2], 0, 2] - 90)*((((*state.pSclArr[state.fActiveFromTo[2]]).minMaxS)[1]-((*state.pSclArr[state.fActiveFromTo[2]]).minMaxS)[0])/255.) $
                                                               + ((*state.pSclArr[state.fActiveFromTo[2]]).minMaxS)[0]
            sortS = where( (*state.pSclArr[state.fActiveFromTo[2]]).outS ge (*state.pSclArr[state.fActiveFromTo[2]]).selectS, count)
            (*state.pSclArr[state.fActiveFromTo[2]]).fractS = 1.* count / (state.dimI[1]*state.dimI[2])
            widget_control, state.wSliderArr[state.fActiveFromTo[2],0], set_value = (*state.pSclArr[state.fActiveFromTo[2]]).selectS, /upDate
         endcase
      endcase

      print, 'selectS', (*state.pSclArr[state.fActiveFromTo[2]]).selectS
      print, 'count', count
      print, 'press:', transpose(state.xyFromTo[state.fActiveFromTo[2], [0,1], 2] - [90,356])
      print, 255 - (*state.pSclArr[state.fActiveFromTo[2]]).fractS * 255., (*state.pSclArr[state.fActiveFromTo[2]]).fractS * 100., '%'

      thisData[3,state.xyFromTo[state.fActiveFromTo[2], abs(state.fActiveFromTo[3]-1), 0],yPos[0]:yPos[1]] = 0b
      thisData[3,state.xyFromTo[state.fActiveFromTo[2], abs(state.fActiveFromTo[3]-1), 2],yPos[0]:yPos[1]] = 255b

      oImage->setProperty, data = thisData, /no_copy
      widget_control, state.wDraw, get_value = thisWindow
      thisWindow->draw, state.thisView
      fViewDataTable = state.fViewDataTable
      fSortIndexTables = state.fSortIndexTables
      fSilent = state.fSilent
   widget_control, wTopBase, set_uValue = state, /no_copy
   if (fSortIndexTables and not(fSilent)) then s_Image_SRO_sortColorTables, wTopBase
   if (fViewDataTable and not(fSilent)) then s_Image_SRO_upDateDataTable, wTopBase
end


pro s_Image_SRO_upDateBetaImage, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      oImage = (state.thisView->get(position = 0))->getByName('Beta Image')
      oImage->getProperty, data = thisData, /no_copy

        ; update Active Frame
      yPos = state.dimAlpha[2] - [49 + state.fActiveFromTo[0] * (state.yScale + 10) + state.yScale, 50 + state.fActiveFromTo[0] * (state.yScale + 10)]
      case state.fActiveFromTo[1] of
        0: begin
          thisData[3,88:89,yPos[0]:yPos[1]] = 0b
          thisData[3,90 + state.intScale:91 + state.intScale,yPos[0]:yPos[1]] = 0b
          thisData[3,88:89 + state.intScale,yPos[0]:yPos[0]+1] = 0b
          thisData[3,88:89 + state.intScale,yPos[1]-1:yPos[1]] = 0b
         endcase
        1: begin
          thisData[3,354:355,yPos[0]:yPos[1]] = 0b
          thisData[3,356 + state.intScale:357 + state.intScale, yPos[0]:yPos[1]] = 0b
          thisData[3,355:356 + state.intScale,yPos[0]:yPos[0]+1] = 0b
          thisData[3,355:356 + state.intScale,yPos[1]-1:yPos[1]] = 0b
         endcase
      endcase

      yPos = state.dimAlpha[2] - [49 + state.fActiveFromTo[2] * (state.yScale + 10) + state.yScale, 50 + state.fActiveFromTo[2] * (state.yScale + 10)]
        ; update Active Bar Marker
      thisData[3,state.xyFromTo[state.fActiveFromTo[2], state.fActiveFromTo[3], 0],yPos[0]:yPos[1]] = 0b
      thisData[3,state.xyFromTo[state.fActiveFromTo[2], state.fActiveFromTo[3], 2],yPos[0]:yPos[1]] = 255b

        ; update Active Frame
      case state.fActiveFromTo[3] of
        0: begin
          thisData[3,88:89,yPos[0]:yPos[1]] = 255b
          thisData[3,90 + state.intScale:91 + state.intScale,yPos[0]:yPos[1]] = 255b
          thisData[3,88:89 + state.intScale,yPos[0]:yPos[0]+1] = 255b
          thisData[3,90:89 + state.intScale,yPos[1]-1:yPos[1]] = 255b
         endcase
        1: begin
          thisData[3,354:355,yPos[0]:yPos[1]] = 255b
          thisData[3,356 + state.intScale:357 + state.intScale,yPos[0]:yPos[1]] = 255b
          thisData[3,355:356 + state.intScale,yPos[0]:yPos[0]+1] = 255b
          thisData[3,355:356 + state.intScale,yPos[1]-1:yPos[1]] = 255b
         endcase
      endcase

      oImage->setProperty, data = thisData, /no_copy
      widget_control, state.wDraw, get_value = thisWindow
      thisWindow->draw, state.thisView
   widget_control, wTopBase, set_uValue = state, /no_copy
   s_Image_SRO_echoBetaImage, wTopBase
end


pro s_Image_SRO_upDateDataTable, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      if state.fViewDataTable then begin
        widget_control, wTopBase, set_uValue = state, /no_copy
          s_Image_SRO_getParamValues, wTopBase
        widget_control, wTopBase, get_uValue = state, /no_copy
        paramTableuValue = {groupLeader:wTopBase,$
                            paramColumnNames:state.paramColumnNames,$
                            paramRowNames:state.paramRowNames,$
                            param:state.SROParams,$
                            name:'RankOrder',$
                            wTopBase:state.child_DataTable_tlb}
         widget_control, wTopBase, set_uValue = state, /no_copy
         s_OPTW_Update, paramTableUValue = paramTableUValue
         widget_control, wTopBase, get_uValue = state, /no_copy
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Image_SRO_upDateROIImage, wTopBase
   widget_control, wTopBase, get_uValue = state, /no_copy
      dimI = size(state.images, /dim)
      if (state.fViewROIMask and (dimI[0] ge 2)) then begin
         image = make_array(state.dimI[1], state.dimI[2], /byte) + state.ROIMasks[0,*,*] * 85 + state.ROIMasks[1,*,*] * 170
         live_control, image, /update, window_in = 's_ImageColoc |-> ROI-Masks'
      endif
   widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Image_SRO_backgroudCorrection, wTopBase
     widget_control, wTopBase, get_uValue = state, /no_copy
       for i = 0, state.dimI[0]-1 do state.images[i,*,*] = (fix(temporary(state.images[i,*,*])) - state.backgroudValues[i]) > 0
       newImages = state.images
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_Image_SRO_update, wTopBase, newImages = newImages
end


pro s_Image_SRO_colors, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = i, /no_copy
      if (tag_names(ev, /structure_name) eq 'XCOLORS_LOAD') then begin
         if (ev.name ne 'Unknown') then begin
            oImage = (state.thisView->get(position = 0))->getByName('Alpha Image')
            oImage->getProperty, data = thisData, /no_copy

            yPos = state.dimAlpha[2] - [49 + i * (state.yScale + 10) + state.yScale, 50 + i * (state.yScale + 10)]
            xPos = [30, 29 + round(state.dimI[1]/state.dimI[2]*50)]
            thisData[0,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.r[(*state.pSclArr[i]).thumbC]
            thisData[1,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.g[(*state.pSclArr[i]).thumbC]
            thisData[2,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.b[(*state.pSclArr[i]).thumbC]
            xPos = [90, 89 + state.intScale]
            thisData[0,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.r[state.colorBar]
            thisData[1,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.g[state.colorBar]
            thisData[2,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.b[state.colorBar]
            xPos = [356, 355 + state.intScale]
            thisData[0,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.r[(*state.pSclArr[i]).barS]
            thisData[1,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.g[(*state.pSclArr[i]).barS]
            thisData[2,xPos[0]:xPos[1],yPos[0]:yPos[1]] = ev.b[(*state.pSclArr[i]).barS]

            oImage->setProperty, data = thisData, /no_copy
            (*state.pClrArr[i]).rgboValues[0,*] = ev.r
            (*state.pClrArr[i]).rgboValues[1,*] = ev.g
            (*state.pClrArr[i]).rgboValues[2,*] = ev.b
            (*state.pClrArr[i]).colTbl = ev.index
            (*state.pClrArr[i]).gamma = ev.gamma
            (*state.pClrArr[i]).opacStretchBotTop = ev.stretch

            oImage->setProperty, data = thisData, /no_copy
            widget_control, state.wDraw, get_value = thisWindow
            thisWindow->draw, state.thisView
         endif
      endif else begin
      XColors, NotifyID = [ev.id, ev.top],$
            group_leader = ev.top,$
            title = strCompress('Color_Table_Ch_' + string(i)),$
            xOffset = 100+50*i, yOffset = 100+100*i,$
            colTbl = (*state.pClrArr[i]).colTbl,$
            rgbValues = make_array(1, 3, 256, /byte) + (*state.pClrArr[i]).rgboValues[0:2,*],$
            gamma = (*state.pClrArr[i]).gamma,$
            stretch = (*state.pClrArr[i]).opacStretchBotTop
      endelse
      widget_control, ev.id, set_uValue = i, /no_copy
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_SRO_vector, ev
     widget_control, ev.top, get_uValue = state, /no_copy
        widget_control, ev.id, get_uValue = uNum, /no_copy
         state.fOpac[uNum] = 1
         if tag_names(ev, /structure_name) eq 'XCOLORS_LOAD' then begin
            iName = strCompress('Image_Channel' + string(uNum), /rem)
            oImage = (state.thisView->get(position = 0))->getByName(iName)
            oImage->getProperty, data = thisData, /no_copy
            thisData[3,*,*] = ev.r[state.images[uNum,*,*]]
            oImage->setProperty, data = thisData, /no_copy
            state.opacValues[uNum,*] = ev.r
            state.opacGamma[uNum] = ev.gamma
            state.opacStretchBotTop[uNum,*] = ev.stretch
            if state.fMergeColorTables then begin
               widget_control, ev.top, set_uValue = state, /no_copy
                 s_Image_SRO_mergeColorTables, {top:ev.top}
               widget_control, ev.top, get_uValue = state, /no_copy
            endif else state.thisWindow->draw, state.thisView
         endif else begin
          opacValues = state.rgbValues[uNum,*,*] * 0
          opacValues[0,0,*] = state.opacValues[uNum,*]
          opacValues[0,1,*] = state.opacValues[uNum,*]
          opacValues[0,2,*] = state.opacValues[uNum,*]
          XColors, NotifyID = [ev.id, ev.top], group_leader = ev.top,$
                    title = strCompress('Color_Blend_Ch_' + string(uNum)),$
                    xOffset = 100, yOffset = 100+100*uNum,$
                    colTbl = 0,$
                    rgbValues = opacValues,$
                    gamma = state.opacGamma[uNum],$
                    stretch = state.opacStretchBotTop[uNum,*]
         endelse
        widget_control, ev.id, set_uValue = uNum, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
    s_Image_SRO_upDateViewColorHist, ev.top
end


pro s_Image_SRO_cleanUp, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
    if (n_elements(state) ne 0) then begin
       obj_destroy, state.thisContainer
       if (state.fViewROIMask) then live_destroy, window_in = 's_ImageColoc |-> ROI-Masks'

       live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Merge Channel'
       if not((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> Merge Channel" does not exist.  Operation not performed.')) then $
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Merge Channel'

       for i = 0,n_tags(state)-1 do begin
          case size(state.(i), /tname) of
           'POINTER': ptr_free, state.(i)
           'OBJREF': obj_destroy, state.(i)
           else:
          endcase
       endfor
   endif
end


pro s_Image_SRO_slider, ev
   widget_control, ev.top, get_uValue = state, /no_copy
;      widget_control, ev.id, get_uValue = uNum, /no_copy
;         state.fOpac[uNum] = 0
;         iName = strCompress('Image_Channel' + string(uNum), /rem)
;         oImage = (state.thisView->get(position = 0))->getByName(iName)
;         oImage->getProperty, data = thisData, /no_copy
;         state.blendValues[uNum] = ev.value
;         thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
;         oImage->setProperty, data = thisData, /no_copy
;      widget_control, ev.id, set_uValue = uNum, /no_copy
;      if state.fMergeColorTables then begin
;         widget_control, ev.top, set_uValue = state, /no_copy
;          s_Image_SRO_mergeColorTables, {top:ev.top}
;         widget_control, ev.top, get_uValue = state, /no_copy
;      endif else state.thisWindow->draw, state.thisView
;   widget_control, ev.top, set_uValue = state, /no_copy
;   s_Image_SRO_upDateDataTable, ev.top
   s_Image_SRO_upDateROIImage, ev.top
end


pro s_Image_SRO_cutBottom_slider, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = uNum, /no_copy
         iName = strCompress('Image_Channel' + string(uNum), /rem)
         oImage = (state.thisView->get(position = 0))->getByName(iName)
         oImage->getProperty, data = thisData, /no_copy
         state.bottomValues[uNum] = ev.value

         state.opacMasks[uNum,*,*] = state.ROIMasks[uNum,*,*]
         case state.fOpac[uNum] of
            0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
            1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
         endcase
         oImage->setProperty, data = thisData, /no_copy
      widget_control, ev.id, set_uValue = uNum, /no_copy
      if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
            s_Image_SRO_mergeColorTables, {top: ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Image_SRO_upDateDataTable, ev.top
   s_Image_SRO_upDateROIImage, ev.top
end


pro s_Image_SRO_cutTop_slider, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.id, get_uValue = uNum, /no_copy
         iName = strCompress('Image_Channel' + string(uNum), /rem)
         oImage = (state.thisView->get(position = 0))->getByName(iName)
         oImage->getProperty, data = thisData, /no_copy
         state.topValues[uNum] = ev.value

         state.opacMasks[uNum,*,*] = state.ROIMasks[uNum,*,*]

         case state.fOpac[uNum] of
           0: thisData[3,*,*] = state.opacMasks[uNum,*,*] * state.blendValues[uNum]
           1: thisData[3,*,*] = (state.opacValues[uNum,*])[state.images[uNum,*,*]]
         endcase
         oImage->setProperty, data = thisData, /no_copy
      widget_control, ev.id, set_uValue = uNum, /no_copy
      if state.fMergeColorTables then begin
         widget_control, ev.top, set_uValue = state, /no_copy
          s_Image_SRO_mergeColorTables, {top:ev.top}
         widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Image_SRO_upDateDataTable, ev.top
   s_Image_SRO_upDateROIImage, ev.top
end


pro s_Image_SRO_expose, ev
   widget_control, ev.top, get_uValue = state, /no_copy
       if state.fMergeColorTables then begin
          widget_control, ev.top, set_uValue = state, /no_copy
             s_Image_SRO_mergeColorTables, {top:ev.top}
          widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_SRO_event, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      widget_control, ev.top, scr_xsize = ev.x
       state.thisWindow->setProperty, dimension = [ev.x, ev.y*state.drawScale+15]
       if state.fMergeColorTables then begin
          widget_control, ev.top, set_uValue = state, /no_copy
              s_Image_SRO_mergeColorTables, {top: ev.top}
          widget_control, ev.top, get_uValue = state, /no_copy
      endif else state.thisWindow->draw, state.thisView
   widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_Image_SRO_mergeColorTablesOnOff, ev
   widget_control, ev.top, get_uValue = state, /no_copy
      state.fMergeColorTables = s_ToggleButtonOnOffState(state.wMergeColorTablesOnOff)
      if (state.fMergeColorTables eq 0) then begin
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 0'
         if (error ne 'No application/applet is present for use with LIVE_INFO.') then live_destroy, window_in = 's_ImageColoc |-> Channel 0'
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Channel 1'
         if (error ne 'No application/applet is present for use with LIVE_INFO.') then live_destroy, window_in = 's_ImageColoc |-> Channel 1'
         live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> Merge Channel'
         if (error ne 'No application/applet is present for use with LIVE_INFO.') then live_destroy, window_in = 's_ImageColoc |-> Merge Channel'
      endif
   widget_control, ev.top, set_uValue = state, /no_copy
   s_Image_SRO_mergeColorTables, ev
   s_Image_SRO_upDateViewColorHist, ev.top
end


pro s_Image_SRO_drawEvents, ev
   widget_control, ev.top, get_uValue = state, /no_copy
   case (([ 'PRESS', 'RELEASE', 'MOTION', 'SCROLL' ])[ev.type]) of
     'PRESS':begin
         case what_button_pressed(ev) of
           'RIGHT': begin
               state.fMap = 1 - state.fMap
               widget_control, state.wFlexBase, map = state.fMap
             endcase
           'LEFT': begin
                  for i = 0, state.dimI[0]-1 do begin
                     yPos = state.dimAlpha[2] - [49 + i * (state.yScale + 10) + state.yScale, 50 + i * (state.yScale + 10)]
                     if ((ev.y ge yPos[0]) and (ev.y le yPos[1])) then begin
                        case 1 of
                          (ev.x ge 356) and (ev.x le 355 + state.intScale): j = 1
                          (ev.x ge 90) and (ev.x le 89 + state.intScale): j = 0
                          else: j = -1
                        endcase
                        if (j ne -1) then begin

                           state.xyFromTo[i,j,*] = [state.xyFromTo[i,j,2], state.xyFromTo[i,j,3], ev.x, ev.y]
                           state.fActiveFromTo = [state.fActiveFromTo[2], state.fActiveFromTo[3], i, j]

                           if (j eq 0) then begin
                              (*state.pSclArr[i]).selectS = (ev.x - 90)*(((*state.pSclArr[i]).minMaxS[1]-(*state.pSclArr[i]).minMaxS[0])/255.) + (*state.pSclArr[i]).minMaxS[0]
                              (*state.pSclArr[i]).selectC = ev.x - 90

                              sortS = where( (*state.pSclArr[i]).outS ge (*state.pSclArr[i]).selectS, count)
                              (*state.pSclArr[state.fActiveFromTo[2]]).fractS = 1. * count / (state.dimI[1] * state.dimI[2])

                              (*state.pSclArr[i]).fractS = ((where(((*state.pSclArr[i]).barS)[*,0] ge (ev.x - 90)))[0] > 0) / (state.intScale-1.)
                              widget_control, state.wSliderArr[i,j], set_value = (*state.pSclArr[i]).selectS, /upDate
                           endif

                           widget_control, ev.top, set_uValue = state, /no_copy
                              s_Image_SRO_upDateAlphaImage, ev.top
                              s_Image_SRO_upDateBetaImage, ev.top
                           widget_control, ev.top, get_uValue = state, /no_copy
                        endif
                     endif
                  endfor
             endcase
           else:
         endcase
       endcase
     else:
   endcase
   widget_control, ev.top, set_uValue = state, /no_copy
end


; TODO Temporary (?) wrapper function that avoids duplicated name conflicts in IDL versions above 7.0
pro s_Image_SRO_update, wTopBase, images = images, newImages = newImages, doNotUseBothKeywordsImagesAndNewImages = doNotUseBothKeywordsImagesAndNewImages
  case 1 of
    keyword_set(images)   : s_Image_SRO_updateImages, wTopBase, images = images
    keyword_set(newImages): s_Image_SRO_updateNewImages, wTopBase, newImages = newImages
  endcase
end


pro s_Image_SRO_updateImages, wTopBase, images = images
  widget_control, wTopBase, get_uValue = state, /no_copy

     for i = 0, state.dimI[0]-1 do begin

       (*state.pSclArr[i]).outS = make_array(state.dimI[1], state.dimI[2]) + images[i,*,*]
       (*state.pSclArr[i]).outC = (make_array(state.dimI[1], state.dimI[2]) + (images[i,*,*] - (*state.pSclArr[i]).minMaxS[0])) / (((*state.pSclArr[i]).minMaxS[1] - (*state.pSclArr[i]).minMaxS[0]) * (1./(state.intScale-1.)))

       (*state.pSclArr[i]).thumbC = congrid((*state.pSclArr[i]).outC, round(state.dimI[1]/state.dimI[2]*50),50)
       (*state.pSclArr[i]).sortS = sort((images)[i,*,*])

       dummy = (congrid(((images)[i,*,*])[(*state.pSclArr[i]).sortS], state.intScale, /minus) - (*state.pSclArr[i]).minMaxS[0]) / (((*state.pSclArr[i]).minMaxS[1]-(*state.pSclArr[i]).minMaxS[0]) * (1./(state.intScale-1.)))
       for j = 0, state.yScale-1 do (*state.pSclArr[i]).barS[*,j] = dummy

     endfor

  widget_control, wTopBase, set_uValue = state, /no_copy
  s_Image_SRO_upDateAlphaImage, wTopBase
  s_Image_SRO_upDateBetaImage, wTopBase
end


pro s_Image_SRO_updateNewImages, wTopBase, newImages = newImages
    widget_control, wTopBase, get_uValue = state, /no_copy

       if (n_elements(newImages) gt 0) then begin
             ; rescales non-byte images
          if (size(newImages, /type) ne 1) then newImages = bytscl(newImages, min = 0, max = 4096)

          dimIN = size(newImages, /dim)
          typeIN = size(newImages, /type)
          if ((dimIN[0] ne state.dimI[0]) or (dimIN[1] ne state.dimI[1]) or  (dimIN[2] ne state.dimI[2])) then begin
             for i = 0, (state.dimI[0]<dimIN[0] )-1 do state.images[i,*,*] = congrid(make_array(dimIN[1], dimIN[2], type = typeIN) + newImages[i,*,*], state.dimI[1], state.dimI[2])
          endif else state.images = newImages
       endif

       for i = 0, state.dimI[0]-1 do begin
         iName = strCompress('Image_Channel' + string(i), /rem)
          oImage = (state.thisView->get(position = 0))->getByName(iName)
          oImage->getProperty, data = thisData, /no_copy
          thisData[0,*,*] = (bytArr(255) + state.rgbValues[i, 0,*])[state.images[i,*,*]]
          thisData[1,*,*] = (bytArr(255) + state.rgbValues[i, 1,*])[state.images[i,*,*]]
          thisData[2,*,*] = (bytArr(255) + state.rgbValues[i, 2,*])[state.images[i,*,*]]

          state.opacMasks[i,*,*] = state.ROIMasks[i,*,*]
          case state.fOpac[i] of
             0: thisData[3,*,*] = state.opacMasks[i,*,*] * state.blendValues[i]
             1: thisData[3,*,*] = (state.opacValues[i,*])[state.images[i,*,*]]
          endcase
         oImage->setProperty, data = thisData, /no_copy
       endfor
       if state.fMergeColorTables then begin
          widget_control, wTopBase, set_uValue = state, /no_copy
              s_Image_SRO_mergeColorTables, {top:wTopBase}
          widget_control, wTopBase, get_uValue = state, /no_copy
       endif else state.thisWindow->draw, state.thisView
    widget_control, wTopBase, set_uValue = state, /no_copy
    s_Image_SRO_upDateDataTable, wTopBase
    s_Image_SRO_upDateROIImage, wTopBase
end


pro s_Image_SRO_control, ev
  widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
    case uValue of
       'FREQHISTLOGONOFF':begin
          state.fBiFreqHisLog = s_ToggleButtonOnOffState(state.wViewFreqHisLogOnOff)
          if state.fViewFreqHist then begin
             widget_control, ev.top, set_uValue = state, /no_copy
                s_Image_SRO_upDateViewFreqHist, ev.top
             widget_control, ev.top, get_uValue = state, /no_copy
          endif
       endcase
       'VIEWROIMASKSONOFF':begin
             state.fViewROIMask = s_ToggleButtonOnOffState(state.wViewROIMaskOnOff)
             if state.fViewROIMask then begin
                live_info, error = error, properties = prop, window_in = 's_ImageColoc |-> ROI-Masks'
                if ((error eq 'No application/applet is present for use with LIVE_INFO.') or (error eq 'Error occurred during LIVE_INFO.  Window "s_ImageColoc |-> ROI-Masks" does not exist.  Operation not performed.')) then begin $
                  image = make_array(state.dimI[1], state.dimI[2], /byte)
                  tvLCT, rr, gg, bb, /get
                  loadCT, 47
                  tvLCT, r, g, b, /get
                  tvLCT, rr, gg, bb
                  if (state.dimI[1] ge state.dimI[2]) then drawDim = [300.*state.dimI[1]/state.dimI[2], 300.] else drawDim = [300., 300.*state.dimI[1]/state.dimI[2]]
                  live_image, image, red = r, green = g, blue = b, draw_dimension = round(drawDim), /no_select, reference_out = refOut, title = 's_ImageColoc |-> ROI-Masks'
                  live_info, refOut.vis, properties = variable, window_in = refOut.Win
                    variable.color = 'Light Gray'
                  live_control, refOut.vis, properties = variable, window_in = refOut.Win
                endif
                widget_control, ev.top, set_uValue = state, /no_copy
                  s_Image_SRO_upDateROIImage, ev.top
                widget_control, ev.top, get_uValue = state, /no_copy
             endif else live_destroy, window_in = 's_ImageColoc |-> ROI-Masks'
       endcase
       'ONEBYEONOFF': begin
             state.fOneByE = s_ToggleButtonOnOffState(state.wOneByEOnOff)
             if (state.fOneByE and state.fOneByESq) then state.fOneByESq = s_ToggleButtonOnOffState(state.wOneByESqOnOff)
       endcase
       'ONEBYESQONOFF': begin
             state.fOneByESq = s_ToggleButtonOnOffState(state.wOneByESqOnOff)
             if (state.fOneByESq and state.fOneByE) then state.fOneByE = s_ToggleButtonOnOffState(state.wOneByEOnOff)
       endcase
       'SORTINDEXTABLESONOFF': begin
             state.fSortIndexTables = s_ToggleButtonOnOffState(state.wSortIndexTablesOnOff)
             if state.fSortIndexTables then begin
                state.fActiveFromTo[0] = (state.fActiveFromTo[2] + 1) mod state.dimI[1]
                widget_control, ev.top, set_uValue = state, /no_copy
                  s_Image_SRO_sortColorTables, ev.top
                widget_control, ev.top, get_uValue = state, /no_copy
             endif else begin
                widget_control, ev.top, set_uValue = state, /no_copy
                   s_Image_SRO_colorTables, ev.top
                widget_control, ev.top, get_uValue = state, /no_copy
             endelse
       endcase
       'SETRANDOMPARAMS':begin
            state.fRandomAndRadius = s_ToggleButtonOnOffState(state.wRandomAndRadiusOnOff)
            if state.fRandomAndRadius then begin
               paramTableuValue = {groupLeader:ev.top,$
                                       paramColumnNames:['Random Steps', 'Radius / Boxsize'],$
                                       param:[state.randomValue, state.radiusValue],$
                                       name:'Colocalization Random and Radius Values',$
                                       wTopBase:state.child_DataTable_tlb}
               s_ObjParamTableWidget, paramTableuValue = paramTableuValue
               state.child_RandomandRadiusTable_tlb = paramTableuValue.wTopBase
            endif else if widget_info(state.child_RandomandRadiusTable_tlb, /valid) then widget_control, state.child_RandomandRadiusTable_tlb, /destroy
       endcase
       'VIEWDATATABLEONOFF':begin
             state.fViewDataTable = s_ToggleButtonOnOffState(state.wViewDataTableOnOff)
             if state.fViewDataTable then begin
                widget_control, ev.top, set_uValue = state, /no_copy
                   s_Image_SRO_getParamValues, ev.top
                widget_control, ev.top, get_uValue = state, /no_copy

                paramTableuValue = {groupLeader:ev.top,$
                                     paramColumnNames:state.paramColumnNames,$
                                     paramRowNames:state.paramRowNames,$
                                     param:state.SROParams,$
                                     name:'RankOrder',$
                                     wTopBase:state.child_DataTable_tlb}
                 s_ObjParamTableWidget, paramTableuValue = paramTableuValue
                 state.child_DataTable_tlb = paramTableuValue.wTopBase
             endif else if widget_info(state.child_DataTable_tlb, /valid) then widget_control, state.child_DataTable_tlb, /destroy
       endcase
       else:
    endcase
    widget_control, ev.id, set_uValue = uValue, /no_copy
  widget_control, ev.top, set_uValue = state, /no_copy
end


function s_Image_SRO_getXYfromIntValue, wTopBase, fActive, Value
  widget_control, wTopBase, get_uValue = state, /no_copy
   case fActive[1] of
     0: begin
       x = 90 + ev.value / ((((*state.pSclArr[fActive[0]]).minMaxS)[1]-((*state.pSclArr[fActive[0]]).minMaxS)[0])/255.)
       y = state.dimAlpha[2] - 49 + fActive[0] * (state.yScale + 10) + state.yScale
      endcase
     1: begin
       x = 90 + ev.value / ((((*state.pSclArr[uVal.fActive[0]]).minMaxS)[1]-((*state.pSclArr[uVal.fActive[0]]).minMaxS)[0])/255.)
       y = state.dimAlpha[2] - 49 + uVal.fActive[0] * (state.yScale + 10) + state.yScale
      endcase
     else:
   endcase
  return, [x,y]
  widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_Image_SRO_window_event, ev
  widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uVal, /no_copy

    case uVal.fActive[1] of
      0: begin

        if (state.fOneByE or state.fOneByESq) then begin
           ev.value = ( state.fOneByE * exp(-1.) + state.fOneByESq * exp(-2.) ) * $
                      ((*state.pSclArr[uVal.fActive[0]]).minMaxS[1] - (*state.pSclArr[uVal.fActive[0]]).minMaxS[0]) + (*state.pSclArr[uVal.fActive[0]]).minMaxS[0]
        endif

        x = 90 + (ev.value - (*state.pSclArr[uVal.fActive[0]]).minMaxS[0]) / (((*state.pSclArr[uVal.fActive[0]]).minMaxS[1]-(*state.pSclArr[uVal.fActive[0]]).minMaxS[0])/255.)
        y = state.dimAlpha[2] - 49 + uVal.fActive[0] * (state.yScale + 10) + state.yScale
        (*state.pSclArr[uVal.fActive[0]]).selectS = ev.value
        (*state.pSclArr[uVal.fActive[0]]).selectC = x - 90
        (*state.pSclArr[uVal.fActive[0]]).fractS = ((where(((*state.pSclArr[uVal.fActive[0]]).barS)[*,0] ge (x - 90)))[0] > 0) / (state.intScale-1.)
       endcase
      else:
    endcase

    state.xyFromTo[uVal.fActive[0],uVal.fActive[1],*] = [state.xyFromTo[uVal.fActive[0],uVal.fActive[1],2], state.xyFromTo[uVal.fActive[0],uVal.fActive[1],3], x, y]
    state.fActiveFromTo = [state.fActiveFromTo[2], state.fActiveFromTo[3], uVal.fActive[0], uVal.fActive[1]]

    widget_control, ev.id, set_uValue = uVal, /no_copy
    if (state.fOneByE or state.fOneByESq) then widget_control, state.wSliderArr[state.fActiveFromTo[2],state.fActiveFromTo[3]], set_value = (*state.pSclArr[state.fActiveFromTo[2]]).selectS, /upDate
  widget_control, ev.top, set_uValue = state, /no_copy
  s_Image_SRO_upDateBetaImage, ev.top
end


function s_Image_SRO_getCorrelationNames
   return, ['Pearsons Correlation (PC)','Overlap Coefficient','B of Fit Y = A + B * X','Corr. Coeff. of Raw Images (CCRI)','Joint Moment of Standardized Images (JMSI)','Cross-Correlation (G[dx])']
end


pro s_Image_ShowRankOrder_Window, images, stack_tlb = stack_tlb, application_tlb = application_tlb, groupLeader = groupLeader, pSclArr = pSclArr, iNames = iNames

    if (n_elements(groupLeader) eq 0) then groupLeader = 0l
    if (n_elements(stack_tlb) eq 0) then stack_tlb = groupLeader
    if (n_elements(fScale) eq 0) then fScale = 1b
    if (n_elements(iNames) eq 0) then iNames = ['Channel 0', 'Channel 1', s_Image_SRO_getCorrelationNames()]

    if (n_elements(images) eq 0) then begin
       filename = filePath(subDir = ['examples', 'data'], 'worldelv.dat')
       openR, lun, filename, /get_lun
       backgroundImage = bytArr(360,360)
       readU, lun, backgroundImage
       free_lun, lun
       filename = filePath(subDir = ['examples', 'data'], 'ctscan.dat')
       openR, lun, filename, /get_lun
       foregroundImage = bytArr(256,256)
       readU, lun, foregroundImage
       free_lun, lun
       images = bytArr(3, 360, 360)
       images[0,*,*] = backgroundImage
       images[1,*,*] = congrid(foregroundImage, 360,360)
       images[2,*,*] = 1./100. * images[0,*,*] * images[1,*,*]
    endif

       ; set image & rank bar parameters
    intScale = 256
    yScale = 50
    dimI = size(images, /dim)
    mask = make_array(dimI[1], dimI[2], /byte, value = 1b)
    fActiveFromTo = [0,0,0,0]
    xyFromTo = make_array(dimI[0], 2, 4, /int)

    alpha_image = make_array(4, 700, dimI[0]*50+200, /float, value = 200b)
    beta_image = make_array(4, 700, dimI[0]*50+200, /float, value = 255b)
    beta_image[3,*,*] = 0b
    dimAlpha = size(alpha_image, /dim)

    colorBar = make_array(intScale, yScale, /byte)
    dummy = make_array(intScale, /byte, /index)
    for i = 0, yScale-1 do colorBar[*, i] = dummy

       ; set Index parameters
    paramNames = ['Min Value','Max Value','Selected Value','Selected Area [%]','Coloc Area [%]','Manders Coeff']
    if not(ptr_valid(pSclArr)) then begin
       pSclArr = ptrArr(dimI[0]+2)

       for i = 0, dimI[0]-1 do begin
          minI = min(images[i,*,*], max = maxI)
          sortS = sort((images)[i,*,*])
          barS = fltArr(intScale, yScale)
          dummy = (congrid(((images)[i,*,*])[sortS], intScale, /minus) - minI) / ((maxI-minI) * (1./(intScale-1.)))
          for j = 0, yScale-1 do barS[*,j] = dummy

          scaleState = {name: iNames[i],$
                        outS: make_array(dimI[1],dimI[2]) + images[i,*,*],$
                        sortS: sortS,$
                        minMaxS: [minI,maxI],$
                        selectS: 0.368 * (maxI-minI) + minI,$
                        fractS: 1.,$
                        barS: barS,$
                        outC: (make_array(dimI[1],dimI[2]) + (images[i,*,*]-minI)) / ((maxI-minI) * (1./(intScale-1.))),$
                        thumbC: make_array(dimI[1]/dimI[2] * 50,50, /float),$
                        minMaxC: [0., intScale-1.],$
                        selectC: 0.}

          sortS = where(scaleState.outS ge scaleState.selectS, count)
          scaleState.fractS = 1. * count / (dimI[1]*dimI[2])
          scaleState.selectC = (scaleState.selectS - scaleState.minMaxS[0]) / ((scaleState.minMaxS[1] - scaleState.minMaxS[0]) / (intScale-1.))
          scaleState.thumbC = congrid(scaleState.outC, round(dimI[1]/dimI[2]*50),50)
          pSclArr[i] = ptr_new(scaleState, /no_copy)
       endfor
    endif

       ; set color parameters & image values
    if not(ptr_valid(pClrArr)) then begin
       pClrArr = ptrArr(dimI[0])
       if (n_elements(colorPalette) eq 0) then colorPalette = [41, 42, 44, 3, 0]
       if (dimI[0] gt n_elements(colorPalette)) then for i = 0, (n_elements(colorPalette)-dimI[0]) do colorPalette = [colorPalette, [43+i]]

       for i = 0, dimI[0]-1 do begin
          loadCT, colorPalette[i]
          tvLCT, r, g, b, /get
          colorState = {colTbl: colorPalette[i],$
                       rgboValues: make_array(4,intScale, /byte),$
                       stretchBotTop: [0,intScale-1],$
                       gamma: 1.,$
                       fOpac: 0b,$
                       opacValue: 255b,$
                       opacValues: make_array(256, /byte, value = opacValue),$
                       opacStretchBotTop: [0,intScale-1],$
                       opacGamma: 1.}

          colorState.rgboValues[0,*] = r
          colorState.rgboValues[1,*] = g
          colorState.rgboValues[2,*] = b
          colorState.rgboValues[3,*] = make_array(intScale, /byte, value = 255b)

          pClrArr[i] = ptr_new(colorState, /no_copy)
       endfor
    endif

    thisContainer = obj_new('IDL_Container')
    thisModel = obj_new('IDLgrModel')
    thisContainer->add, thisModel
    oImage = obj_new('IDLgrImage', alpha_image, dimensions = [dimAlpha[1], dimAlpha[2]], name = 'Alpha Image')
    thisModel->add, oImage
    thisContainer->add, oImage
    oImage = obj_new('IDLgrImage', beta_image, dimensions = [dimAlpha[1], dimAlpha[2]], blend_func = [3,4], name = 'Beta Image')
    thisModel->add, oImage
    thisContainer->add, oImage

    oText = obj_new('IDLgrText', 'Beta Image')
    thisModel->add, oText
    thisContainer->add, oText

      ; Create a view.
    viewRect = [0, 0, dimAlpha[1], dimAlpha[2]]
    thisView = obj_new('IDLgrView', viewPlane_rect = viewRect)
    thisView->add, thisModel
    thisContainer->add, thisView

        ; Create the widgets for this program.
    wTopBase = widget_base(title = 's_RankOrder |->', tlb_size_events = 1)
    application_tlb = wTopBase
    wFlexBase = widget_base(wTopBase, map = 0, column = 1)
    wDraw = widget_draw(wTopBase, xSize = dimAlpha[1], ySize = dimAlpha[2], graphics_level = 2, button_events = 1, expose_events = 1, retain = 2, event_pro = 's_Image_SRO_drawEvents')

      ; Get geometry information for resizing.
    tlbGeo = widget_info(wTopBase, /geometry)
    drawGeo = widget_info(wDraw, /geometry)
    drawScale = float(drawGeo.scr_ySize) / tlbGeo.ySize

      ; realize the widgets and get the window object.
    widget_control, wDraw, /realize
    widget_control, wDraw, get_value = thisWindow
    thisWindow->draw, thisView
    thisContainer->add, thisWindow

    wColorTable = widget_button(wFlexBase, value = 'Color Tables', /menu)
    wOpacityTable = widget_button(wFlexBase, value = 'Opacity Tables', /menu)
    wSliderArr = make_array(dimI[0],2, /long)
    for i = 0, dimI[0]-1 do begin
       wFlexBase2 = widget_base(wFlexBase, column = 1, /frame)
       iTitle = 'Color Table for Channel:' + strCompress(string(i), /rem)
       void = widget_button(wColorTable, value = iTitle, uValue = i, event_pro = 's_Image_SRO_colors')
       iTitle = 'Opacity Table for Channel:' + strCompress(string(i), /rem)
       void = widget_button(wOpacityTable, value = iTitle, uValue = i, event_pro = 's_Image_SRO_vector')
       iTitle = strCompress('Scale_Bar_Ch ' + string(i), /rem)
       wSliderArr[i,0] = cw_fslider(wFlexBase2, min = ((*pSclArr[i]).minMaxS)[0], max = ((*pSclArr[i]).minMaxS)[1], value = (*pSclArr[i]).selectS, title = iTitle, uValue = {fActive: [i,0]}, /edit)
    endfor
    wMergeColorTablesOnOff = widget_button(wColorTable, value = 'Merge Color Tables (off)', event_pro = 's_Image_SRO_mergeColorTablesOnOff', /sep)

    if (dimI[0] lt 2) then sensitive = 0 else sensitive = 1
    wColocTable = widget_button(wFlexBase, value = 'Colocalisation', /menu, sensitive = sensitive)

    void = widget_button(wColocTable, value = 'Plot Windows:', sensitive = 0)
    wRandomAndRadiusOnOff = widget_button(wColocTable, value = '...|-> Set Random Parameters (off)', uValue = 'SETRANDOMPARAMS', event_pro = 's_Image_SRO_control')
    wViewDataTableOnOff = widget_button(wColocTable, value = '...|-> View Data Table (off)', uValue = 'VIEWDATATABLEONOFF',event_pro = 's_Image_SRO_control')
    wViewROIMaskOnOff = widget_button(wColocTable, value = '...|-> View ROI-Masks (off)', uValue = 'VIEWROIMASKSONOFF',event_pro = 's_Image_SRO_control')

    void = widget_button(wColocTable, value = 'Statistics:', sensitive = 0, /sep)
    wOneByEOnOff = widget_button(wColocTable, value = '...|-> Use 1/e criteria (off)', uValue = 'ONEBYEONOFF',event_pro = 's_Image_SRO_control')
    wOneByESqOnOff = widget_button(wColocTable, value = '...|-> Use 1/e² criteria (off)', uValue = 'ONEBYESQONOFF',event_pro = 's_Image_SRO_control')
    wSortIndexTablesOnOff = widget_button(wColocTable, value = '...||-> Sort Index Tables (off)', uValue = 'SORTINDEXTABLESONOFF',event_pro = 's_Image_SRO_control')

    state = {thisContainer:thisContainer,$  ; The container object.
             wTopBase:wTopBase,$
             stack_tlb:stack_tlb,$
             groupLeader:groupLeader,$
             pSclArr:pSclArr,$
             pClrArr:pClrArr,$
             dimI:dimI,$
             dimAlpha:dimAlpha,$
             fScale:fScale,$
             fActiveFromTo:fActiveFromTo,$
             xyFromTo:xyFromTo,$
             intScale:intScale,$
             yScale:yScale,$
             mask:mask,$
             colorBar:colorBar,$
             wDraw:wDraw,$                  ; The draw widget identifier.
             thisWindow:thisWindow,$        ; The window object.
             fMap:0,$
             wSliderArr:wSliderArr,$
             wFlexBase:wFlexBase,$
             fOpac:0b,$
             wRandomAndRadiusOnOff:wRandomAndRadiusOnOff,$
                fRandomAndRadius:0b,$
                child_RandomandRadiusTable_tlb:-1l,$
                randomValue:20b,$
                radiusValue:5,$
             wViewROIMaskOnOff:wViewROIMaskOnOff,$
                fViewROIMask:0b,$
             wViewDataTableOnOff:wViewDataTableOnOff,$
                fViewDataTable:0b,$
                child_DataTable_tlb:-1l,$
             wOneByEOnOff:wOneByEOnOff,$
                fOneByE:0b,$
             wOneByESqOnOff:wOneByESqOnOff,$
                fOneByESq:0b,$
             wSortIndexTablesOnOff:wSortIndexTablesOnOff,$
                fSortIndexTables:0b,$
             fSilent: 0b,$
             paramRowNames: iNames,$
             paramColumnNames: paramNames,$
             SROParams: fltArr(n_elements(paramNames), n_elements(iNames)),$
             drawScale:drawScale,$
             thisView:thisView}

    widget_control, wTopBase, set_uValue = state, /no_copy
    XManager, 's_Image_SRO_Window', wTopBase, Cleanup = 's_Image_SRO_cleanUp', /no_block, group_leader = groupLeader
    s_Image_SRO_upDateAlphaImage, wTopBase
    for i = 0, dimI[0]-1 do begin
       widget_control, wTopBase, get_uValue = state, /no_copy
          state.xyFromTo[i,0,*] = [0,0, 90 + ((*state.pSclArr[i]).selectS - (*state.pSclArr[i]).minMaxS[0]) / (((*state.pSclArr[i]).minMaxS[1]-(*state.pSclArr[i]).minMaxS[0])/255.), state.dimAlpha[2] - 49 + i * (state.yScale + 10) + state.yScale]
          state.fActiveFromTo = [(i-1)>0,0,i,0]
       widget_control, wTopBase, set_uValue = state, /no_copy
       s_Image_SRO_upDateBetaImage, wTopBase
    endfor
end