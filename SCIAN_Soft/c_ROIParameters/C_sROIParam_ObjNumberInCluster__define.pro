;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_ObjNumberInCluster
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_ObjNumberInCluster' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

pro C_sROIParam_ObjNumberInCluster::apply, whereXY = whereXY, objNumber = objNumber, single = single, mask = mask, group = group, position = position, xySizePerPixel = xySizePerPixel

    if ptr_valid((*self.pParamStruct).pROINumberVect) then ptr_free, (*self.pParamStruct).pROINumberVect

    whParam = [  (where( *(*self.pParamStruct).pNames eq 'Number of Cells in Cluster [pixel]'))[0] ,$
                    (where( *(*self.pParamStruct).pNames eq 'Number of Cells in Cluster [µm²]'))[0] ]

    whParamActive = whParam * 0
    if (n_elements(position) gt 0) then begin
       if (position[0] ne -1) then for i = 0, n_elements(position)-1 do whParamActive[position[i]] = 1
    endif else for i = 0, n_elements(whParam)-1 do whParamActive[i] = (*(*self.pParamStruct).pActive)[whParam[i]]

    if whParamActive[0] then begin
       if ptr_valid((*(*self.pValueStruct)[whParam[0]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[0]]).pROIParamVect
       basicPixCellSize = 1. * (*(*(*self.pValueStruct)[whParam[0]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[0]]).pNames eq 'Size of Basic Cell [pixel]'))[0]]
       if (basicPixCellSize eq 0) then basicPixCellSize = 1.
    endif
    if whParamActive[1] then begin
       if ptr_valid((*(*self.pValueStruct)[whParam[1]]).pROIParamVect) then ptr_free, (*(*self.pValueStruct)[whParam[1]]).pROIParamVect
       (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'x-Size per Pixel'))[0]] = xySizePerPixel[0]
       (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'y-Size per Pixel'))[0]] = xySizePerPixel[1]
       basicRealCellSize = (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'Size of Basic Cell [µm²]'))[0]]
       factor = 1.*xySizePerPixel[0]*xySizePerPixel[1]
       if (basicRealCellSize eq 0) then basicRealCellSize = 1.
       if (factor eq 0) then factor = 1.
       basicRealCellSize = temporary(basicRealCellSize) / factor
    endif

    if keyword_set(single) then begin
       (*self.pParamStruct).pROINumberVect = ptr_new( objNumber, /no_copy)
       if whParamActive[0] then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( round(n_elements(whereXY)/basicPixCellSize) > 1, /no_copy)
       if whParamActive[1] then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( round(n_elements(whereXY)*(factor/basicRealCellSize)) > 1, /no_copy)
    endif

    if keyword_set(group) then begin
       hmax = max(mask)
       if (hmax eq 0) then begin
         (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
         if whParamActive[0] then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( -1, /no_copy)
         if whParamActive[1] then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( -1, /no_copy)
       endif else begin
         hmin = min(mask[where(mask ne 0)])
         hist = histogram(mask, min = hmin, max = hmax)
         whereHist = where(hist ne 0)
         (*self.pParamStruct).pROINumberVect = ptr_new( (whereHist + hmin))
         if whParamActive[0] then (*(*self.pValueStruct)[whParam[0]]).pROIParamVect = ptr_new( round(hist[whereHist]/basicPixCellSize) > 1, /no_copy)
         if whParamActive[1] then (*(*self.pValueStruct)[whParam[1]]).pROIParamVect = ptr_new( round(hist[whereHist]*(factor/basicRealCellSize)) > 1, /no_copy)
       endelse
    endif
end


function C_sROIParam_ObjNumberInCluster::init

    ROIParamStruct = {name:'Object Number in Cluster',$     ;  ROI Name.
                    type:   'Single ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$    ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$      ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$         ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$     ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect: ptr_new()   }     ; Pointer on ROI-Obj Number Vector

    self.pValueStruct = ptr_new(ptrArr(2))
    ROIParamWidgetType = make_array(2, /string, value = 'widget_slider')
    ROIParamNames = [ 'Number of Cells in Cluster [pixel]', 'Number of Cells in Cluster [µm²]']
    ROIParamActive = [1,0]
    ROIParamMin = [0,0]
    ROIParamMax = [0,0]
    ROIParamValues = [0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name: 'Number of Cells in Cluster [pixel]',$
                        type: 'Single ROI-Parameter-Method',$
                        pWidgetType:ptr_new(),$   ; Pointer on ROI-Obj Parameter WidgetType.
                        pNames:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                        pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                        pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                        pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                        pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                        pROIParamVect: ptr_new()  }       ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(9, /string, value = 'widget_slider')
    ROIValueNames = ['Size of Basic Cell [pixel]',$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueActive = make_array(9, /byte, value = 0)
    ROIValueMin = make_array(9, /float, value = 0)
    ROIValueMax = make_array(9, /float, value = 1)
    ROIValueMax[0] = 1000
    ROIValueValues =[ 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)


    ROIValueStruct = {name: 'Number of Cells in Cluster [µ]',$
                        type: 'Single ROI-Parameter-Method',$
                        pWidgetType:ptr_new(),$   ; Pointer on ROI-Obj Parameter WidgetType.
                        pNames:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                        pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                        pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                        pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                        pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                        pROIParamVect: ptr_new()  }       ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(11, /string, value = 'widget_slider')
    ROIValueNames = ['Size of Basic Cell [µm²]',$
                        'x-Size per Pixel', 'y-Size per Pixel',$
                        'Threshold_1a', 'Threshold_1b',$
                        'Threshold_2a', 'Threshold_2b',$
                        'Threshold_3a', 'Threshold_3b',$
                        'Threshold_4a', 'Threshold_4b']
    ROIValueActive = make_array(11, /byte, value = 0)
    ROIValueMin = make_array(11, /float, value = 0)
    ROIValueMax = make_array(11, /float, value = 1)
    ROIValueMax[0:3] = 1000
    ROIValueValues =[1., 1., 1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1.,$
                        0.,1. ]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType, /no_copy)
    ROIValueStruct.pNames = ptr_new(ROIValueNames, /no_copy)
    ROIValueStruct.pActive = ptr_new(ROIValueActive, /no_copy)
    ROIValueStruct.pMin = ptr_new(ROIValueMin, /no_copy)
    ROIValueStruct.pMax = ptr_new(ROIValueMax, /no_copy)
    ROIValueStruct.pValues = ptr_new(ROIValueValues, /no_copy)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect, /no_copy)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)

    return, 1
end

pro C_sROIParam_ObjNumberInCluster__define
   tmp = {C_sROIParam_ObjNumberInCluster, pParamStruct: ptr_new(),$
                                  pValueStruct: ptr_new(),$
                                  inherits C_sROIParam }
end