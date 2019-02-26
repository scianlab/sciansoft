;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_ConfinedGrowthFilter
;
; PURPOSE:
;       - imagine the universe in a nutshell, expand it to reach the edges ... and try to estimate if you are close to the edges.
;
; AUTHOR:
;     slave Felipe Andrés Santibáñez-Leal (2012)
;     e_mail: fsantibanez@med.uchile.cl
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_ConfinedGrowthFilter' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sImageFilter_ConfinedGrowthFilter::getImageFilterDevelopmentalState
    ;return, 'Release_Image_Filter_Method'
    return, 'Alpha_Debugging_Filter_Method'
    ;return, 'Beta_Release_Filter_Method'
end

function C_sImageFilter_ConfinedGrowthFilter::getImageFilterType
    return, 'Multiple_Image_Filter_Method'
end

function C_sImageFilter_ConfinedGrowthFilter::apply, image = image,$
                                     selectedStackObject = selectedStackObject,$
                                     stack_tlb = stack_tlb,$
                                     tPos = tPos,$
                                     chPos = chPos,$
                                     zPos = zPos,$
                                     clusPos = clusPos,$
                                     segPos = segPos,$
                                     cut_x = cut_x, cut_y = cut_y

    pParamStruct = selectedStackObject->getpParamStruct()
    totalTimes = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Times'))[0]]-1
    totalChannels = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Total Number of Channels'))[0]]-1

       ; decide if 1st_Self_or 2nd_Self
    fSelf = 0b
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Self_or 2nd_Self'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then fSelf = (*(*self.pParamStruct).pValues)[whParam]

       ; get first mask
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_1 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_1 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_1 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_1
    whParam = (where((*(*self.pParamStruct).pNames) eq '1st_Cluster'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_1 = round((*(*self.pParamStruct).pValues)[whParam]) else cluster_1 = clusPos

    if (fSelf eq 0) or (fSelf eq 2) then begin
       selectedStackObject->getSelectedClusterMask, mask = mask_1, tPos = time_1, chPos = channel_1, zPos = zPos, clusPos = cluster_1
       if ((n_elements(mask_1) eq 1) and (mask_1[0] eq -1)) then begin
          oImage = selectedStackObject->getSelectedImageObject(tPos = time_1, chPos = channel_1, zPos = zPos)
          mask_1 = oImage->applyImageSegmentation( selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                            tPos = time_1,$
                                                            chPos = channel_1,$
                                                            zPos = zPos,$
                                                            clusPos = cluster_1,$
                                                            cut_x = cut_x, cut_y = cut_y)
       endif else mask_1 = mask_1[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endif
    if (fSelf eq 1) then mask_1 = image

       ; get second mask
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Time'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then time_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalTimes) else time_2 = (tPos < totalTimes)
    (*(*self.pParamStruct).pValues)[whParam] = time_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Channel'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then channel_2 = (round((*(*self.pParamStruct).pValues)[whParam]) < totalChannels) else channel_2 = (chPos < totalChannels)
    (*(*self.pParamStruct).pValues)[whParam] = channel_2
    whParam = (where((*(*self.pParamStruct).pNames) eq '2nd_Cluster'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then cluster_2 = round((*(*self.pParamStruct).pValues)[whParam]) else cluster_2 = clusPos
    
    if (fSelf eq 0) or (fSelf eq 1) then begin
       selectedStackObject->getSelectedClusterMask, mask = mask_2, tPos = time_2, chPos = channel_2, zPos = zPos, clusPos = cluster_2
       if ((n_elements(mask_2) eq 1) and (mask_2[0] eq -1)) then begin
          oImage = selectedStackObject->getSelectedImageObject(tPos = time_2, chPos = channel_2, zPos = zPos)
          mask_2 = oImage->applyImageSegmentation(selectedStackObject = selectedStackObject, stack_tlb = stack_tlb,$
                                                            tPos = time_2,$
                                                            chPos = channel_2,$
                                                            zPos = zPos,$
                                                            clusPos = cluster_2,$
                                                            cut_x = cut_x, cut_y = cut_y)
       endif else mask_2 = mask_2[cut_x[0]:cut_x[1], cut_y[0]:cut_y[1]]
    endif
    if (fSelf eq 2) then mask_2 = image

    whParam = (where((*(*self.pParamStruct).pNames) eq 'angular_Step'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then angularStep = (*(*self.pParamStruct).pValues)[whParam]
    if (angularStep lt (*(*self.pParamStruct).pMin)[whParam]) then angularStep = (*(*self.pParamStruct).pMin)[whParam]
    if (angularStep gt (*(*self.pParamStruct).pMax)[whParam]) then angularStep = (*(*self.pParamStruct).pMax)[whParam]    
    (*(*self.pParamStruct).pValues)[whParam] = angularStep

      ; Apply crazy operation ....
    whParam = (where((*(*self.pParamStruct).pNames) eq '0->ConfinedGrowth_AreaRate'))[0]
    if ( (*(*self.pParamStruct).pActive)[whParam]) then operator = round((*(*self.pParamStruct).pValues)[whParam]) else operator = 0
    operator = 0 ; We have only One XD

    case logOperator of
       0: begin
       ; create structure for area estimation
       numSteps = floor(360 / angularStep);
       distanceContour = MAKE_ARRAY(numSteps,/DOUBLE, VALUE= 0)
       ; mask_1 are centers ... Binary
       ; mask_2 are contours ... Binary
          dimI = size(mask_1, /dim)
          width = dimI(0)
          height = dimI(1)
        
            ; create the ROI object to get the masks, images, ...
          oROI2DGroup = obj_new('C_sROIGroupObj', xyzPixSize = [width, height, 1], ZSliceInterval = 1)
          oROI2DGroup->addROIObjectsFromSegImage, maskImage = mask_1, intImage = mask_1, objNumberVector = objNumberVector
          counter = oROI2DGroup->getObjectNumberVector()
                 
          imageSeg = image * 0.0d
             for i=0, n_elements(counter)-1 do begin
                contourArea = contourArea * 0
                Obj = ((oROI2DGroup->get(position = i))->getxyzPoints())[0:1,*]

                ; find area
                areaRoiMask_1 = N_ELEMENTS(Obj[0,*]) * 1.0d
            
                ;get center?
                ;center = [floor(total(Obj[0,*])/ areaRoiMask_1),floor(total(Obj[1,*])/ areaRoiMask_1)] 
                center = ((oROI2DGroup->get(position = i))->getCenterXYZ())[0:1]

                imageRot = image * 0
                imageRot[center[0],0:center[1]] = 1 
                ; Estimate Distance Mask_2 closed at ROI ;ask_1 in direction unitVector 
                for actualStep = 0, numSteps-1 do begin
                  ; Estimate local x,y distance (position in relative coord system)
                  angleRadian = angularStep * actualStep * !PI / 180.0d 
                  unitVector  = [cos(angleRadian),sin(angleRadian)]
                  ; try to find in mask_2 the nearet component 
                  ; rotate RefImage using selected angle
                    ; Rotate by theta, obtain the center, interpolate
                    kernel = rot(imageRot, angleRadian, 1.0, center[0], center[1], /INTERP)

                    distanceContour[actualStep] = 0
                endfor
                ; verify lost points
                
                
                ; set points
                for actualStep = 0, numSteps-1 do begin
                  if(total(abs(contourArea[actualStep,*])) gt 0) then begin
                    angleRadian = angularStep * actualStep * !PI / 180.0d 
                    unitVector  = [cos(angleRadian),sin(angleRadian)] 
                    if(actualStep eq 0) then begin
                      contourArea[actualStep,*] = unitVector * distanceContour[actualStep]
                    endif
                    
                  endif
                endfor

                
                ; Assign estimation 
                whereROI = (oROI2DGroup->get(position = i))->getpWherePoints()
                areaRoiMask_2 = 1000.0d
                
                imageSeg[whereROI] = areaRoiMask_1 / areaRoiMask_2
             endfor
          return, imageSeg
       
;          labelMask_1 = label_region(mask_1)
;          maxLM = max(labelMask_1)
;          image = image * 0.0d
;          if (maxLM ge 1) then begin
;             for i = 1, maxLM do begin
;                whereObj = where(labelMask_1 eq i)
;                ;find center
;                mask_1[whereObj]
;             endfor
;             return, image
;          endif else return, mask_1
       endcase
       else: return, mask_1
    endcase
end


function C_sImageFilter_ConfinedGrowthFilter::init
    ImageFilterStruct = {Name: 'C_LogicalFilter',$   ;  Filter Name.
                           pWidgetType: ptr_new(),$     ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$     ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$    ; Pointer on Filter Parameter Max_Values.
                           pValues: ptr_new()}         ; Pointer on Filter Parameter Values.

    filterParamWidgetType = make_array(10, /string, value = 'widget_slider')
    filterParamNames = ['1st_Time',$
                        '1st_Channel',$
                        '1st_Cluster',$
                        '2nd_Time',$
                        '2nd_Channel',$
                        '2nd_Cluster',$
                        '1st_Self_or 2nd_Self',$
                        'angular_Step',$
                        '0->KeepValues||1->MaxValue||2->MeanValue||3->MedianValue',$
                        '0->ConfinedGrowth_AreaRate']
    filterParamActive = [1, 1, 1, 1, 1, 1, 0,1, 1,1]
    filterParamMin = [0, 0, 0, 0, 0, 0, 1,1,0, 0]
    filterParamMax = [1000, 1000, 1000, 1000, 1000, 1000, 2,120, 10,8]
    filterParamValues = [0, 0, 0, 0, 0, 0, 1, 30,0,0]

    ImageFilterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    ImageFilterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    ImageFilterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    ImageFilterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    ImageFilterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    ImageFilterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(ImageFilterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_ConfinedGrowthFilter__define
  tmp = {C_sImageFilter_ConfinedGrowthFilter, pParamStruct: ptr_new(), inherits C_sImageFilter}
end