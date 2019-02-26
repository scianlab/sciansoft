;_____________________________IOISIOI____________________
; NAME:
;      C_sROIParam_3DObjOpticalFlow
;
; PURPOSE:
;       - Calculation of Object Size in Pixels
;
; AUTHOR:
;     Dr. Steffen Härtel (2001)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       result = obj_new('C_sROIParam_3DObjOpticalFlow' )
;
; METHOHDS:
;_____________________________IOISIOI____________________


pro C_sROIParam_3DObjOpticalFlow::apply, apply, stack_tlb = stack_tlb, C_sROI3DGroupObj = C_sROI3DGroupObj, position = position

    ;['3D Speed [Voxel/Time]','3D Angle phi [deg]' ,'3D Angle theta [deg]']
    whParam = [   (where( *(*self.pParamStruct).pNames eq '3D Speed [Voxel/Time]'))[0],$
                  (where( *(*self.pParamStruct).pNames eq '3D Angle phi [deg]'))[0],$
                  (where( *(*self.pParamStruct).pNames eq '3D Angle theta [deg]'))[0] ]

    ; check Active Parameter
    whParamActive = whParam * 0
    case (n_elements(position) gt 0) of
       1:if (position[0] eq -1) then return else  whParamActive[position] = 1
       else:whParamActive = (*(*self.pParamStruct).pActive)[whParam]
    endcase
    wherePA = where(whParamActive eq 1)

    ; check Pointers
    if ~ptr_valid((*self.pParamStruct).pROINumberVect) then (*self.pParamStruct).pROINumberVect = ptr_new( -1, /no_copy)
    if (wherePA[0] eq -1) then return
    
    for i = 0, n_elements(wherePA)-1 do $
      if ~ptr_valid((*(*self.pValueStruct)[wherePA[i]]).pROIParamVect) then $
        (*(*self.pValueStruct)[wherePA[i]]).pROIParamVect = ptr_new( -1, /no_copy)

    nObjects = C_sROI3DGroupObj->count()
    if (nObjects lt 1) then begin
      *(*self.pParamStruct).pROINumberVect = -1
        
      for i = 0, n_elements(wherePA)-1 do *(*(*self.pValueStruct)[whParam[wherePA[i]]]).pROIParamVect = -1
        
    endif else begin
    
      ;there is more than 1 object and a selected sub-roi parameter
      ;compute OF at each voxel
      self->CallOpticalFlow3D, stack_tlb, C_sROI3DGroupObj, u, v, w
      
      ; set Object Number Vector
      *(*self.pParamStruct).pROINumberVect = C_sROI3DGroupObj->getObjectNumberVector()
      
      for i = 0, nObjects-1 do begin
        print, 'Computing ROI Optical flow'
        indVol =*((C_sROI3DGroupObj->get(position = i))->getpWherePoints())
        vx=mean(u(indVol))
        vy=mean(v(indVol))
        vz=mean(w(indVol))
        
        mvx=max(u(indVol))
        mvy=max(v(indVol))
        mvz=max(w(indVol))
        
        mag=sqrt(vx*vx+vy*vy+vz*vz)
        if vx ne 0 then begin
          theta=180.0*atan(vy/vx)/!pi
        endif else begin
          theta=2*!pi
        endelse
        
        if mag ne 0 then begin
          phi=180.0*acos(vz/mag)/!pi
        endif else begin
          phi=2*!pi
        endelse
        
        print, 'Mean OF-ROI (vx vy vz)=(', vx, vy, vz, ' )'
        print, 'Max OF-ROI (vx vy vz)=(', mvx, mvy, mvz, ' )'
        print, 'Mean OF-ROI (r theta phi)=(', mag, theta, phi, ' )'
        
      end
      
      print, 'ROI OF Computed'
      
      ; Object-Loop for the determination of object size [Voxel]
      paramVect = fltArr(nObjects)
      for i = 0, nObjects-1 do paramVect[i] = n_elements( *((C_sROI3DGroupObj->get(position = i))->getpWherePoints()) )
  
      ; 3D Volume [Voxel]
      if whParamActive[0] then *(*(*self.pValueStruct)[whParam[0]]).pROIParamVect = paramVect
  
      ; 3D Volume [x³]
      if whParamActive[1] then begin
        pParamStruct = C_sROI3DGroupObj->getpParamStruct()
        xSizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'x-Size [pixel]'))[0]]
        ySizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'y-Size [pixel]'))[0]]
        zSizePerPixel = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [real]'))[0]] * 1. / *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'z-Size [pixel]'))[0]]
        (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'x-Size per Pixel'))[0]] = xSizePerPixel
        (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'y-Size per Pixel'))[0]] = ySizePerPixel
        (*(*(*self.pValueStruct)[whParam[1]]).pValues)[(where( *(*(*self.pValueStruct)[whParam[1]]).pNames eq 'z-Size per Pixel'))[0]] = zSizePerPixel
    
        *(*(*self.pValueStruct)[whParam[1]]).pROIParamVect = paramVect * (xSizePerPixel*ySizePerPixel*zSizePerPixel)
        *(*(*self.pValueStruct)[whParam[2]]).pROIParamVect = paramVect * (xSizePerPixel*ySizePerPixel*zSizePerPixel)
           
       endif
    endelse
end


function C_sROIParam_3DObjOpticalFlow::init

    ROIParamStruct = {name:'3D Object Optical Flow',$   ;  ROI Name.
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter Names.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROINumberVect:ptr_new()}     ; Pointer on ROI-Obj Number Vector

    ROIParamNames = ['3D Speed [Voxel/Time]','3D Angle phi [deg]' ,'3D Angle theta [deg]']
    self.pValueStruct = ptr_new(ptrArr(n_elements(ROIParamNames)))
    ROIParamWidgetType = make_array(n_elements(ROIParamNames), /string, value = 'widget_slider')
    ROIParamActive = [1,1,1]
    ROIParamMin = [0,0,0]
    ROIParamMax = [1,1,1]
    ROIParamValues = [0,0,0]
    pROINumberVect = [-1]

    ROIParamStruct.pWidgetType = ptr_new(ROIParamWidgetType, /no_copy)
    ROIParamStruct.pNames = ptr_new(ROIParamNames, /no_copy)
    ROIParamStruct.pActive = ptr_new(ROIParamActive, /no_copy)
    ROIParamStruct.pMin = ptr_new(ROIParamMin, /no_copy)
    ROIParamStruct.pMax = ptr_new(ROIParamMax, /no_copy)
    ROIParamStruct.pValues = ptr_new(ROIParamValues, /no_copy)
    ROIParamStruct.pROINumberVect = ptr_new(pROINumberVect, /no_copy)

    self.pParamStruct = ptr_new(ROIParamStruct, /no_copy)

    ROIValueStruct = {name:'3D Speed [Voxel/Time]',$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b','Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b','Threshold_4a', 'Threshold_4b']

    ROIValueActive = make_array(8, /int, value = 0)
    ROIValueMin = make_array(8, /float, value = 0.)
    ROIValueMax = make_array(8, /float, value = 1.)
    ROIValueValues = [0.,1.,0.,1.,0.,1.,0.,1.]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    (*self.pValueStruct)[0] = ptr_new(ROIValueStruct, /no_copy)

    ROIValueStruct = {name:'3D Angle phi [deg]',$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()}     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = ['widget_slider', 'widget_slider', 'widget_slider', ROIValueWidgetType]
    ROIValueNames = ['x-Size per Pixel', 'y-Size per Pixel', 'z-Size per Pixel',ROIValueNames]
    ROIValueActive = [1, 1, 1, ROIValueActive]
    ROIValueMin = [0., 0., 0., ROIValueMin]
    ROIValueMax = [1000., 1000., 1000., ROIValueMax]
    ROIValueValues = [1., 1., 1., ROIValueValues]

    pROIParamVect = [-1]
    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)

    (*self.pValueStruct)[1] = ptr_new(ROIValueStruct, /no_copy)
    
    ROIValueStruct = {name:'3D Angle theta [deg]',$
                    type:'3D ROI-Parameter-Method',$
                    pWidgetType:ptr_new(),$     ; Pointer on ROI-Obj Parameter WidgetType.
                    pNames:ptr_new(),$       ; Pointer on ROI-Obj Parameter Names.
                    pActive:ptr_new(),$      ; Pointer on ROI-Obj Parameter Active Bool.
                    pMin:ptr_new(),$     ; Pointer on ROI-Obj Parameter Min_Values.
                    pMax:ptr_new(),$       ; Pointer on ROI-Obj Parameter Max_Values.
                    pValues:ptr_new(),$       ; Pointer on ROI-Obj Parameter Values.
                    pROIParamVect:ptr_new()  }     ; Pointer on ROI-Obj Parameter Vector.

    ROIValueWidgetType = make_array(8, /string, value = 'widget_slider')
    ROIValueNames = ['Threshold_1a', 'Threshold_1b','Threshold_2a', 'Threshold_2b',$
                     'Threshold_3a', 'Threshold_3b','Threshold_4a', 'Threshold_4b']

    ROIValueActive = make_array(8, /int, value = 0)
    ROIValueMin = make_array(8, /float, value = 0.)
    ROIValueMax = make_array(8, /float, value = 1.)
    ROIValueValues = [0.,1.,0.,1.,0.,1.,0.,1.]
    pROIParamVect = [-1]

    ROIValueStruct.pWidgetType = ptr_new(ROIValueWidgetType)
    ROIValueStruct.pNames = ptr_new(ROIValueNames)
    ROIValueStruct.pActive = ptr_new(ROIValueActive)
    ROIValueStruct.pMin = ptr_new(ROIValueMin)
    ROIValueStruct.pMax = ptr_new(ROIValueMax)
    ROIValueStruct.pValues = ptr_new(ROIValueValues)
    ROIValueStruct.pROIParamVect = ptr_new(pROIParamVect)
    
    (*self.pValueStruct)[2] = ptr_new(ROIValueStruct, /no_copy)
    
    
    return, 1
end

pro C_sROIParam_3DObjOpticalFlow::CallOpticalFlow3D, stack_tlb, C_sROI3DGroupObj, u, v, w
  ; get 3d image, compute of, move ROI...
  widget_control, stack_tlb, get_uValue = stackState, /no_copy
  imageStackInfoObject = *stackState.pImageStackInfoObject
  widget_control, stack_tlb, set_uValue = stackState, /no_copy
  
  xyzDim = C_sROI3DGroupObj->getxyzDim()
  pParamStruct = C_sROI3DGroupObj->getpParamStruct()
  
  clusPos = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Cluster Position'))[0]]
  chPos   = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Channel Position'))[0]]
  tPos    = *((*pParamStruct).pValues)[(where(*((*pParamStruct).pNames) eq 'Time Position'))[0]]

  ;compute optical flow
  volData1 = make_array(xyzDim, /byte)
  for z = 0, xyzDim[2]-1 do $
    volData1[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos, chPos = chPos, zPos = z)
  
  volData2 = make_array(xyzDim, /byte)
  for z = 0, xyzDim[2]-1 do $
    volData2[*,*,z] = imageStackInfoObject->getSelectedImage(tPos = tPos+1, chPos = chPos, zPos = z)

  print, 'OF 3D starting...'
  opticalFlowHS3D, volData1, volData2, u, v, w, alpha = 80, iterations = 200
  print, 'OF 3D computed'
  
  
  ;compute trajectories
  
end

pro C_sROIParam_3DObjOpticalFlow__define
   tmp = {C_sROIParam_3DObjOpticalFlow, pParamStruct:ptr_new(),pValueStruct:ptr_new(),inherits C_sROIParam}
end