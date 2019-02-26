;_____________________________IOISIOI____________________
; FUNCTIONNAME:
;       s_create_ROI_objects
;
; PURPOSE:
;       Find minimum distances between all numerbered objects in NumberedObjectImage
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       s_min_object_distance, NumberedObjectImage
;           NumberedObjectImage: A 2D array of object data | Objects must be numbered consecutively
;
; REQUIRED INPUTS:
;       None. -1 is returned if NumberedObjectImage does not fullfill requirements
;
; OPTIONAL KEYWORD PARAMETERS:
;       _EXTRA:
;_____________________________IOISIOI____________________


; FASL 2013...  DEPRECATED FUNCTION?!?!?!??!
function s_create_ROI_objects, image,$                ; image:   2D/3D - Data Set with numbered or binary Regions
                   TwoPhases = TwoPhases,$            ; TwoPhases:  Regions & Holes will be considered
                   pROIObjectList = pROIObjectList,$ ; pROIObjectList: Poniter on image Object List
                   EightNeighbor = EightNeighbor         ; EightNeighbor: If set, 8-neighbor is definition in label_region. Default: 4-neighbor

    if (n_elements(image) eq 0) then begin
       s_apop_shout, 'No image-data passed'
       Return, -1
    endif

    if (max(image) eq 0) then begin
       a = s_apop_shout( 'No object segmented in image')
       Return, -1
    endif

;________Label_Regions in 2/3D image | Border Pixels are not set to 0 (see label_region)
    dim_Image = size(image, /Dimension)
    if (n_elements(dim_Image) eq 2) then begin
       dummy = bytArr(dim_Image[0]+2, dim_Image[1]+2)
       dummy[1:dim_Image[0], 1:dim_Image[1]] = image
       if (keyWord_set(EightNeighbor)) then Label_Image = (label_region(dummy GT 0, /All))[1:dim_Image[0], 1:dim_Image[1]] $
          else Label_Image = (label_region(dummy GT 0))[1:dim_Image[0], 1:dim_Image[1]]
    endif
    if (n_elements(dim_Image) eq 3) then begin
       dummy = bytArr(dim_Image[0]+2, dim_Image[1]+2, dim_Image[2]+2)
       dummy[1:dim_ori[0], 1:dim_ori[1], 1:dim_ori[2]] = image
       if (keyWord_set(EightNeighbor)) then Label_Image = (label_region(dummy GT 0, /All))[1:dim_Image[0], 1:dim_Image[1], 1:dim_ori[2]] $
          else Label_Image = (label_region(dummy GT 0))[1:dim_Image[0], 1:dim_Image[1], 1:dim_ori[2]]
    endif
;________END_Label_Regions in 2/3D image

;________Build ROIGroupObj
    ; DEPRECATED!?!?!?
    ;oROIGroup = obj_new('C_sROIGroupObj', framePixSize = size(Label_Image, /dim))
    oROIGroup = obj_new('C_sROIGroupObj', xyzPixSize = [size(Label_Image, /dim),1])


    pParamStruct = oROIGroup->getpParamStruct()
    xSizePerPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox xSize [real]'))[0]] * 1. / $
                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox xSize [pixel]'))[0]]
    ySizePerPixel = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox ySize [real]'))[0]] * 1. / $
                   *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'ROI-Group ShowBox ySize [pixel]'))[0]]
    for i = 1, max(Label_Image) do begin
       number = i
       ; Old format... trying to solve .. :_(
       dummy = obj_new('C_sROIObject', number = number-1,$
                                        framePixSize = [size(Label_Image, /Dim),1],$
                                        xyzSizePerPixel = [xSizePerPixel,ySizePerPixel,1],$
                                        wherePoints = where(Label_Image eq number),$
                                        pointValues = number )

       dummy->IDLgrROI::setProperty, color = [255, (i*5) MOD 255, 0]
        oROIGroup->add, dummy;, position = (i-1)
    endfor

    ;________END_Build Data Object
;*(pROIObjectList[0]) = [ *(pROIObjectList[0]), dummy]
;*(pROIGroupObjList[0]) = [*(pROIGroupObjList[0]), oROIGroup]
;________END_Build ROIGroupObjs

;________Calculate Group Parameters
    oROIGroup->CalculateGroupParameters ;, ParameterList = ['Object_Border_Distances','Object_Center_Distances']

;________Initialize_PhaseModelObject
    oPhaseModel = obj_new('IDLgrModel', uValue = 'oPhaseModel')

       ; Add SubPhase
    oSubPhase = s_objworldMakeObj('oSubPhase', FramePixSize = size(Label_Image, /Dim) )
    oPhaseModel->add, oSubPhase

       ; Add Group Model
    oROIGroupModel = obj_new('IDLgrModel', uValue = 'GroupModel')
    oROIGroupModel->add, oROIGroup
    oROIGroupModel->translate, 0, 0, .025, /premultiply
    oPhaseModel->add, oROIGroupModel

       ; Add ObjectBorderPolygons
    pz = oROIGroup->getpoBorderModel()
    oPhaseModel->add, *pz
    ptr_free, pz
       ; Add BorderDistancePolygon
    pz = oROIGroup->getpoCenterDistanceModel(NumberNearestNeighbour = 6)
    oPhaseModel->add, *pz
    ptr_free, pz
       ; Add BorderDistancePolygon
    pz = oROIGroup->getpBorderDistanceModel(NumberNearestNeighbour = 6)
    *pz->translate, 0, 0, .0125
    oPhaseModel->add, *pz
    ptr_free, pz

    oPhaseModel->scale, 2.,2.,2., /premultiply

    CALL_PROCEDURE, 's_objworld', init_oModel = oPhaseModel, application_TLB = application_TLB

;   s_objworldEvent, {Add_grROIObjects,$
;                    TOP: application_TLB,$
;                    uval: ['ADD','grROIObject'],$
;                    oModel : oPhaseModel $
;                    }

                    widget_control, application_TLB, get_uvalue = state, /no_copy
;                   Obj->[IDLanROI::]Rotate, Axis, Angle [, CENTER = [x, y[, z]]]
              for i = 0,20 do begin
;                     dummy->Rotate, [0,0,1], i/!radeg, Center = [0,0,0]
;                     dummypoly->Rotate, [0,0,1], i/!radeg, Center = [0,0,0]
;                     demo_draw, state.win, state.oScene, debug = state.debug
                    endfor
                    widget_control, application_TLB, set_uvalue = state, /no_copy
;________End_Initialize_s_objworld

;function C_sROIGroupObj::getGroupMask, pMask = pMask


    return, 1
end
