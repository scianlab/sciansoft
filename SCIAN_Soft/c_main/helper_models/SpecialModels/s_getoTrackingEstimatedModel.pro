; FASL get Tracking Model
pro s_getoTrackingEstimatedModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference, poCurrROIGraphicModel = poCurrROIGraphicModel
    widget_control, stack_tlb, get_uValue = stackState, /no_copy
     (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
    widget_control, stack_tlb, set_uValue = stackState, /no_copy
    path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]


   oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
   xyzDim = oGroupReference->getxyzDim()
   xyzSizePerPixel = oGroupReference->getxyzSizePerPixel()

   radio = 5
   
   whParam = (where(*(*(oGroupReference->getpParamStruct())).pNames eq 'Radio Balls Model'))[0]
   if (whParam ne -1) then begin
      radio = fix(*(*(oGroupReference->getpParamStruct())).pValues[whParam] > 1)
      *(*(oGroupReference->getpParamStruct())).pValues[whParam] = radio
   endif
   
   oColorModel = *poCurrROIGraphicModel->getByName('3D Surface AC Model')
   if not(obj_valid(oColorModel)) then oColorModel = *poCurrROIGraphicModel->getByName('3D Surface Mesh Model')
   if obj_valid(oColorModel) then fOk = 1b else fOk = 0b

   factorXYZ = xyzSizePerPixel[0]/xyzSizePerPixel[2]

   xyzSPPNorm = (*(oGroupReference->getpEigenSys())).sizePerXYZ / max((*(oGroupReference->getpEigenSys())).sizePerXYZ)
   scaleAxis = 2
   thickEV = 2.
   colEV = [0,0,255]

; Obtener centroide de todos los objetos en el tiempo
    clusPos  = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Cluster Position'))[0]]
    chPos    = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Channel Position'))[0]]
    tPos     = *(((*(oGroupReference->getpParamStruct()))).pValues)[(where(*(((*(oGroupReference->getpParamStruct()))).pNames) eq 'Time Position'))[0]]
    tPosNext = tPos + 1   
   ;pParamStruct = selectedStackObject->getpParamStruct()
   
   radio = 5

   colEV = [0.,0.,255.]

   ;For COntrol
   seleccionados = make_array(2,12,/INTEGER)
   seleccionados[0,*] = [102,110,57,152,138,141,159,111,103,101,152,172]
   seleccionados[1,*] = [18,6,34,41,21,40,7,13,22,12,46,53]
   
   ;; For Tg
   ;seleccionados = make_array(2,13,/INTEGER)
   ;seleccionados[0,*] = [78,78,144,144,192,59,127,138,96,63,128,74,111]
   ;seleccionados[1,*] = [19,28,22,23,25,22,1,1,7,9,8,13,27]

   vRadio = 0.0
   vAngleCorreccion = 0.0
   vCentroCorreccion = [0.0,0.0,0.0]
   vFlipeado = 0
   
   ; FASL REFERENCE ... aqui anotar los tres puntos para cada modelo ... :_(
    filename = strCompress(path+strcompress('_Tracking' + path_sep()+'_ParamSphere' + path_sep() + 'PointsAndAngle.txt', /rem))
    punto1 = make_Array(3,/double)
    punto2 = make_Array(3,/double)
    punto3 = make_Array(3,/double)
    if(FILE_TEST(filename)) then begin
       line = make_Array(3,/double)
       h=0
       GET_LUN, inunit
       openr, inunit, filename
          salirCiclo = 0b
          while (~ EOF(inunit) and (salirCiclo eq 0b)) do begin
             if(h le 2) then begin
                 READF, inunit, line
                 if(h eq 0) then punto1 =line
                 if(h eq 1) then punto2 =line
                 if(h eq 2) then punto3 =line
               endif else begin
                 if(h eq 3) then begin
                   READF, inunit, vAngleCorreccion
                 endif
                 if(h eq 4) then begin
                   READF, inunit, vFlipeado
                   salirCiclo = 1b
                 endif
               endelse
              h++ 
          endwhile    
       FREE_LUN, inunit
    endif       
     
   vCentroCorreccion = s_CenterCircleBy3Point(point1 = punto1, point2 = punto2, point3 = punto3, xyzDim = xyzDim) ; Modelo  
   vRadio            = sqrt(total((punto1 - vCentroCorreccion)*(punto1 - vCentroCorreccion)))   
   vAngleCorreccion  = vAngleCorreccion * (!PI/180.0) ; el primer numero es el angulo deseado
   ;vAngleCorreccion = 0 ; // Debugging  
     
   masterTracking = obj_new('C_s3DTrackingEstimator')
   limitDistance = 40
   masterTracking->MasterTrackingCreation, stack_tlb = stack_tlb,oGroupReference = oGroupReference, pParamStruct = *(oGroupReference->getpParamStruct()), limitDistance = limitDistance, vAngleCorreccion = vAngleCorreccion, vCentroCorreccion = vCentroCorreccion, vRadio = vRadio, vFlipeado = vFlipeado
   ;masterTracking->MasterTrackingCreation, vSelected = seleccionados,stack_tlb = stack_tlb, pParamStruct = *(oGroupReference->getpParamStruct()), limitDistance = limitDistance, vAngleCorreccion = vAngleCorreccion, vCentroCorreccion = vCentroCorreccion, vRadio = vRadio, vFlipeado = vFlipeado
   
   ;masterTracking->CalculatesParamsFromIndividualSelectedTrackingBase
   ;masterTracking->CalculatesParamsFromSelectedTrackingBase, vCalc = 0   
   ;masterTracking->CalculatesParamsFromSelectedTrackingBase, vCalc = 1
   ;masterTracking->CalculatesParamsFromSelectedTrackingBase, vCalc = 2

    masterTracking->CalculatesJointParamsFromAllTrackingBase, vCalc = 0   
    masterTracking->CalculatesJointParamsFromAllTrackingBase, vCalc = 1
    masterTracking->CalculatesJointParamsFromAllTrackingBase, vCalc = 2
    masterTracking->CalculatesJointParamsFromAllTrackingBase, vCalc = 3
    masterTracking->CalculatesJointParamsFromAllTrackingBase, vCalc = 4

   ;masterTracking->CalculatesParamsFromAllTrackingBase, vCalc = 0   
   ;masterTracking->CalculatesParamsFromAllTrackingBase, vCalc = 1
   ;masterTracking->CalculatesParamsFromAllTrackingBase, vCalc = 2
   
   radio = 5
   colEV = [117.,117.,117.]
   thickEV = 2.
   
   ;masterTracking->DrawFromSelectedTrackingBase, radio = radio, colEV = colEV, thickEV = thickEV
   masterTracking->DrawSelectedTrackingBase, radio = radio, colEV = colEV, thickEV = thickEV
   ;masterTracking->DrawAllNodesBase, radio = radio, colEV = colEV, thickEV = thickEV

   radio = 10
   colEV = [255.,0.,0.]
   thickEV = 2.

   masterTracking->setVision, vVision = 5
   ;masterTracking->DrawFromSelectedTrackingMovie, radio = radio, colEV = colEV, thickEV = thickEV
   
   masterTracking->DrawSelectedTrackingMovie, radio = radio, colEV = colEV, thickEV = thickEV
   
   ;masterTracking->DrawFromSelectedTrackingMovieCreative, radio = radio, colEV = colEV, thickEV = thickEV
  OBJ_DESTROY, masterTracking
  
  print, 'End estimated tracking proccess'
end
