
pro s_getoUnDriftedPlaneOrthogonalModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference
    s_ISM_getProjectInfo, stack_tlb = stack_tlb, tPos = tPos, chPos = chPos,clusPos = clusPos
    tPos = 0
    
    oInitReference = s_ISegM_GetROI3DGroup(stack_tlb = stack_tlb,tPos = 0, chPos = chPos, clusPos = clusPos)

    oInitReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    
    xyzDim = oInitReference->getxyzDim()
    c = oInitReference->getGroupCenterXYZ()
    center=[total(c[0,*])/oInitReference->count(),total(c[1,*])/oInitReference->count(),total(c[2,*])/oInitReference->count()]
    verts = fltarr(3,4)
    verts[0:2,0]=[(center[0] - max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]
    verts[0:2,1]=[(center[0] - max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts[0:2,2]=[(center[0] + max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts[0:2,3]=[(center[0] + max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]
    ctm_mesh=make_array(4,4,/double)
    ctm_plane=make_array(4,4,/double)
    pp=make_array(4,4,/double)
    nn=make_array(4,2,/double)
    new_verts=make_array(4,4,/double)
    conn = fltarr((size(verts, /DIMENSIONS))[1]+1)
    num = (size(verts, /DIMENSIONS))[1]
    conn = [num, indgen(num)]
    color = [127,127,127]
    
    P=center+[0, 0, (center[2] - center[2]/1.15)]
    n=[[center],[P]]



; Extract transformation matrixs for currents planes
      ctm_plane=make_array(4,4,/double, value = 0.0d)
      ctm_planeComplementary=make_array(4,4,/double, value = 0.0d)
      ctm_orthogonalplane=make_array(4,4,/double, value = 0.0d)    
      ctm_plane[0,0] = 1.0d
      ctm_plane[1,1] = 1.0d
      ctm_plane[2,2] = 1.0d
      ctm_plane[3,3] = 1.0d
      ctm_planeComplementary[0,0] = 1.0d
      ctm_planeComplementary[1,1] = 1.0d
      ctm_planeComplementary[2,2] = 1.0d
      ctm_planeComplementary[3,3] = 1.0d
      ctm_orthogonalplane[0,0] = 1.0d
      ctm_orthogonalplane[1,1] = 1.0d
      ctm_orthogonalplane[2,2] = 1.0d
      ctm_orthogonalplane[3,3] = 1.0d

      widget_control, stack_tlb, get_uValue = stackState, /no_copy
       (*stackState.pImageStackInfoObject)->get, pParamStruct = pParamStruct
      widget_control, stack_tlb, set_uValue = stackState, /no_copy
  
      path = *(*pParamStruct).pValues[(where( *(*pParamStruct).pNames eq 'Stack Path'))[0]]
      name = strCompress('GraphicState_Time_' + strCompress(tPos) + '.sav')
      filename = strCompress(path + strcompress(name, /rem))
      flag = FILE_TEST(filename)
      line = make_Array(4,/double)
      i=0
      if ~flag then Result = DIALOG_MESSAGE('File ' + name + ' Not Found')
      if flag then begin
          GET_LUN, inunit
          openr, inunit, filename
          while ~ EOF(inunit) do begin
             READF, inunit, line 
             if( i le 3) then ctm_mesh[*,i]=line
             if((i gt 3) and (i le 7) ) then ctm_plane[*,i-4]=line
             if((i gt 7) and (i le 11) ) then ctm_orthogonalplane[*,i-8]=line
             if((i gt 11) ) then ctm_planeComplementary[*,i-12]=line
             i++ 
          endwhile    
          FREE_LUN, inunit
      endif    
    
    for i = 0, (oInitReference->count())-1 do begin
        oObjectModel->Add, obj_new('IDLgrPolygon',data = verts, poly = conn, color = color,$
                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                                name = strCompress('3D_UnDrifted_PlaneModel_Orthogonal:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
        
        oObjectModel->Add, OBJ_NEW('IDLgrPolyline', [n[0,0],n[0,1]], [n[1,0],n[1,1]], [n[2,0],n[2,1]], color = color, thick=2, $
                              xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                              name = strCompress('3D_UnDrifted_NormalPlane_Orthogonal:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
    endfor

    matrixMagix = s_Driftmatrix( vDraworData = 0b, vstack_tlb = stack_tlb, oGroupReference = oInitReference, actual3DGroupObject = oInitReference, actualTime = tPos, chPos = chPos, clusPos = clusPos)
    ;matrixMagix[3,0:2] = [2*xCoord_conv[1],2*yCoord_conv[1],2*zCoord_conv[1]]
    oObjectModel->setproperty, TRANSFORM = ctm_orthogonalplane # matrixMagix        
end
