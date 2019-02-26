;plane model
pro s_getoPlaneModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference

    oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzDim = oGroupReference->getxyzDim()
    c = oGroupReference->getGroupCenterXYZ()
    center=[total(c[0,*])/oGroupReference->count(),total(c[1,*])/oGroupReference->count(),total(c[2,*])/oGroupReference->count()]
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
    
    for i = 0, (oGroupReference->count())-1 do begin
        oObjectModel->Add, obj_new('IDLgrPolygon',data = verts, poly = conn, color = color,$
                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                                name = strCompress('3DPlaneModel_Base:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
        
        oObjectModel->Add, OBJ_NEW('IDLgrPolyline', [n[0,0],n[0,1]], [n[1,0],n[1,1]], [n[2,0],n[2,1]], color = color, thick=2, $
                              xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                              name = strCompress('3DNormalPlane_Base:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
    endfor
end
