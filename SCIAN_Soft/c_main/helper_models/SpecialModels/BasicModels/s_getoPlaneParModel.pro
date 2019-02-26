pro s_getoPlaneParModel, oObjectModel = oObjectModel, stack_tlb = stack_tlb, oGroupReference = oGroupReference

    oGroupReference->IDLgrROIGroup::getProperty, xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv
    xyzDim = oGroupReference->getxyzDim()
    c = oGroupReference->getGroupCenterXYZ()
    xyzPixel = oGroupReference->getxyzSizePerPixel()
    zxFactor = xyzPixel[2]/xyzPixel[0]
    center=[total(c[0,*])/oGroupReference->count(),total(c[1,*])/oGroupReference->count(),total(c[2,*])/oGroupReference->count()]
    verts = fltarr(3,4)
    verts[0:2,0]=[(center[0] - max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]
    verts[0:2,1]=[(center[0] - max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts[0:2,2]=[(center[0] + max(c[0,*])/2), (center[1] + max(c[1,*])/2), (center[2])]
    verts[0:2,3]=[(center[0] + max(c[0,*])/2), (center[1] - max(c[1,*])/2), (center[2])]

    x=make_Array(3,4)
    for i=0, 3 do x[*,i]=verts[*,i]-center
    vertspr=[x,transpose([1,1,1,1])]
    ctm=[[0,0,1,0],[0,1,0,0],[-1,0,0,0],[0,0,0,1]]
    for i=0, 3 do vertspr[*,i]=vertspr[*,i]#ctm
    vertspr_new=[vertspr[0,*],vertspr[1,*],vertspr[2,*]/zxFactor]
    for i=0, 3 do vertspr_new[*,i]=vertspr_new[*,i]+center
    ctm_mesh=make_array(4,4,/double)
    ctm_plane=make_array(4,4,/double)    
    pp=make_array(4,4,/double)
    nn=make_array(4,2,/double)
    new_verts=make_array(4,4,/double)
    conn = fltarr((size(verts, /DIMENSIONS))[1]+1)
    num = (size(verts, /DIMENSIONS))[1]
    conn = [num, indgen(num)]
    color = [127,127,127]
    color2 = [255,255,255]
    
    radio=5
    
    ;Se agregan planos 1, 2 y esfera
    for i = 0, (oGroupReference->count())-1 do begin
        oObjectModel->Add, obj_new('IDLgrPolygon',ambient = [0,0,0], data = verts, poly = conn, color = color, vert_colors=127, shading = 1,$
                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                                name = strCompress('3DPlane1:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')   
        oObjectModel->Add, obj_new('IDLgrPolygon',ambient = [0,0,0], data = vertspr_new, poly = conn, color = color2, vert_colors=255, shading = 1,$
                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv, $
                                name = strCompress('3DPlane2:'+string(i+1),/rem),  uvalue = 'ObjInOriginalPosition')
        oObjectModel->add, obj_new('C_sOrb', POS=center*1.5, RADIUS=radio, xyzFact = zxFactor, color = color, $
                                xCoord_conv = xCoord_conv, yCoord_conv = yCoord_conv, zCoord_conv = zCoord_conv,$
                                name = strCompress('3D_Sphere:'+string(i+1),/rem), uvalue = 'ObjInOriginalPosition')
    endfor
end
