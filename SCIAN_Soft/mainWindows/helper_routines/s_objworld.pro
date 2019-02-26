;_____________________________IOISIOI____________________
; NAME:
;       s_objworld
;
; PURPOSE:
;       Interactive Object Analysis
;
; AUTHOR:
;     Dr. Steffen Härtel (2002)
;     e_mail:shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      s_objworld, path = path,$
;               group = group,$               ; IN:(opt) group identifier
;               debug = debug,$               ; IN:(opt) debug flag
;               init_oModel = init_oModel,$                        ; IN:initialize with init_oModel
;               application_TLB = application_TLB               ; OUT:(opt) TLB of this application
;
; REQUIRED INPUTS:
;       None. -1 is returned if NumberedObjectImage does not fullfill requirements
;
;  EXTERNAL FUNCTIONS, PROCEDURES, and FILES:
;       pro trackball__define   -  Create the trackball object
;       pro IDLexModelManip__define - Define Model Manipulator
;       pro IDLexViewManip__define  - Define View Manipulator
;       pro demo_gettips        - Read the tip file and create widgets
;
;  MODIFICATION HISTORY:
;     $Id:s_objworld.pro,v 1.24 2000/06/28 20:28:02 paulcs Exp $
;       Copyright (c) 1997-2000, Research Systems, Inc. All rights reserved.
;       Unauthorized reproduction prohibited.
;       1/97, ACY   - adapted from objworld, written by R.F.
;       7/98, PCS   - changed GUI to require only the left
;                       mouse-button.  Changed code to use
;                       IDLexModelManip and IDLexViewManip.
;       6/02         -  amplification to S_Object_Analysis
;_____________________________IOISIOI____________________

function s_objworldReadPalette, n
    filename = filepath('colors1.tbl', subdir = ['resource', 'colors'])
    openr,lun,filename, /block, /get_lun
       ntables = 0b
       readu, lun, ntables
       tnum = n
       if (tnum lt 0) then tnum = 0
       if (tnum ge ntables) then tnum = ntables-1
       arr = bytArr(256)
       ctab = bytArr(3,256)
       point_lun, lun, tnum*768L + 1L
       readu, lun, arr
       ctab[0,*] = arr
       readu, lun, arr
       ctab[1,*] = arr
       readu, lun, arr
       ctab[2,*] = arr
    close,lun
    free_lun,lun
    return,ctab
end


pro s_objworldSceneAntiAlias, w, v, n, oAlias = oAlias
    widget_control, /hourglass
    case n of
        2:begin
            jitter = [  [ 0.246490, 0.249999],[-0.246490, -0.249999]  ]
            njitter = 2
            endcase
        3:begin
            jitter = [  [-0.373411, -0.250550],$
                      [ 0.256263, 0.368119],$
                      [ 0.117148, -0.117570]  ]
            njitter = 3
            endcase
        8:begin
            jitter = [  [-0.334818, 0.435331],$
                      [ 0.286438, -0.393495],$
                      [ 0.459462, 0.141540],$
                      [-0.414498, -0.192829],$
                      [-0.183790, 0.082102],$
                      [-0.079263, -0.317383],$
                      [ 0.102254, 0.299133],$
                      [ 0.164216, -0.054399]  ]
            njitter = 8
            endcase
        else:begin
            jitter = [  [-0.208147, 0.353730],$
                      [ 0.203849, -0.353780],$
                      [-0.292626, -0.149945],$
                      [ 0.296924, 0.149994] ]
            njitter = 4
            endcase
        endcase

    w->getProperty, dimension = d
    acc = fltarr(3, d[0], d[1])

    if obj_isa(v, 'IDLgrView') then begin
        nViews = 1
        oViews = objarr(1)
        oViews[0] = v
    endif else begin
        nViews = v->count()
        oViews = v->get(/all)
    endelse

    rects = fltarr(4, nViews)
    for j = 0,nViews-1 do begin
        oViews[j]->getProperty, viewplane_rect = viewplane_rect
        rects[*,j] = viewplane_rect
    endfor

    for i = 0,njitter-1 do begin
        for j = 0,nViews-1 do begin
            sc = rects[2,j] / float(d[0])
            oViews[j]->setProperty, view = [ rects[0,j] + jitter[0,i] * sc, rects[1,j] + jitter[1,i] * sc, rects[2,j], rects[3,j] ]
        endfor
        demo_draw, w, v
        img = w->read()
        img->getProperty,data = data
        acc = temporary(acc) + float(data)
        obj_destroy, img
        for j = 0, nViews-1 do oViews[j]->setProperty, viewplane_rect = rects[*,j]
    endfor

    m = obj_new('IDLgrModel')
    m->add, obj_new('IDLgrImage', (acc / float(njitter)))
    v2 = obj_new('IDLgrView', view = [0,0,d[0],d[1]], proj = 1)
    v2->add, m
    demo_draw, w, v2
    if (n_elements(oAlias) eq 0) then obj_destroy, v2 else oAlias = v2
end


function s_objworldMakeView, xdim, ydim, uval
       ;Compute viewplane rect based on aspect ratio.
    aspect = xdim / float(ydim)
    myview = [-1, -1, 2, 2] * sqrt(2)
    if (aspect > 1) then begin
        myview[0] = myview[0] - ((aspect-1.0) * myview[2])/2.0
        myview[2] = myview[2] * aspect
    endif else begin
        myview[1] = myview[1] - (((1.0/aspect)-1.0) * myview[3])/2.0
        myview[3] = myview[3] / aspect
    endelse

    ;Create view. Background
	  colorFondo = [0,0,0]
    oView = obj_new('IDLgrView', projection = 2, eye = 3, zclip = [2.,-2.], dim = [xdim, ydim],$
                                 viewplane_rect = myview, color = colorFondo, uValue = uval )
    ;Create model.
    oAtom = obj_new('IDLgrModel')
    oModel = obj_new('IDLgrModel')
    oModel->add, oAtom

    s_objworldMake4x4Mesh, oAtom
    oModel->setProperty, uValue = '4x4Mesh'

    ;Define the object tree add point.
    oAtom->add, obj_new('IDLgrModel')

    ;Add some lights.
    oModel->add, obj_new('IDLgrLight', loc = [2,2,5], type = 2, color = [255,255,255], intensity = .5 )
    oModel->add, obj_new('IDLgrLight', type = 0, intensity = .5, color = [255,255,255] )
    ;Place the model in the view.
    oView->add, oModel
    return, oView
end


pro s_objworldGetViewObjs, view, oWorldRotModel, oBasePolygonAtom, model_top
       ;Luckily, this portion of the hierarchy is fixed.
    gg = view->get()
    oWorldRotModel = gg->get(pos = 0)
    oBasePolygonAtom = oWorldRotModel->get(pos = 0)
    model_top = oWorldRotModel->get(pos = 1)
end


pro s_objworldCone, verts, conn, n
    verts = fltarr(3,n+1)
    verts[0:2,0] = [0.0, 0.0, 0.1]
    t = 0.0
    tinc = (2.*!PI)/float(n)
    for i = 1,n do begin
        verts[0:2,i] = [0.1*cos(t), 0.1*sin(t), -0.1]
        t = t + tinc
    endfor
    conn = fltarr(4*n+(n+1))
    i = 0
    conn[0] = n
    for i = 1,n do conn[i] = n-i+1
    j = n+1
    for i = 1,n do begin
        conn[j:j+3] = [3, i, 0, i +1]
        if (i eq n) then conn[j+3] = 1
        j = j + 4
    endfor
end

pro s_objworldPlane, verts, conn, n
    verts = fltarr(3,4)
    verts[0:2,0]=n*[-0.1, -0.1, 0.0]
    verts[0:2,1]=n*[-0.1, 0.1, 0.0]
    verts[0:2,2]=n*[0.1, 0.1, 0.0]
    verts[0:2,3]=n*[0.1, -0.1, 0.0]
    
    conn = fltarr((size(verts, /DIMENSIONS))[1]+1)
    num = (size(verts, /DIMENSIONS))[1]
    conn = [num, indgen(num)]
end

pro s_objworldMakeFrame, oModel, FramePixSize
    verts = 1.* $
          [[-1., -1., .01],$
           [FramePixSize[0], -1., .01],$
           [FramePixSize[0], FramePixSize[1], .01],$
           [-1., FramePixSize[1], .01]]
    verts[0,*] = verts[0,*] - Floor((2.+FramePixSize[0])/2.)
    verts[1,*] = verts[1,*] - Floor((2.+FramePixSize[1])/2.)
    xcoord_conv = [0., 1.0/(max(verts[0,*])-min(verts[0,*])) ]
    ycoord_conv = [0., 1.0/(max(verts[1,*])-min(verts[1,*])) ]
    oModel->add, obj_new('IDLgrPolygon', verts,$
                                      poly = [4, 0, 1, 2, 3],$
                                      color = [0, 0, 255],$
                                      Bottom = [0, 255, 0],$
                                      xcoord_conv = xcoord_conv,$
                                      ycoord_conv = ycoord_conv,$
                                      shading = 0)
end


pro s_objworldMakegrROIObject, oModel, PassoModel
    oModel->add, PassoModel
end

pro s_objworldMakegrPolygonModel, oModel, PassoModel
    oModel->add, PassoModel
end

pro s_objworldMake4x4Mesh, oModel
    ;Create the base plate.
    b_verts = fltarr(3,5,5)
    b_conn = lonarr(5,16)
    vert_cols = bytArr(3,25)

    j = 0
    for i = 0,15 do begin
        b_conn[0:4, i] = [4, j, j+1, j+6, j+5]
        j = j + 1
        if (j mod 5) eq 4 then  j = j + 1
     endfor

    k = 0
    for i = 0,4 do begin
        for j = 0,4 do begin
            b_verts[0:2, i, j] = [i, j, 0]
            if (k eq 1) then vert_cols[*, i+j*5] = [40,40,40] $
                else vert_cols[*, i+j*5] = [255,255,255]-40
            k = 1 - k
       endfor
    endfor

    b_verts[0,*,*] = (b_verts[0,*,*]-2)/2.0
    b_verts[1,*,*] = (b_verts[1,*,*]-2)/2.0
    oModel->add, obj_new('IDLgrPolygon', b_verts, poly = b_conn, shading = 0, vert_colors = vert_cols )
end

pro s_objworldMakeSphere, oModel
     oModel->add, obj_new('C_sOrb', color = [255,0,0], density = 10, radius = 0.1, shading = 1, select_target = 1)
end

pro s_objworldYuspe, oModel
    pos6 =[ [031012022.1, 064028055.0, 728],$   ;1
         [031012020.2, 064028053.3, 729],$    ;2
         [031012008.5, 064029004.2, 778],$    ;3
         [031012010.4, 064029007.9, 783]] ;4

    pos2 =[ [031012012.3, 064029013.3, 762],$
         [031012010.4, 064029007.9, 783],$
         [031012008.5, 064029004.2, 778],$
         [031012006.6, 064029000.4, 766],$
         [031012004.6, 064028056.6, 761],$
         [031011053.7, 064029003.1, 766],$
         [031011059.3, 064029016.6, 797],$
         [031012000.7, 064029020.2, 786]]

    pos1 =[ [031012000.7, 064029020.2, 786],$
         [031012007.1, 064029035.9, 739],$
         [031012020.0, 064030003.1, 755],$
         [031012022.5, 064030006.7, 740],$    ;13
         [031012022.1, 064030009.4, 735],$    ;14
         [031012031.3, 064029048.4, 730],$    ;15
         [031012017.8, 064029027.8, 723],$    ;38
         [031012016.0, 064029018.6, 728],$    ;44
         [031012016.1, 064029017.0, 725],$    ;43
         [031012012.3, 064029013.3, 762]] ;53

    pos1 =[ [031012016.0, 064029018.6, 728],$   ;44
         [031012016.1, 064029017.0, 725],$    ;43
         [031012017.1, 064029014.8, 720],$    ;46
         [031012019.2, 064029012.0, 730],$    ;47
         [031012029.3, 064029004.1, 723],$    ;48
         [031012022.8, 064029001.6, 723],$    ;49
         [031012012.3, 064029013.3, 762]] ;53

    pos7 =[ [031012022.8, 064029001.6, 723],$   ;49
           [031012021.1, 064028059.7, 726],$ ;50
         [031012024.8, 064028056.7, 723],$    ;51
         [031012022.1, 064028055.0, 728],$    ;1
         [031012010.4, 064029007.9, 783],$    ;4
         [031012012.3, 064029013.3, 762]] ;53

     oModel->add, obj_new('IDLgrPolygon', verts, poly = conn, color = [0,255,0], shading = 0)
end

pro s_objworldMakeCube, oModel
    verts = [[-0.1,-0.1,-0.1],[0.1,-0.1,-0.1],[0.1,0.1,-0.1], [-0.1,0.1,-0.1],[-0.1,-0.1, 0.1],[0.1,-0.1, 0.1],[0.1,0.1, 0.1], [-0.1,0.1, 0.1]]

     conn = [[4,3,2,1,0],[4,4,5,6,7],[4,0,1,5,4],[4,1,2,6,5], [4,2,3,7,6],[4,3,0,4,7]]
;     oModel->add, obj_new('IDLgrPolygon', verts, poly = conn, color = [0,255,0], shading = 0)
     oModel->add, obj_new('IDLgrROI', [[-0.1,-0.1,-0.1],[0.1,-0.1,-0.1],[0.1,0.1,-0.1], [-0.1,0.1,-0.1],[-0.1,-0.1, 0.1],[0.1,-0.1, 0.1],[0.1,0.1, 0.1], [-0.1,0.1, 0.1]], color = [0,255,0], style = 0)
end

pro s_objworldMakeTetrahedron, oModel
    s_objworldCone, verts, conn, 3
     oModel->add, obj_new('IDLgrPolygon', verts, poly = conn, color = [0,255,255], shading = 0)
end

pro s_objworldMakeCone, oModel
    s_objworldCone, verts, conn, 20
    oModel->add, obj_new('IDLgrPolygon',verts, poly = conn, color = [255,128,255], shading = 1)
end

pro s_objworldMakePlane, oModel
    s_objworldPlane, verts, conn, 5
    oModel->Add, obj_new('IDLgrPolygon',verts, poly = conn, color = [127,127,127], vert_colors=127, shading = 1)
end     

pro s_objworldMakeGreen_Light, oModel
    s_objworldCone, verts, conn, 4
    oModel->add, obj_new('IDLgrPolygon', verts*0.5, poly = conn, color = [100,255,100], shading = 0)
    oModel->add, obj_new('IDLgrPolyline',[[0,0,0],[0,0,-0.1]],color = [100,255,100])
    oModel->add, obj_new('IDLgrLight',loc = [0,0,0],dir = [0,0,-1],cone = 40,focus = 0,type = 3,color = [100,255,100])
end

pro s_objworldMakeWhite_Light, oModel
    s_objworldCone, verts, conn, 4
    oModel->add, obj_new('IDLgrPolygon', verts*0.5, poly = conn, color = [255,255,255], shading = 0)
    oModel->add, obj_new('IDLgrPolyline', [[0,0,0], [0,0,-0.1]], color = [255,255,255] )
    oModel->add, obj_new('IDLgrLight', loc = [0,0,0], dir = [0,0,-1], cone = 20, focus = 0, type = 3, color = [255,255,255] )
end

pro s_objworldMakeSurface, oModel
    ;  Surface data is read from elevation data file.
    e_height = bytArr(64,64, /NOZERO)
    openR, lun, /get_lun, demo_filepath('elevbin.dat', SUBDIR = ['examples','data'])
    readU, lun, e_height
    free_lun, lun
    zdata = e_height / (1.7 * max(e_height)) + .001
    xdata = (findgen(64)-32.0)/64.0
    ydata = (findgen(64)-32.0)/64.0
    oModel->add, obj_new('IDLgrSurface', zdata, shading = 1, style = 2, datax = xdata, datay = ydata, color = [150,50,150])
end


pro s_objworldSingleHistPlot, oModel
   img = read_bmp(s_getPathForSystem()+'objSinglePlot.bmp', /rgb)
   imgSize = size(img, /dim)
   if (imgSize[1] gt imgSize[2]) then imgSize = [1., 1.*imgSize[2]/imgSize[1]] else imgSize = [1.*imgSize[2]/imgSize[1], 1.]
   oTextureImage = obj_new('IDLgrImage', img, loc = [0.0,0.0], dim = ([0.01,0.01] * imgSize), hide = 1 )
   help, oTextureImage
   oModel->add, oTextureImage
   xp = 0.5*imgSize[0]
   yp = 0.5*imgSize[1]
   zp = 0.1
   oModel->add, obj_new('IDLgrPolygon', [[-xp,-yp,zp],[xp,-yp,zp],[xp,yp,zp],[-xp,yp,zp]],$
                                                        texture_coord = [[0,0],[1,0],[1,1],[0,1]],$
                                                        texture_map = oTextureImage,$
                                                        color = [255,255,255]  )
end


pro s_objworldMixedPlot, oModel
   img = read_bmp(s_getPathForSystem()+'objMixedPlot.bmp', /rgb)
   imgSize = size(img, /dim)
   if (imgSize[1] gt imgSize[2]) then imgSize = [1., 1.*imgSize[2]/imgSize[1]] else imgSize = [1.*imgSize[2]/imgSize[1], 1.]
   oTextureImage = obj_new('IDLgrImage', img, loc = [0.0,0.0], dim = ([0.01,0.01] * imgSize), hide = 1)
   help, oTextureImage
   oModel->add, oTextureImage
   xp = 0.5*imgSize[0]
   yp = 0.5*imgSize[1]
   zp = 0.1
   oModel->add, obj_new('IDLgrPolygon', [[-xp,-yp,zp],[xp,-yp,zp],[xp,yp,zp],[-xp,yp,zp]],$
                                                        texture_coord = [[0,0],[1,0],[1,1],[0,1]],$
                                                        texture_map = oTextureImage,$
                                                        color = [255,255,255])
end


pro s_objworldMakeSubphase, oModel, FramePixSize
   ctab = s_objworldReadPalette(1)
   img = bytArr(3,FramePixSize[0],FramePixSize[1])
   img[0,*,*] = ctab[0,200]
   img[1,*,*] = ctab[1,200]
   img[2,*,*] = ctab[2,200]
   oTextureImage = obj_new('IDLgrImage', img, loc = [0.0,0.0], dim = [0.01,0.01], hide = 1 )
   oModel->add, oTextureImage
   xp = 0.5
   yp = 0.5
   zp = 0.0
   oModel->add, obj_new('IDLgrPolygon', [[-xp,-yp,zp],[xp,-yp,zp],[xp,yp,zp],[-xp,yp,zp]], texture_coord = [[0,0],[1,0],[1,1],[0,1]], texture_map = oTextureImage, color = [200,200,200]  )
end


pro s_objworldMake3D_Text, oModel, thefont
     oModel->add, obj_new('IDLgrText', 'IDL', location = [0,0,0.001], align = 0.5, color = [125,125,125], font = thefont[0] )
end


pro s_objworld_updateMovieViews, wTopBase, infoString = infoString, infoDim = infoDim
    widget_control, wTopBase, get_uValue = state, /no_copy
       oModels = state.oCurrTopModel->get(/all)
    widget_control, wTopBase, set_uValue = state, /no_copy
    if (obj_valid(oModels[0])) then for i = 0, n_elements(oModels)-1 do begin
       oModels[i]->getProperty, uValue = sSubjSelect
       case sSubjSelect of
          'Movie_Info_Text':((oModels[i])->get(position = 0))->setProperty, strings = infoString
          'Movie_Dim_Text':((oModels[i])->get(position = 0))->setProperty, strings = infoDim
          'Movie_Single_Hist_Plot':begin
                   img = read_bmp(s_getPathForSystem()+'objSinglePlot.bmp', /rgb)
                   imgSize = size(img, /dim)
                   print, imgSize
                   if (imgSize[1] gt imgSize[2]) then imgSize = [1., 1.*imgSize[2]/imgSize[1]] else imgSize = [1.*imgSize[2]/imgSize[1], 1.]
                   ((oModels[i])->get(position = 0))->setProperty, data = img, dim = ([0.01,0.01] * imgSize)
                   xp = 0.5*imgSize[0]
                   yp = 0.5*imgSize[1]
                   zp = 0.1
                 ((oModels[i])->get(position = 1))->setProperty, data = [[-xp,-yp,zp],[xp,-yp,zp],[xp,yp,zp],[-xp,yp,zp]]
              endcase
          'Movie_Mixed_Plot':begin
                   img = read_bmp(s_getPathForSystem()+'objMixedPlot.bmp', /rgb)
                   imgSize = size(img, /dim)
                   print, imgSize
                   if (imgSize[1] gt imgSize[2]) then imgSize = [1., 1.*imgSize[2]/imgSize[1]] else imgSize = [1.*imgSize[2]/imgSize[1], 1.]
                   ((oModels[i])->get(position = 0))->setProperty, data = img, dim = ([0.01,0.01] * imgSize)
                   xp = 0.5*imgSize[0]
                   yp = 0.5*imgSize[1]
                   zp = 0.1
                 ((oModels[i])->get(position = 1))->setProperty, data = [[-xp,-yp,zp],[xp,-yp,zp],[xp,yp,zp],[-xp,yp,zp]]
              endcase
          else:
       endcase
    endfor
end


pro s_objworldMakePlot, oModel, thefont

    ; Data for plot generated from Magnitude example in Chapter 13, 'Signal Processing', of _Using IDL_.
    N = 1024       ; number of time samples in data set
    delt = 0.02    ; sampling interval in seconds

    U = -0.3 $     ;  DC component
         + 1.0 * Sin(2*!Pi* 2.8 *delt*FIndGen(N)) $ ;2.8 c/s comp
         + 1.0 * Sin(2*!Pi* 6.25*delt*FIndGen(N)) $ ;6.25 c/s comp
         + 1.0 * Sin(2*!Pi*11.0 *delt*FIndGen(N)); 11.0 c/s comp

    V = fft(U) ; compute spectrum v

    ; signal_x is [0.0, 1.0/(N*delt), ..., 1.0/(2.0*delt)]
    signal_x = FINDGEN(N/2+1) / (N*delt)
    mag = ABS(V[0:N/2]); magnitude of first half of v
    signal_y = 20*ALOG10(mag)
     phi = ATAN(V[0:N/2]) ; phase of first half of v

    xc = [-0.5,1.0/25.0]
    yc = [0.5,1.0/80.0]
     oModel->add, obj_new('IDLgrPolygon', [[-7,-90,-0.002],[30,-90,-0.002],$
                                                   [30,10,-0.002],[-7,10,-0.002]],$
                                                   color = [0,0,0],$
                                                   xcoord_conv = xc,$
                                                   ycoord_conv = yc )
    s = obj_new('IDLgrAxis', 0, range = [0.0,25.0],$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       location = [0,-80.0],$
                                       color = [128,60,39],$
                                       ticklen = 5,$
                                       /exact )
     s->getProperty, ticktext = tt
     tt->setProperty,font = thefont[3]
     oModel->add,s
     oModel->add, obj_new('IDLgrAxis', 0, range = [0.0,25.0],$
                                                   /notext,$
                                                   xcoord_conv = xc,$
                                                   ycoord_conv = yc,$
                                                   location = [0.0,0.0],$
                                                   color = [128,60,39],$
                                                   ticklen = -5,$
                                                   /exact )
     s = obj_new('IDLgrAxis', 1, range = [-80.0,0.0],$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       color = [128,60,39],$
                                       ticklen = 1.0,$
                                       /exact  )
    s->getProperty,ticktext = tt
    tt->setProperty,font = thefont[3]
    oModel->add,s
    oModel->add,obj_new('IDLgrAxis', 1,$
                                                 range = [-80.0,0.0],$
                                                 /notext,$
                                                 xcoord_conv = xc,$
                                                 ycoord_conv = yc,$
                                                 loc = [25.0,0.0],$
                                                 color = [128,60,39],$
                                                 ticklen = -1.0,$
                                                 /exact)
    oModel->add,obj_new('IDLgrPlot', signal_x,$
                                          signal_y,$
                                          xcoord_conv = xc,$
                                          ycoord_conv = yc,$
                                          color = [0,255,255] )
end


pro s_objworldMakeSinglePlot, oModel, thefont

    ; Data for plot generated from Magnitude example in Chapter 13, 'Signal Processing', of _Using IDL_.
    N = 1024       ; number of time samples in data set
    delt = 0.02    ; sampling interval in seconds

    U = -0.3 $     ;  DC component
         + 1.0 * Sin(2*!Pi* 2.8 *delt*FIndGen(N)) $ ;2.8 c/s comp
         + 1.0 * Sin(2*!Pi* 6.25*delt*FIndGen(N)) $ ;6.25 c/s comp
         + 1.0 * Sin(2*!Pi*11.0 *delt*FIndGen(N)); 11.0 c/s comp

    V = fft(U) ; compute spectrum v

    ; signal_x is [0.0, 1.0/(N*delt), ..., 1.0/(2.0*delt)]
    signal_x = FINDGEN(N/2+1) / (N*delt)
    mag = ABS(V[0:N/2]); magnitude of first half of v
    signal_y = 20*ALOG10(mag)
     phi = ATAN(V[0:N/2]) ; phase of first half of v

    xc = [-0.5,1.0/25.0]
    yc = [0.5,1.0/80.0]
     oModel->add, obj_new('IDLgrPolygon', [[-7,-90,-0.002],[30,-90,-0.002],$
                                                   [30,10,-0.002],[-7,10,-0.002]],$
                                                   color = [0,0,0],$
                                                   xcoord_conv = xc,$
                                                   ycoord_conv = yc )
    s = obj_new('IDLgrAxis', 0, range = [0.0,25.0],$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       location = [0,-80.0],$
                                       color = [128,60,39],$
                                       ticklen = 5,$
                                       /exact )
     s->getProperty, ticktext = tt
     tt->setProperty,font = thefont[3]
     oModel->add,s
     oModel->add, obj_new('IDLgrAxis', 0, range = [0.0,25.0],$
                                                   /notext,$
                                                   xcoord_conv = xc,$
                                                   ycoord_conv = yc,$
                                                   location = [0.0,0.0],$
                                                   color = [128,60,39],$
                                                   ticklen = -5,$
                                                   /exact )
     s = obj_new('IDLgrAxis', 1, range = [-80.0,0.0],$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       color = [128,60,39],$
                                       ticklen = 1.0,$
                                       /exact  )
    s->getProperty,ticktext = tt
    tt->setProperty,font = thefont[3]
    oModel->add,s
    oModel->add,obj_new('IDLgrAxis', 1,$
                                                 range = [-80.0,0.0],$
                                                 /notext,$
                                                 xcoord_conv = xc,$
                                                 ycoord_conv = yc,$
                                                 loc = [25.0,0.0],$
                                                 color = [128,60,39],$
                                                 ticklen = -1.0,$
                                                 /exact)
    oModel->add,obj_new('IDLgrPlot', signal_x,$
                                          signal_y,$
                                          xcoord_conv = xc,$
                                          ycoord_conv = yc,$
                                          color = [0,255,255] )
end


pro s_objworldMakeRibbon_Plot, oModel
               x = indgen(200)
               yexp = exp(-x*0.015)
               ysexp = exp(-x*0.015)*sin(x*0.1)
               dataz = fltarr(200,5)
               dataz[*,0] = yexp
               dataz[*,1] = yexp
               dataz[*,2] = replicate(1.1,200)
               dataz[*,3] = ysexp-0.01
               dataz[*,4] = ysexp-0.01
               datay = fltarr(200,5)
               datay[*,0] = 0.0
               datay[*,1] = 1.0
               datay[*,2] = 0.0
               datay[*,3] = 0.0
               datay[*,4] = 1.0
               cbins = bytArr(3,60)
               for i = 0,59 do begin
                   color_convert, float(i)*4., 1., 1., r,g,b, /HSV_RGB
                   cbins[*,59-i] = [r,g,b]
               endfor
               colors = bytArr(3,200*5)
               colors[0,0:599] = replicate(80,3*200)
               colors[1,0:599] = replicate(80,3*200)
               colors[2,0:599] = replicate(200,3*200)
               colors[*,600:799] = cbins[*,(ysexp+1.0)*30.0]
               colors[*,800:999] = cbins[*,(ysexp+1.0)*30.0]
               xc = [-0.5,1.0/200.0]*0.8
               yc = [-0.5,1.0/1.0]*0.1
               zc = [-0.5,1.0/1.0]*0.4
               oModel->add, obj_new('IDLgrAxis', 0,$
                                       range = [0,200],$
                                       color = [255,255,255],$
                                       ticklen = 0.2,$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       zcoord_conv = zc )
               oModel->add, obj_new('IDLgrAxis', 2,$
                                       range = [-1.,1.],$
                                       color = [255,255,255],$
                                       ticklen = 4,$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       zcoord_conv = zc )
               oModel->add, obj_new('IDLgrSurface', dataz,$
                                       style = 2,$
                                       vert_colors = colors,$
                                       datay = datay,$
                                       max_value = 1.05,$
                                       shading = 1,$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       zcoord_conv = zc )
              oModel->add, obj_new('IDLgrSurface', dataz,$
                                       style = 3,$
                                       color = [0,0,0],$
                                       datay = datay,$
                                       max_value = 1.05,$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       zcoord_conv = zc )
end

pro s_objworldMakeBar_Plot, oModel
               dataz = dist(8)
               dataz[[1,3,5],*] = -1
               dataz[*,[1,3,5]] = -1
               dataz = dataz + 1
               cbins = [   [255,0,0], [255,85,0], [255,170,0], [255,255,0], [170,255,0], [85,255,0], [0,255,0] ]
               colors = bytArr(3, 8*8)
               minz = min(dataz)
               maxz = max(dataz)
               zi = round((dataz - minz)/(maxz-minz) * 6.0)
               colors[*,*] = cbins[*,zi]
               xc = [-0.5,1.0/8.0]*0.4
               yc = [-0.5,1.0/8.0]*0.4
               zc = [0,1.0/8.0]*0.4
              oModel->add, obj_new('IDLgrAxis', 0,$
                                       range = [0,8],$
                                       major = 5,$
                                       color = [255,255,255],$
                                       ticklen = 0.2,$
                                       /exact,$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       zcoord_conv = zc )
              oModel->add, obj_new('IDLgrAxis', 1,$
                                       range = [0,8],$
                                       major = 5,$
                                       color = [255,255,255],$
                                       ticklen = 0.2,$
                                       /exact,$
                                       xcoord_conv = xc,$
                                       ycoord_conv = yc,$
                                       zcoord_conv = zc )
              oModel->add, obj_new('IDLgrAxis', 2,$
                                         range = [0,8],$
                                         major = 5,$
                                         color = [255,255,255],$
                                         ticklen = 0.2,$
                                         /exact,$
                                         xcoord_conv = xc,$
                                         ycoord_conv = yc,$
                                         zcoord_conv = zc )
              oModel->add, obj_new('IDLgrSurface', dataz,$
                                         STYLE = 6,$
                                         VERT_COLORS = colors,$
                                         xcoord_conv = xc,$
                                         ycoord_conv = yc,$
                                         zcoord_conv = zc )
end


pro s_objworldMakeTextured_Surface, oModel
               ;  Surface data is read from elevation data file.
               e_height = bytArr(64,64, /nozero)
               openr, lun, /get_lun, demo_filepath('elevbin.dat', subdir = ['examples','data'])
               readu, lun, e_height
               free_lun, lun

               ;Get image data for texture map.
               read_jpeg, demo_filepath( 'elev_t.jpg', subdir = ['examples','data'] ), e_image, true = 3

               zm = max(e_height)
               zdata = float(e_height)/(1.7*float(zm))
               xdata = (findgen(64)-32.0)/64.0
               ydata = (findgen(64)-32.0)/64.0
               oTextureImage = obj_new('IDLgrImage', e_image,$ ;>255,$
                                                        hide = 1,$
                                                        dim = [0.01,0.01],$
                                                        interleave = 2 )
              oModel->add, oTextureImage
              oModel->add, obj_new('IDLgrSurface', zdata,$
                                          shading = 1,$
                                          style = 2,$
                                          datax = xdata,$
                                          datay = ydata,$
                                          color = [255,255,255],$
                                          TEXTURE_MAP = oTextureImage )
end


function s_objworldMakeObj, sSubjSelect,$       ; Selection (String)
                                  theFont = theFont,$    ; TextFont _extra
                                  FramePixSize = FramePixSize,$     ;Pixel Dimensions of Frame
                                  PassoModel = PassoModel
                                  the2DData = the2DData  ; 2D-Data

    oModel = obj_new('IDLgrModel')
    case sSubjSelect of
        'Movie_Single_Hist_Plot':s_objworldSingleHistPlot, oModel
        'Movie_Mixed_Plot':s_objworldMixedPlot, oModel
        'Movie_Info_Text':s_objworldMake3D_Text, oModel, thefont
        'Movie_Dim_Text':s_objworldMake3D_Text, oModel, thefont
        'Sphere':s_objworldMakeSphere, oModel
        'Cube':s_objworldMakeCube, oModel
        'Tetrahedron':s_objworldMakeTetrahedron, oModel
        'Plane':s_objworldMakePlane, oModel
        'Cone':s_objworldMakeCone, oModel
        'Green_Light':s_objworldMakeGreen_Light, oModel
        'Surface':s_objworldMakeSurface, oModel
        'White_Light':s_objworldMakeWhite_Light, oModel
        'Plot':s_objworldMakePlot, oModel, thefont
        'Ribbon_Plot':s_objworldMakeRibbon_Plot, oModel
        'Bar_Plot':s_objworldMakeBar_Plot, oModel
        '4x4Mesh':s_objworldMake4x4Mesh, oModel
        'Textured_Surface':s_objworldMakeTextured_Surface, oModel
        '2D_Frame':s_objworldMakeFrame, oModel, FramePixSize
        'grROIObject':s_objworldMakegrROIObject, oModel, PassoModel
        'grPolygonModel':s_objworldMakegrPolygonModel, oModel, PassoModel
        'oSubPhase':s_objworldMakeSubphase, oModel, FramePixSize
    endcase
    oModel->setProperty, uValue = sSubjSelect
    return, oModel
end


pro s_objworldNewMode, state, mode
    widget_control, /hourglass
    state.oModelMan->setProperty, mode = mode
    widget_control, state.wModelModeRadio, set_value = mode
end


pro s_objworldAdd, state, oModel, as_child = as_child
    if keyWord_set(as_child) then state.oSelected->add, oModel else state.oCurrTopModel->add, oModel

    state.oCurrView->getProperty, uValue = view_uval
    nList =  n_elements(*(state.oModelListArr[view_uval.num]))
    *(state.oModelListArr[view_uval.num]) = [oModel, *(state.oModelListArr[view_uval.num])]
;   if (nList gt 1) then  *(state.oModelListArr[view_uval.num]) = [(*(state.oModelListArr[view_uval.num]))[1:nList-1], (*(state.oModelListArr[view_uval.num]))[0], (*(state.oModelListArr[view_uval.num]))[nList]]

    state.fModelList = 0

    ;Make the new object be the current selection...
    state.oSelected = oModel
    ;g = oModel->get(pos = 0)
    ;if obj_isa(g,'IDLgrText') then rect = state.win->gettextdimensions(g)

    state.oModelMan->setTarget, state.oSelected
    FORWARD_FUNCTION IDLexModelManip__get_bounds
    box = IDLexModelManip__get_bounds(state.oSelected, state.oSelected)    
    
    state.oSelected->getProperty, uValue = s
    widget_control, state.wTopBase, tlb_set_title = 'S_Interactive_Object_Analysis ||   ' + state.path + '...  ||   ' + 'Current selection:' + s
    widget_control, state.wModelDeleteButton, sensitive = 1
    widget_control, state.wAddChildButton, sensitive = 1
    widget_control, state.wModelUnselectButton, sensitive = 1
    widget_control, state.wModelModeRadio, sensitive = 1
    widget_control, state.wSaveButton, sensitive = ([1,0])[lmgr(/demo)]
    widget_control, state.wModelSelectButton, sensitive = n_elements(*(state.oModelListArr[view_uval.num])) gt 2

    s_objworld_update_wListFrame, state
    demo_draw, state.win, state.oScene, debug = state.debug
end


pro s_objworldCleanup, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
         ;Remove manipulators.
       if obj_valid(state.oModelMan) then state.oModelMan->setTarget, obj_new() else obj_destroy, state.oModelMan
       if obj_valid(state.oViewMan) then state.oViewMan->setTarget, obj_new(), state.win else obj_destroy, state.oViewMan

         ;Clean up heap variables.
       for i = 0,n_tags(state)-1 do begin
           case size(state.(i), /tname) of
               'POINTER':ptr_free, state.(i)
               'OBJREF':obj_destroy, state.(i)
               else:
          endcase
        endfor

         ;Restore the color table.
       tvlct, state.colortable
       if widget_info(state.groupLeader, /valid_id) then widget_control, state.groupLeader, /map
end


pro s_objworldEvent, ev
    case tag_names(ev, /structure_name) of
        'WIDGET_BASE':begin
                           widget_control, ev.top, get_uValue = state, /no_copy
                             widget_control, state.wTopRowBase, xSize = ev.x-10, ySize = ev.y-10
                             widget_control, state.wDraw, xSize = ev.x-110, ySize = ev.y-10
                           widget_control, ev.top, set_uValue = state, /no_copy
                           return
            endcase
        'WIDGET_KILL_REQUEST':begin
                           widget_control, ev.top, /destroy
                           return
            endcase
        'ADD_GRROIOBJECTS':uval = ev.uval
        'NOTIFY_ANALYSISKILL':uval = ev.uval
        else:widget_control, ev.id, get_uval = uval
     endcase

       ;If mouse buttons are down, only process draw widget events.
    widget_control, ev.top, get_uValue = state, /no_copy

    if (state.btndown eq 1) then begin
        if (uval[0] eq 'DRAW') then begin
            if ev.type eq 0 then begin ; Button down event?...
                widget_control, ev.top, set_uValue = state, /no_copy
                return ; ...ignore it.  A mouse button is already down.
             endif
        endif else begin
            widget_control, ev.top, set_uValue = state, /no_copy
            return
        endelse
     endif
     widget_control, ev.top, set_uValue = state, /no_copy

    ;Normal event handling.
    case uval[0] of
       'FILEDELETE':
        'PRINT':begin
            widget_control, ev.top, get_uValue = state, /no_copy
                    state.win->getProperty, dimension = wdims
                    oViews = state.oScene->get(/all)
                    for i = 0,n_elements(oViews)-1 do begin
                        oViews[i]->IDLgrView::getproperty, loc = loc, dim = vdim
                        loc = loc/DPI
                        vdim = vdim/DPI
                        oViews[i]->IDLgrView::setProperty, loc = loc, dim = vdim, units = 1
                    endfor
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'ABOUT':begin
              ; File not exists... program fall... searching i include the next :D FASL 
             helpFileName = demo_filepath("idl_demo.hlp", $
                               SUBDIR=['examples','demo','demohelp'])
             dummy=FINDFILE(helpFileName, count=count)
             if (count GT 0) then begin
                ;topicNum = 3250 ;; slicer
               topicNum = 3180
                ONLINE_HELP, topicNum, /CONTEXT, $
                   book=helpFileName, $
                   /FULL_PATH
             endif else begin
                XDISPLAYFILE, FILEPATH("widabout.txt", $
                    SUBDIR=['help', 'widget']), $
                    TITLE="Help Not found", $
                    GROUP=ev.top, $
                    /MODAL, $
                    WIDTH=72, HEIGHT=24
             endelse        
            end
        'VRML':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            if (state.oCurrView ne obj_new()) then begin
                file = dialog_pickfile(  /write, file = 'untitled.wrl', group = ev.top, filter = '*.wrl' )
                if (file ne '') then begin
                    widget_control, /hourglass
                    state.win->getProperty, dimension = wdims,$
                                            resolution = res,$
                                            color_model = cm,$
                                            n_colors = icolors
                    oVRML = obj_new('IDLgrVRML', dimensions = wdims,$
                                                  resolution = res,$
                                                  color_model = cm,$
                                                  n_colors = icolors )
                    oVRML->setProperty, filename = file
                    demo_draw, oVRML, state.oCurrView, debug = state.debug
                    obj_destroy, oVRML
                    end
                end
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'CLIPBOARD':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            state.win->getProperty, dimension = wdims,$
                                    resolution = res,$
                                    color_model = cm,$
                                    n_colors = icolors
            oClipboard = obj_new('IDLgrClipboard', dimensions = wdims,$
                                                 resolution = res,$
                                                 color_model = cm,$
                                                 n_colors = icolors )
            demo_draw, oClipboard, state.oScene, debug = state.debug
            obj_destroy, oClipboard
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'PRINT':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            oPrinter = obj_new('IDLgrPrinter')
            if (dialog_printersetup(oPrinter)) then begin
                if (dialog_printjob(oPrinter)) then begin
                    oPrinter->getProperty,resolution = res
                    DPI = 2.54/float(res)
                    state.win->getProperty,resolution = res
                    DPI = 2.54/float(res)

                    ;Hack, swap from pixels to inches for views...
                    state.win->getProperty, dimension = wdims
                    oViews = state.oScene->get(/all)
                    for i = 0,n_elements(oViews)-1 do begin
                        oViews[i]->IDLgrView::getproperty, loc = loc, dim = vdim
                        loc = loc/DPI
                        vdim = vdim/DPI
                        oViews[i]->IDLgrView::setProperty, loc = loc, dim = vdim, units = 1
                    endfor

                    ;...PRINT!...
                    demo_draw, oPrinter, state.oScene, debug = state.debug
                    oPrinter->newdocument

                    ;...and back to pixels
                    for i = 0,n_elements(oViews)-1 do begin
                        oViews[i]->IDLgrView::getproperty, loc = loc, dim = vdim
                        loc = loc*DPI
                        vdim = vdim*DPI
                        oViews[i]->IDLgrView::setProperty, loc = loc, dim = vdim, units = 0
                    endfor

                    end
                end
            obj_destroy,oPrinter
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'ANTIALIAS':begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                     s_objworldSceneAntiAlias, state.win, state.oScene, 8
                  widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'LOAD':begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                  if ((state.oSelected ne obj_new()) and (state.fTool eq 1)) then begin
                      file = dialog_pickfile( /read, /must_exist, path = state.path, get_path = path, group = ev.top, filter = '*.sav', /multiple_files)
                      if (file[0] ne '') then begin
                          state.path = path
                          if (n_elements(file) gt 1) then file[[0,n_elements(file)-1]] = file[ [n_elements(file)-1, 0] ]
                             widget_control, state.wListFile, get_uValue = uvalueFileParameter, /no_copy
                        if ((*uvalueFileParameter.value)[0] eq '-NO FILES-') then begin
                           widget_control, state.wListFile, set_value = file
                           if (ptr_valid(uvalueFileParameter.value)) then ptr_free, uvalueFileParameter.value
                           uvalueFileParameter.value = ptr_new(file, /no_copy)
                        endif else begin
                           file = [file, *uvalueFileParameter.value]
                           widget_control, state.wListFile, set_value = file
                           if ptr_valid(uvalueFileParameter.value) then ptr_free, uvalueFileParameter.value
                           uvalueFileParameter.value = ptr_new(file, /no_copy)
                        endelse
                        restore, (*uvalueFileParameter.value)[0], /relaxed_structure_assignment
                    widget_control, state.wListFile, set_uValue = uvalueFileParameter, /no_copy
                         s_objworldAdd, state, tmp_obj
                         widget_control, state.wFileSelectButton, sensitive = 1
                         widget_control, state.wFileUnselectButton, sensitive = 1
                         widget_control, state.wFileDeleteButton, sensitive = 1
                      endif else i = s_apop_shout('Nothing loaded')
                  endif
                  widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'SAVE':begin
               widget_control, ev.top, get_uValue = state, /no_copy
               if ((state.oSelected ne obj_new()) and $
                  (state.oSelected ne state.oCurrTopModel) and $
                  (state.fTool eq 1)) then begin

                    file = dialog_pickfile(/write, file = 'untitled.sav', group = ev.top, filter = '*.sav')
                    if (file ne '') then begin
       ;               Isolate tmp_obj from the tree.
                       state.oSelected->getProperty, parent = parent
                       parent->remove, state.oSelected
                       state.oModelMan->setTarget, obj_new()
                       tmp_obj = state.oSelected
       ;               Save it.
                       save, tmp_obj, filename = file
       ;               Repair the tree.
                       parent->add, state.oSelected
                       state.oModelMan->setTarget, state.oSelected
                   endif
               endif
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'MODELSELECT':begin ; Select next object.
               widget_control, ev.top, get_uValue = state, /no_copy
               wDraw = state.wDraw
               widget_control, ev.top, set_uValue = state, /no_copy
               s_objworldEvent, {id:wDraw,$
                                top:ev.top,$
                                handler:0L,$
                                type:0,$; Button down
                                press:4,$ ; Right Mouse-button.
                                x:-2,$
                                y:-2  }
        endcase
        'VIEWSELECT':begin ; Select next view.
               widget_control, ev.top, get_uValue = state, /no_copy
               wDraw = state.wDraw
               widget_control, ev.top, set_uValue = state, /no_copy
               s_objworldEvent, {id:wDraw,$
                                top:ev.top,$
                                handler:0L,$
                                type:0,$; Button down
                                press:4,$ ; Right Mouse-button.
                                x:-2,$
                                y:-2  }
        endcase
        'MODELUNSELECT':begin
               widget_control, /hourglass
               widget_control, ev.top, get_uValue = state, /no_copy
               state.oSelected = state.oCurrTopModel
               widget_control, state.wModelDeleteButton, sensitive = 0
               widget_control, state.wAddChildButton, sensitive = 0
               widget_control, state.wModelUnselectButton, sensitive = 0
               widget_control, state.wModelModeRadio, sensitive = 0
               widget_control, state.wModelSelectButton, sensitive = 1
               widget_control, state.wSaveButton, sensitive = 0
               widget_control, state.wTopBase, tlb_set_title = 'S_Interactive_Object_Analysis ||   ' + state.path + '...  ||   ' + 'CurrentTopModel selected'
               state.oModelMan->setTarget, obj_new()
               demo_draw, state.win, state.oScene, debug = state.debug
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'MAKEROTATIONMOVIE':begin
            widget_control, ev.top, get_uValue = state, /no_copy
             state.win->getProperty, dimension = mainDims, resolution = mainRes, color_model = cm, n_colors = icolors
             oClipboard = obj_new('IDLgrClipboard', dimensions = mainDims, resolution = mainRes, color_model = cm, n_colors = icolors)
             state.oWorldRotModel->getProperty, transform = t
             time = 0
             for i = 0,360 do begin
                 cosa = cos(!DTOR*i)
                 sina = sin(!DTOR*i)
                 rotT = [[ cosa, 0., sina, 0.],$
                         [   0., 1.,   0., 0.],$
                         [-sina, 0., cosa, 0.],$
                         [   0., 0.,   0., 1.]]
                 state.oWorldRotModel->setProperty, transform = t # rotT
                 demo_draw, state.win, state.oScene, debug = state.debug

                 timeStr = strCompress(string(time))
                 for j = strLen(timeStr), 4 do timeStr = strCompress('0'+timeStr, /rem)
                 fileName = strCompress(s_getPathForSystem()+'3DRotMovie_' +string(timeStr) +'.bmp', /rem)
                 oAlias = 1
                 s_objworldSceneAntiAlias, state.win, state.oScene, 8, oAlias = oAlias
                 oClipboard->draw, oAlias, fileName = fileName
                 obj_destroy, oAlias
                 time += 1
             endfor
             obj_destroy, oClipboard
          widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'TOOLVIEW':begin   ; View Manipulator tool oSelected.
               widget_control, ev.top, get_uValue = state, /no_copy
                state.oModelMan->setTarget, obj_new()
                state.oViewMan->setTarget, state.oCurrView, state.win
                state.oSelected = state.oCurrView
                state.oSelected->getProperty, uValue = view_uvalue
                widget_control, state.wTopBase, tlb_set_title = 'S_Interactive_Object_Analysis ||   ' + state.path + '...  ||   ' + 'Current selection:' + view_uvalue.name
                demo_draw, state.win, state.oScene, debug = state.debug

                widget_control, state.wModelModeRadio, sensitive = 0
                widget_control, state.wViewControlBase, map = 1
                widget_control, state.wModelControlBase, map = 0
                widget_control, state.wLoadButton, sensitive = 0
                widget_control, state.wSaveButton, sensitive = 0

                state.fTool = 0
                widget_control, state.wToolModel, sensitive = 1
                widget_control, state.wToolView, sensitive = 0

               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'TOOLMODEL':begin  ; Model Manipulator tool oSelected.
               widget_control, ev.top, get_uValue = state, /no_copy
                state.fTool = 1
                state.oViewMan->setTarget, obj_new(), state.win
                wDraw = state.wDraw
                widget_control, ev.top, set_uValue = state, /no_copy
                s_objworldEvent, {id:wDraw,$
                                     top:ev.top,$
                                     handler:0L,$
                                     type:0,$; Button down
                                     press:4,$ ; Right Mouse-button.
                                     x:-1,$
                                     y:-1  }
                widget_control, ev.top, get_uValue = state, /no_copy
                widget_control, state.wViewControlBase, map = 0
                widget_control, state.wModelControlBase, map = 1
                state.oCurrView->getProperty, uValue = view_uval
                num = n_elements( *(state.oModelListArr[view_uval.num]) )
                widget_control, state.wModelSelectButton, sensitive = ([0,1])[num gt 2]
                widget_control, state.wLoadButton, sensitive = 1
                widget_control, state.wSaveButton, sensitive = ([1,0])[lmgr(/demo)]

                widget_control, state.wToolModel, sensitive = 0
                widget_control, state.wToolView, sensitive = 1

               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'MODELMODE':begin
               widget_control, ev.top, get_uValue = state, /no_copy
               s_objworldNewMode, state, ev.value
               demo_draw, state.win, state.oScene, debug = state.debug
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'ADDVIEW':begin
               widget_control, ev.top, get_uValue = state, /no_copy
               state.win->getProperty, dim = wdim
               state.oCurrView = s_objworldMakeView(  wdim[0], wdim[1],$
                                                              {name:'ObjView ' + strCompress(state.highestViewCount),$
                                                               num:state.highestViewCount} )
               state.oModelListArr[state.highestViewCount] = ptr_new(obj_new())
               state.oTrackballMB1->Reset, wdim/2.0, wdim[0]/2.0
               state.oTrackballMB2->Reset, wdim/2.0, wdim[0]/2.0, mouse = 2
               state.selectedViewCount = state.selectedViewCount + 1
               state.highestViewCount = state.highestViewCount + 1
               state.oScene->add, state.oCurrView
               state.oViewMan->setTarget, state.oCurrView, state.win
               state.oSelected = state.oCurrView
               state.oCurrView->getProperty, uValue = view_uvalue
               widget_control, state.wViewDeleteButton, sensitive = ([0,1])[view_uvalue.num ne 0]
               widget_control, state.wViewSelectButton, sensitive = 1
               s_objworldGetViewObjs, state.oSelected, w,b,t
               state.oWorldRotModel = w
               state.oBasePolygonAtom = b
               state.oCurrTopModel = t
               widget_control, state.wTopBase, tlb_set_title = 'S_Interactive_Object_Analysis ||   ' + state.path + '...  ||   ' + 'Current selection:' + view_uvalue.name
               demo_draw, state.win, state.oScene, debug = state.debug
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'ADD':begin
               widget_control, /hourglass
               widget_control, ev.top, get_uValue = state, /no_copy
               if state.oBasePolygonAtom ne obj_new() then $
                 if (tag_names(ev, /structure_name) eq 'ADD_GRROIOBJECTS') then $
                   s_objworldAdd, state, s_objworldMakeObj( uval[1], PassoModel = ev.oModel ) else $
                   s_objworldAdd, state, s_objworldMakeObj( uval[1], theFont = state.theFont )
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'ADDCHILD':begin
               widget_control, /hourglass
               widget_control, ev.top, get_uValue = state, /no_copy
               if state.oBasePolygonAtom ne obj_new() then $
                 if (tag_names(ev, /structure_name) eq 'ADD_GRROIOBJECTS') then $
                     s_objworldAdd, state, s_objworldMakeObj( uval[1], PassoModel = ev.oModel ), /as_child else $
                     s_objworldAdd, state, s_objworldMakeObj( uval[1], theFont = state.theFont ), /as_child
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'SLIDERSCALE':begin
           widget_control, ev.top, get_uValue = state, /no_copy
              state.worldMatrixState[3] = 1. / ev.value
              state.oWorldRotModel->getProperty, transform = t
              t[3,3] = state.worldMatrixState[3]
              state.oWorldRotModel->setProperty, transform = t
              demo_draw, state.win, state.oScene, debug = state.debug
           widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'SLIDERALPHA':begin
           widget_control, ev.top, get_uValue = state, /no_copy
              worldRotModelMatrix = state.worldRotModelMatrix
              state.worldMatrixState[0] = ev.value
              worldRotModelMatrix[3,3] = state.worldMatrixState[3]
              state.oWorldRotModel->setProperty, transform = worldRotModelMatrix
              state.oWorldRotModel->rotate, [1,0,0], state.worldMatrixState[0]
              state.oWorldRotModel->rotate, [0,1,0], state.worldMatrixState[1]
              state.oWorldRotModel->rotate, [0,0,1], state.worldMatrixState[2]
              demo_draw, state.win, state.oScene, debug = state.debug
           widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'SLIDERBETA':begin
          widget_control, ev.top, get_uValue = state, /no_copy
              worldRotModelMatrix = state.worldRotModelMatrix
              state.worldMatrixState[1] = ev.value
              worldRotModelMatrix[3,3] = state.worldMatrixState[3]
              state.oWorldRotModel->setProperty, transform = worldRotModelMatrix
              state.oWorldRotModel->rotate, [1,0,0], state.worldMatrixState[0]
              state.oWorldRotModel->rotate, [0,1,0], state.worldMatrixState[1]
              state.oWorldRotModel->rotate, [0,0,1], state.worldMatrixState[2]
              demo_draw, state.win, state.oScene, debug = state.debug
           widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'SLIDERGAMMA':begin
           widget_control, ev.top, get_uValue = state, /no_copy
              worldRotModelMatrix = state.worldRotModelMatrix
              state.worldMatrixState[2] = ev.value
              worldRotModelMatrix[3,3] = state.worldMatrixState[3]
              state.oWorldRotModel->setProperty, transform = worldRotModelMatrix
              state.oWorldRotModel->rotate, [1,0,0], state.worldMatrixState[0]
              state.oWorldRotModel->rotate, [0,1,0], state.worldMatrixState[1]
              state.oWorldRotModel->rotate, [0,0,1], state.worldMatrixState[2]
              demo_draw, state.win, state.oScene, debug = state.debug
           widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'DEL':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            if ((state.oSelected ne obj_new()) and (state.oSelected ne state.oCurrTopModel)) then begin
                if (state.fTool eq 0) then begin
                    state.oSelected->getProperty, uValue = uvalue
                    if (uValue.num ne 0) then begin ; cannot delete first one
                        state.oViewMan->setTarget, obj_new(), state.win
                        state.oSelected->getProperty, parent = p
                        p->remove, state.oSelected
                        obj_destroy, state.oSelected
                        state.selectedViewCount = state.selectedViewCount - 1

                        widget_control, state.wViewSelectButton, sensitive = ([0,1])[state.selectedViewCount gt 1]

                        ; select next view.
                        wDraw = state.wDraw
                        widget_control, ev.top, set_uValue = state, /no_copy
                        s_objworldEvent, {id:wDraw,$
                                            top:ev.top,$
                                            handler:0l,$
                                            type:0,$  ; Button down
                                            press:4,$ ; Right Mouse-button.
                                            x:-2, y:-2  }
                        widget_control, ev.top, get_uValue = state, /no_copy
                    endif else widget_control, state.wTopBase, tlb_set_title = 's_3DView |-> ' + state.path + '...  ||   ' + 'Cannot delete initial view'
                  s_objworld_update_wListFrame, state
                endif else begin ; Current tool is Model Manipulator
                    state.oModelMan->setTarget, obj_new()
                    state.oSelected->getProperty, parent = p
                    p->remove, state.oSelected
                    obj_destroy, state.oSelected
                    state.oCurrView->getProperty, uValue = view_uval
                    indx = where( obj_valid(*(state.oModelListArr[view_uval.num])), count)
                    if indx[0] eq -1 then begin
                        *(state.oModelListArr[view_uval.num]) = obj_new()
                        state.oSelected = state.oCurrTopModel
                        widget_control, state.wTopBase, tlb_set_title = 's_3DView |-> ' + state.path + '...  ||   ' + 'CurrentTopModel selected'
                        widget_control, state.wModelDeleteButton, sensitive = 0
                        widget_control, state.wAddChildButton, sensitive = 0
                        widget_control, state.wModelUnselectButton, sensitive = 0
                        widget_control, state.wModelSelectButton, sensitive = 0
                        widget_control, state.wSaveButton, sensitive = 0
                        widget_control, state.wModelModeRadio, sensitive = 0
                        demo_draw, state.win, state.oScene, debug = state.debug
                      s_objworld_update_wListFrame, state
                    endif else begin
                        *(state.oModelListArr[view_uval.num]) = [ (*(state.oModelListArr[view_uval.num])) [indx], obj_new()]

                   ;Select something.
                        wDraw = state.wDraw
                        widget_control, ev.top, set_uValue = state, /no_copy
                        s_objworldEvent, {id:wDraw,$
                                                top:ev.top,$
                                                handler:0L,$
                                                type:0,$  ; Button down
                                                press:4,$ ; Right Mouse-button.
                                                x:-1,$
                                                y:-1  }
                        widget_control, ev.top, get_uValue = state, /no_copy
                      s_objworld_update_wListFrame, state
                        widget_control, ev.top, set_uValue = state, /no_copy
                        return
                        end
                    end
                end
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'DRAGQLOW':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            state.fDragQ = 0
            widget_control, state.wDragQLow, sensitive = 0
            widget_control, state.wDragQMedium, sensitive = 1
            widget_control, state.wDragQHigh, sensitive = 1
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'DRAGQMEDIUM':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            state.fDragQ = 1
            widget_control, state.wDragQLow, sensitive = 1
            widget_control, state.wDragQMedium, sensitive = 0
            widget_control, state.wDragQHigh, sensitive = 1
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'DRAGQHIGH':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            state.fDragQ = 2
            widget_control, state.wDragQLow, sensitive = 1
            widget_control, state.wDragQMedium, sensitive = 1
            widget_control, state.wDragQHigh, sensitive = 0
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'GRID(ONOFF)':begin
               widget_control, /hourglass
               widget_control, ev.top, get_uValue = state, /no_copy
               if obj_valid(state.oCurrView) then begin
                   if obj_valid(state.oBasePolygonAtom) then begin
                       state.oBasePolygonAtom->setProperty, hide = 1-s_ToggleButtonOnOffState(state.wGridButton)
                       demo_draw, state.win, state.oScene, debug = state.debug
                   endif
               endif
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        
        'BACKGROUND(ONOFF)':begin
               widget_control, /hourglass
               widget_control, ev.top, get_uValue = state, /no_copy
               if obj_valid(state.oCurrView) then begin
                   if obj_valid(state.oBasePolygonAtom) then begin
                       state.fBACKColorButton = s_ToggleButtonOnOffState(state.wBACKColorButton)

                       ;Device, Decomposed=0
                       ;!P.Color = !P.Color < (!D.Table_Size - 1)
                       ;color = PickColor(!P.Color, Cancel=cancelled)
                       color = PickColor(Cancel=cancelled)
                       IF NOT cancelled THEN BEGIN
                           ;TVLCT, color, !P.Color
                           ;Plot, data
                           ;color = colorFondo               
    
                            state.oCurrView->SetProperty, color = transpose(color)
                           
                       ENDIF
                       demo_draw, state.win, state.oScene, debug = state.debug
                   endif
               endif
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'CENTRED(ONOFF)':begin
               widget_control, /hourglass
               widget_control, ev.top, get_uValue = state, /no_copy
               if obj_valid(state.oCurrView) then begin
                   if obj_valid(state.oBasePolygonAtom) then begin
                       state.fCenterButton = s_ToggleButtonOnOffState(state.wCenterButton)
                       state.oCurrView->getProperty, dim = dim, loc = loc
                       
                       state.oModelMan->getProperty, target = manip                                              
                       manip->GetProperty, TRANSFORM=tm
                       box = IDLexModelManip__get_bounds(manip, manip)
                   
                       demo_draw, state.win, state.oScene, debug = state.debug
                   endif
               endif
               widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'DRAW':begin
            widget_control, ev.top, get_uValue = state, /no_copy

            ; Expose.
            if (ev.type eq 4) then demo_draw, state.win, state.oScene, debug = state.debug

            ; Handle trackball updates.
            if state.oTrackballMB2->Update(ev, transform = qmat) then begin
                state.oWorldRotModel->getProperty, transform = t
                state.oWorldRotModel->setProperty,transform = t # qmat
                demo_draw, state.win, state.oScene, debug = state.debug
            endif
            have_mb1_transform = state.oTrackballMB1->Update(ev, transform = mb1_transform)

            ; Handle other events (selection, etc.) ...
            case ev.type of
                0:begin ; Button press
                    case 1 of
                        (ev.press eq 4) and (state.fTool eq 0):begin
                            widget_control, /hourglass
                            if ev.x eq -2 then begin
                                picked = state.oScene->get()
                            endif else begin
                                state.oViewMan->setProperty,hide = 1
                                picked = state.win->select(state.oScene, [ev.x,ev.y])
                                state.oViewMan->setProperty, hide = 0
                            endelse
                            if obj_valid(picked[0]) then begin
                                state.oSelected = picked[0]
                                state.oSelected->getProperty,uValue = view_uvalue
                                widget_control, state.wViewDeleteButton, sensitive = ([1,0])[view_uvalue.num eq 0]
                                state.strSelected = 'Current selection:' + view_uvalue.name
                                state.oScene->remove, picked[0]
                                state.oScene->add, picked[0]
                            endif else begin
                                state.oSelected = obj_new()
                                state.strSelected = 'CurrentTopModel selected'
                            endelse

                            ; point the oViewMan at the node...
                            state.oViewMan->getProperty, target = manip
                            if (manip ne state.oSelected) then  state.oViewMan->setTarget,obj_new(),state.win
                            if state.oSelected ne obj_new() then begin
                                state.oCurrView = state.oSelected
                                state.oCurrView->getProperty,dim = dim,loc = loc
                                state.oTrackballMB1->Reset, loc + dim/2.0, dim[0]/2.0
                                state.oTrackballMB2->Reset, loc + dim/2.0, dim[0]/2.0, mouse = 2
                                s_objworldGetViewObjs,state.oSelected,w,b,t
                                state.oWorldRotModel = w
                                state.oBasePolygonAtom = b
                                state.oCurrTopModel = t
                                state.oViewMan->setTarget,state.oSelected,state.win
                            endif
                            widget_control, state.wTopBase, tlb_set_title = 's_3DView |-> ' + state.path + '...  ||   ' + state.strSelected
                            state.win->draw,state.oScene
                            demo_draw, state.win, state.oScene, debug = state.debug
                            end
                        ev.press eq 4:begin
                            widget_control, /hourglass

                            if ev.x lt 0 then begin
                                state.oCurrView->getProperty, uValue = view_uval
                                if ((where(tag_names(ev) eq 'LISTOBJECT'))[0]  ne -1) then begin
                                   picked = ((*(state.oModelListArr[view_uval.num]))[ state.fModelList ])->get(Position = ev.ListObject)
                            endif else if ((where(tag_names(ev) eq 'LISTFRAME'))[0]  ne -1) then begin
                                   picked = ((*(state.oModelListArr[view_uval.num]))[ ev.ListFrame])
                                endif else begin
                                   if (n_elements(*(state.oModelListArr[view_uval.num])) gt 1) then begin
                                       state.fModelList = state.fModelList + ([0,1])[abs(ev.x) - 1]
                                       state.fModelList = state.fModelList $
                                           mod (  n_elements(  *(state.oModelListArr[view_uval.num])  )  - 1 ) ; Last item on list is obj_new().
                                       picked = ((*(state.oModelListArr[ view_uval.num]))[ state.fModelList ])->get()
                                   endif else  begin
                                     picked = obj_new()
                                   endelse
                                endelse
                            endif else begin
                               if obj_valid(state.oModelMan) then begin
                                   state.oModelMan->setProperty,hide = 1
                                     picked = state.win->select( state.oCurrView,[ev.x,ev.y] )
                                   state.oModelMan->setProperty,hide = 0
                                endif else begin
                                   state.oModelMan = obj_new('IDLexModelManip', translate = [1,1,1], selector_color = [255,255,255], manipulator_color = [255, 60, 60] )
                                   picked = state.win->select( state.oCurrView,[ev.x,ev.y] )
                                endelse
                            endelse
                            if obj_valid(picked[0]) then begin
                                if (picked[0] eq state.oBasePolygonAtom) then begin
                                    state.oSelected = state.oCurrTopModel
                                    state.strSelected = 'CurrentTopModel selected'
                                    widget_control, state.wModelDeleteButton, sensitive = 0
                                    widget_control, state.wAddChildButton, sensitive = 0
                                    widget_control, state.wModelUnselectButton, sensitive = 0
                                    widget_control, state.wSaveButton, sensitive = 0
                                endif else begin
                                    if (obj_isa(picked[0],'IDLgrModel') eq 1) then picked[0]->IDLgrModel::getProperty, parent = p else picked[0]->getProperty, parent = p
                                    picked[0]->getProperty, parent = p
                                    if ev.x lt 0 then state.oSelected = picked[0] else state.oSelected = p

                                    if (state.oSelected eq p) then begin
                                        state.oModelMan->getProperty, mode = mode
                                        s_objworldNewMode, state, (mode + 1) mod 3
                                    endif

                                    state.oCurrView->getProperty, uValue = view_uval
                                     if ((where(tag_names(ev) eq 'LISTFRAME'))[0]  ne -1) then begin
                                       state.fModelList = where(*(state.oModelListArr[view_uval.num]) eq state.oSelected)
                                     endif else begin
                                        while ((where(*(state.oModelListArr[view_uval.num]) eq p))[0] eq -1) do begin
                                             if (obj_isa(p,'IDLgrModel') eq 1) then p->IDLgrModel::getProperty, parent = p else p->getProperty, parent = p
                                        endwhile
                             state.fModelList = where(*(state.oModelListArr[view_uval.num]) eq p)
                        endelse

                                    state.oSelected->getProperty, uValue = s
                                    if (n_elements(s) eq 0) then s = 'ups'
                                    state.strSelected = 'Current selection:' + s
                                    widget_control, state.wModelDeleteButton, sensitive = 1

                                    widget_control, state.wAddChildButton, sensitive = 1
                                    widget_control, state.wModelUnselectButton, sensitive = 1
                                    widget_control, state.wModelModeRadio, sensitive = 1
                                    widget_control, state.wSaveButton, sensitive = ([1,0])[lmgr(/demo)]

                                    state.oCurrView->getProperty, uValue = view_uval
                                    if (n_elements( *(state.oModelListArr[view_uval.num])) le 2) then widget_control, state.wModelSelectButton, sensitive = 0
                                endelse
                            endif else begin
                                state.oSelected = state.oCurrTopModel
                                state.strSelected = 'CurrentTopModel selected'

                                widget_control, state.wModelDeleteButton, sensitive = 0
                                widget_control, state.wAddChildButton, sensitive = 0
                                widget_control, state.wModelUnselectButton, sensitive = 0
                                widget_control, state.wModelModeRadio, sensitive = 0

                                ; try to change the current view...
                                if ev.x ge 0 then begin
                                    state.oViewMan->setProperty,hide = 1
                                    picked = state.win->select(state.oScene,[ev.x,ev.y])
                                    state.oViewMan->setProperty, hide = 0
                                    si = size(picked)
                                    if (si[0] ne 0) then begin
                                        if (picked[0] ne state.oCurrView) then begin
                                            state.oCurrView = picked[0]
                                            state.oCurrView->getProperty,dim = dim,loc = loc
                                            state.oTrackballMB1->Reset, loc + dim/2.0, dim[0]/2.0
                                            state.oTrackballMB2->Reset, loc + dim/2.0, dim[0]/2.0, mouse = 2
                                            s_objworldGetViewObjs, state.oCurrView, w, b, t
                                            state.oWorldRotModel = w
                                            state.oBasePolygonAtom = b
                                            state.oCurrTopModel = t
                                            state.oSelected = state.oCurrTopModel
                                            state.strSelected = 'New view oSelected'
                                            widget_control, state.wModelDeleteButton, sensitive = 0
                                            widget_control, state.wAddChildButton, sensitive = 0

                                            state.oScene->remove, state.oCurrView
                                            state.oScene->add, state.oCurrView
                                        endif
                                    endif
                                endif
                                state.oCurrView->getProperty, uValue = view_uval
                                if (n_elements( *(state.oModelListArr[view_uval.num])) gt 1) then widget_control, state.wModelSelectButton, sensitive = 1
                                s_objworld_update_wListFrame, state
                            endelse

                            ; point the oModelMan at the node...
                            state.oModelMan->getProperty, target = manip
                            if (manip ne state.oSelected) then state.oModelMan->setTarget, obj_new()
                            if ((state.oSelected ne state.oCurrTopModel) and (state.oSelected ne obj_new())) then begin
                               dummy = state.oSelected
                               while not(obj_isa(dummy,'IDLgrModel')) do dummy->getProperty, parent = dummy
                               state.oSelected = dummy
                               state.oModelMan->setTarget, dummy
                            endif

                            widget_control, state.wTopBase, tlb_set_title = 's_3DView |-> ' + state.path + '...  ||   ' + state.strSelected
                            demo_draw, state.win, state.oScene, debug = state.debug

                            end
                        ev.press eq 2:begin
                               state.win->setProperty, quality = state.fDragQ
                               widget_control,state.wDraw,/draw_motion
                            end
                        (ev.press eq 1) and (state.fTool eq 0):begin
                               if (state.oSelected ne obj_new()) then begin
                                   state.oViewMan->MouseDown, [ev.x,ev.y], state.win
                                   state.btndown = 1b
                                   state.win->setProperty, QUALITY = state.fDragQ
                                   widget_control, state.wDraw, /draw_motion
                                   demo_draw, state.win, state.oScene, debug = state.debug
                                endif
                            end
                        ev.press eq 1:begin
                            state.win->setProperty, quality = state.fDragQ
                            widget_control, state.wDraw, /draw_motion
                            state.btndown = 1b
                            if ((state.oSelected ne state.oCurrTopModel) and  (state.oSelected ne obj_new())) $
                                then state.oModelMan->MouseDown, [ev.x,ev.y], state.win
                            end
                        else:print, 'Ouch!'
                        endcase
                    end

                2:begin ; Button motion.
                    if state.btndown eq 1b then begin
                        case 1 of
                            state.fTool eq 0:begin
                                state.oViewMan->MouseTrack, [ev.x,ev.y], state.win
                                demo_draw, state.win, state.oScene, debug = state.debug
                                state.oCurrView->getProperty, dim = dim, loc = loc
                                state.oTrackballMB1->Reset, loc + dim/2.0, dim[0]/2.0
                                state.oTrackballMB2->Reset, loc + dim/2.0, dim[0]/2.0, mouse = 2
                            end
                            (state.oSelected ne state.oCurrTopModel) and (state.oSelected ne obj_new()):begin
                                state.oModelMan->MouseTrack, [ev.x,ev.y], state.win
                                demo_draw, state.win, state.oScene, debug = state.debug
                            end
                            else:begin
                                ; Rotate.
                                if have_mb1_transform then begin
                                   state.oWorldRotModel->getProperty, transform = t
                                   state.oWorldRotModel->setProperty, transform = t # mb1_transform
                                   demo_draw, state.win, state.oScene, debug = state.debug
                                endif
                            end
                            endcase
                        end
                    end

                1:begin ; Button release.
                    if state.btndown eq 1b then begin
                       case 1 of
                          state.fTool eq 0:state.oViewMan->MouseUp,[ev.x,ev.y],state.win
                          (state.oSelected ne state.oCurrTopModel) and (state.oSelected ne obj_new()):state.oModelMan->MouseUp, [ev.x,ev.y], state.win
                          else:
                       endcase
                    endif
                    state.btndown = 0b
                    state.win->setProperty, QUALITY = 2
                    demo_draw, state.win, state.oScene, debug = state.debug
                    widget_control,state.wDraw, draw_motion = 0
                    end
                else:
                endcase
            widget_control, ev.top, set_uValue = state, /no_copy
        endcase
        'HOTKEY':begin
            widget_control, ev.top, get_uValue = state, /no_copy
            case strupcase(ev.ch) of
                ' ':begin ; 'Next' select.

    ;               Determine how many things there  are to select.
                    if state.fTool eq 1 then begin
                        state.oCurrView->getProperty, uval = view_uval
                        num_selectables = n_elements( *(state.oModelListArr[view_uval.num])  ) - 1 ; Last item on list is obj_new().
                    endif else num_selectables = state.selectedViewCount

    ;               Select something.
                    case 1 of
                        num_selectables gt 1:begin
                            wDraw = state.wDraw
                            widget_control, ev.top, set_uValue = state, /no_copy
                            s_objworldEvent, {id:wDraw, top:ev.top,$
                                                              handler:0L,$
                                                              type:0,$ ; Button down
                                                              press:4,$ ; Right Mouse-button.
                                                              x:-2, y:-2  }
                            widget_control, ev.top, get_uValue = state, /no_copy
                            end
                        num_selectables eq 1:begin
                            if (state.fTool eq 1) and ( state.oSelected eq state.oCurrTopModel) then begin
                                wDraw = state.wDraw
                                widget_control, ev.top, set_uValue = state, /no_copy
                                s_objworldEvent, {id:wDraw, top:ev.top,$
                                                        handler:0L,$
                                                        type:0,$ ; Button down
                                                        press:4,$ ; Right Mouse-button.
                                                        x:-1, y:-1  }
                                widget_control, ev.top, get_uValue = state, /no_copy
                           endif
                        endcase
                        else:
                        endcase
                    end
                'S':begin ; Scale.
                       if state.fTool eq 1 then begin
                           s_objworldNewMode, state, 2
                           demo_draw, state.win, state.oScene, debug = state.debug
                       endif
                    end
                'R':begin ; Rotate.
                       if state.fTool eq 1 then begin
                           s_objworldNewMode, state, 1
                           demo_draw, state.win, state.oScene, debug = state.debug
                       endif
                    end
                'T':begin ; Translate.
                       if state.fTool eq 1 then begin
                           s_objworldNewMode, state, 0
                           demo_draw, state.win, state.oScene, debug = state.debug
                       endif
                    end
                'U':begin ; Unselect
                       if state.fTool eq 1 then begin
                           wModelUnselectButton = state.wModelUnselectButton
                           widget_control, ev.top, set_uValue = state, /no_copy
                           s_objworldEvent, {top:ev.top, handler:0L, id:wModelUnselectButton }
                           widget_control, ev.top, get_uValue = state, /no_copy
                       endif
                    end
                'D':begin ; Delete
                       wDel = state.wModelDeleteButton
                       widget_control, ev.top, set_uValue = state, /no_copy
                       s_objworldEvent, {top:ev.top, handler:0L, id:wDel }
                       widget_control, ev.top, get_uValue = state, /no_copy
                    end
                'V':begin ; Toggle Manipulate Views mode
                    if (state.fTool eq 0) then wTool = state.wToolModel
                    if (state.fTool eq 1) then wTool = state.wToolView
                    widget_control, ev.top, set_uValue = state, /no_copy
                       s_objworldEvent, {top:ev.top, handler:0L, id:wTool }
                    widget_control, ev.top, get_uValue = state, /no_copy
                    end
                else:
                endcase
            widget_control, ev.top, set_uValue = state, /no_copy
            end
        endcase
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, state.wHotKeyReceptor, /input_focus
    widget_control, ev.top, set_uValue = state, /no_copy
end

;_BEGIN_LIST_FUNCTIONS_____________________________________________________________________
pro s_objworld_wModelListEvent, ev
    widget_control, ev.top, get_uValue = state, /no_copy
    widget_control, ev.id, get_uValue = uValue, /no_copy
    case uValue.name of
       'ListFile':begin
                    uValue.active = ev.index
                    if ((*uValue.value)[ev.index] ne '-NO FILES-') then begin
                                endif
          endcase
       'ListFrame':begin
                    uValue.active = ev.index
                    if ((*uValue.value)[ev.index] ne '-NO MODEL-') then begin
                                   wDraw = state.wDraw
                                   widget_control, ev.top, set_uValue = state, /no_copy
                                   s_objworldEvent, {id:wDraw, top:ev.top,$
                                                           handler:0L,$
                                                           type:0,$ ; Button down
                                                           press:4,$ ; Right Mouse-button.
                                                      ListFrame:ev.index,$ ; List Object seleted
                                                           x:-1, y:-1  }
                                   widget_control, ev.top, get_uValue = state, /no_copy
                                   widget_control, ev.id, set_uValue = uValue, /no_copy
                                     s_objworld_update_wListObject, state
                                   widget_control, ev.id, get_uValue = uValue, /no_copy
                                endif
          endcase
       'ListObject':begin
                    if ((*uValue.value)[ev.index] eq 'AS CHILD:') then uValue.active = ev.index + 1 else uValue.active = ev.index
                    if ((*uValue.value)[ev.index] ne '-NO CHILD-') then begin
                                   wDraw = state.wDraw
                                   widget_control, ev.top, set_uValue = state, /no_copy
                                   s_objworldEvent, {id:wDraw, top:ev.top,$
                                                           handler:0L,$
                                                           type:0,$ ; Button down
                                                           press:4,$ ; Right Mouse-button.
                                                      ListObject:uValue.active,$ ; List Object seleted
                                                           x:-1, y:-1  }
                                   widget_control, ev.top, get_uValue = state, /no_copy
                              endif
          endcase
       'ListParameter':begin
                    uValue.active = ev.index
                    if ((*uValue.value)[ev.index] ne '-NO PARAMS-') then begin
                                endif
          endcase
    endcase
    widget_control, ev.id, set_uValue = uValue, /no_copy
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_objworld_update_wListObject, state
    widget_control, state.wListFrame, get_uValue = uValue, /no_copy
        state.oCurrView->getProperty, uValue = view_uval
        if (obj_valid((*(state.oModelListArr[view_uval.num]))[uValue.active])) then $
         oModels = (*(state.oModelListArr[view_uval.num]))[uValue.active]->get(/all) $
         else oModels = state.oSelected->get(/all)
    widget_control,state.wListFrame, set_uValue = uValue, /no_copy

    if ((n_elements(oModels) gt 0) and (obj_valid(oModels[0]))) then begin
       for i = 0,n_elements(oModels)-1 do begin
         if not(obj_isa(oModels[i], 'IDLEXMODELMANIP')) then begin
          oModels[i]->getProperty, uValue = dummy
          if (n_elements(dummy) ne 0) then $
              if (n_elements(modelList) eq 0) then modelList = dummy else modelList = [modelList, dummy]
         endif
       endfor
    endif
    if  (n_elements(modelList) lt 1) then modelList = '-NO CHILD-'

    widget_control, state.wListObject, get_uValue = uValue, /no_copy
       if (uValue.active gt (n_elements(oModels)-1)) then uValue.active = 0
       widget_control, state.wListObject, set_value = modelList, set_list_select = uValue.active
       if ptr_valid(uValue.value) then ptr_free, uValue.value
       uValue.value = ptr_new(modelList, /no_copy)
    widget_control, state.wListObject, set_uValue = uValue, /no_copy

    widget_control, state.wViewControlBase, /update
end


pro s_objworld_update_wListFrame, state
    state.oCurrView->getProperty, uValue = view_uval
    nModels = n_elements(*(state.oModelListArr[view_uval.num]))-1
    if (nModels ge 1) then begin
       ((*(state.oModelListArr[ view_uval.num]))[0])->getProperty, uValue = dummy
       ListFrame = dummy
       for i = 1, nModels-1 do begin
             ((*(state.oModelListArr[view_uval.num]))[ i ])->getProperty, uValue = dummy
             ListFrame = [ListFrame, dummy]
       endfor
    endif

    if (nModels ge 0) then begin
       widget_control, state.wListFrame, get_uValue = uValue, /no_copy
         if (nModels eq 0) then begin
           ListFrame = '-NO MODEL-'
           uValue.active = 0
         endif
         if (uValue.active gt (nModels-1)) then uValue.active = 0

       widget_control, state.wListFrame, set_value = ListFrame, set_list_select = uValue.active
         if ptr_valid(uValue.value) then ptr_free, uValue.value
         uValue.value = ptr_new(ListFrame, /no_copy)
       widget_control,state.wListFrame, set_uValue = uValue, /no_copy
       s_objworld_update_wListObject, state
    endif
end


function s_objworld_findAndSetObj_wListFrame, state, object
    state.oCurrView->getProperty, uValue = view_uval
    nModels = n_elements(*(state.oModelListArr[view_uval.num]))-1
    if (nModels ge 1) then begin
       ((*(state.oModelListArr[ view_uval.num]))[0])->getProperty, uValue = dummy
       ListFrame = dummy
       for i = 1, nModels-1 do begin
             ((*(state.oModelListArr[view_uval.num]))[ i ])->getProperty, uValue = dummy
             ListFrame = [ListFrame, dummy]
       endfor
    endif
end


pro s_objworld, path = path,$
                basePosition = basePosition,$
                groupLeader = groupLeader,$               ; IN:(opt) group identifier
                debug = debug,$                            ; IN:(opt) debug flag
                init_oModel = init_oModel,$                ; IN:initialize with init_oModel
                application_TLB = application_TLB        ; OUT:(opt) TLB of this application

    if (n_elements(basePosition) eq 0) then basePosition = [0,0]
    widget_control, /hourglass
    addable_subjects = ['Movie_Single_Hist_Plot',$
                        'Movie_Mixed_Plot',$
                        'Movie_Info_Text',$
                        'Movie_Dim_Text',$
                        'Sphere',$
                        'Cube',$
                        'Tetrahedron',$
                        'Plane',$
                        'Cone',$
                        'Green_Light',$
                        'Surface',$
                        'White_Light',$
                        'Plot',$
                        'Single Plot',$
                        'Ribbon_Plot',$
                        'Bar_Plot',$
                        '4x4Mesh',$
                        'Textured_Surface']

    if n_elements(path) eq 0 then path = s_getPathForSystem()

    ;Check the validity of the group identifier.
    ngroup = n_elements(groupLeader)
    if (ngroup ne 0) then begin
        check = widget_info(groupLeader, /valid_id)
        if (check ne 1) then begin
            print,'Error:the group identifier is not valid.'
            print,'Returning to the main application.'
            return
        endif
    endif else groupLeader = 0l

    ;Get the screen size & Set up dimensions of the drawing (viewing) area.
    device, get_screen_size = screensize
    xdim = screensize[0]*0.6
    ydim = xdim/1.25

    ;Get the current color vectors to restore when this application is exited. & Build color table from color vectors
    tvlct, savedr, savedg, savedb, /get
    colortable = [[savedr],[savedg],[savedb]]

    ;_________________________________________;Create widgets.
    wTopBase = widget_base( title = 's_3DView |-> ',$
                            xpad = 2,$
                            ypad = 2,$
                            /tlb_kill_request_events,$
                            tlb_frame_attr = 8,$
                            xOffset = basePosition[0], yOffset = basePosition[1],$
                            tlb_size_events = 1,$
                            mbar = barbase,$
                            group_leader = groupLeader,$
                            /column)

    application_TLB = wTopBase ; Return parameter.
    widget_control, wTopBase, set_uname = 's_objworld:tlb'

    ;_________________________________________Create the menu bar.
    ;Options menu.
    wOptionsButton = widget_button(barbase, value = 'Options', /menu)
        wDragQ = widget_button(wOptionsButton, /menu, value = 'Drag Quality')
            wDragQLow = widget_button( wDragQ, value = 'Low', uval = 'DRAGQLOW' )
            wDragQMedium = widget_button(  wDragQ, value = 'Medium', uval = 'DRAGQMEDIUM', sensitive = 0 )
            wDragQHigh = widget_button( wDragQ, value = 'High', uval = 'DRAGQHIGH' )
            fDragQ = 1
        void = widget_button(wOptionsButton, value = 'Anti-Alias',uval = 'ANTIALIAS')
        wPrintButton = widget_button( wOptionsButton, value = 'Print', uval = 'PRINT', /sep)
        wVRMLButton = widget_button( wOptionsButton, value = 'Export VRML', uval = 'VRML' )
        wClipboardButton = widget_button(  wOptionsButton, value = 'Copy to Clipboard', uval = 'CLIPBOARD' )
        wGridButton = widget_button( wOptionsButton, value = 'Show Grid (on )', uval = 'GRID(ONOFF)', /sep)
        wBACKColorButton = widget_button( wOptionsButton, value = 'BackGround Color (off)', uval = 'BACKGROUND(ONOFF)', /sep)
        wCenterButton = widget_button( wOptionsButton, value = 'Self Centric View (off)', uval = 'CENTRED(ONOFF)', /sep)
    ;Tools menu.
    wToolButton = widget_button(barbase, value = 'Tools', /menu)
        void = widget_button( wToolButton, value = 'Make Rotation Movie', uval = 'MAKEROTATIONMOVIE')
        wToolView = widget_button( wToolButton, value = 'View', uval = 'TOOLVIEW')
        wToolModel = widget_button( wToolButton, value = 'Model', uval = 'TOOLMODEL', sensitive = 0 )
        fTool = 1

    ;Create the menu bar item help that contains the about button.
    wHelpButton = widget_button(barbase, value = 'About', /help, /menu)
    waboutbutton = widget_button(wHelpButton, value = 'About Object World', uValue = 'ABOUT')

    ;_________________________________________Create Main Window
    wTopRowBase = widget_base(wTopBase,/row,/frame)
        wGuiBase = widget_base(wTopRowBase, /column )

            wStackerBase = widget_base(wGuiBase, xpad = 0, ypad = 0)
                wViewControlBase = widget_base(wStackerBase, xpad = 0, ypad = 0, /column, MAP = 0, /frame)
                    void = widget_button( wViewControlBase, value = 'Add View', uname = 's_objworld:addview', uValue = 'ADDVIEW' )
                    wViewDeleteButton = widget_button(  wViewControlBase, value = 'Delete', uname = 's_objworld:viewdelete', uval = 'DEL', sensitive = 0)
                    wViewSelectButton = widget_button( wViewControlBase, value = 'Select', uname = 's_objworld:viewselect', uval = 'VIEWSELECT', sensitive = 0)

                wModelControlBase = widget_base(wStackerBase, xpad = 0, ypad = 0, /column)
                 void = widget_button(wModelControlBase, Value = 'Files', /Menu)
                     wLoadButton = widget_button( void, value = 'Load', uval = 'LOAD' )
                         wFileSelectButton = widget_button( void, value = 'Select', uValue = 'FILESELECT', /sep, sensitive = 0 )
                         wFileUnselectButton = widget_button( void, value = 'Unselect', uValue = 'FILEUNSELECT', sensitive = 0 )
                         wFileDeleteButton = widget_button( void, value = 'Delete', uval = 'FILEDELETE', /sep, sensitive = 0 )
                 wListFile = widget_list(wModelControlBase, event_pro = 's_objworld_wModelListEvent', xsize = 9, ysize = 3, value = ['-NO FILES-' ],$
                         uValue = {name:'ListFile', value:ptr_new(['-NO FILES-'], /no_copy), active:0 }, kill_notify = 'CleanList', /multiple)

                 void = widget_button(wModelControlBase, value = 'Models', /menu)
                   wAddButton = widget_button(void, value = 'Add', /menu)
                         for i = 0,n_elements(addable_subjects)-1 do dummy = widget_button(wAddButton,$
                                                                              value = addable_subjects[i],$
                                                                              uname = 's_objworld:add' + addable_subjects[i],$
                                                                              uValue = ['ADD', addable_subjects[i]] )
                   wAddChildButton = widget_button(void, value = 'Add Child', /menu )
                         for i = 0,n_elements(addable_subjects)-1 do dummy = widget_button(wAddChildButton,$
                                                                              value = addable_subjects[i],$
                                                                              uname = 's_objworld:addchild' + addable_subjects[i],$
                                                                              uValue = ['ADDCHILD', addable_subjects[i]] )
                         wModelSelectButton = widget_button( void, value = 'Select', uname = 's_objworld:modelselect', uValue = 'MODELSELECT', /sep)
                         wModelUnselectButton = widget_button( void, value = 'Unselect', uname = 's_objworld:unselect', uValue = 'MODELUNSELECT' )
                     wSaveButton = widget_button( void, value = 'Save selection', uval = 'SAVE' )
                 wListFrame = widget_list(wModelControlBase, event_pro = 's_objworld_wModelListEvent', xsize = 12, ysize = 5, value = '', uValue = {name:'ListFrame', value:ptr_new(), active:0 }, kill_notify = 'CleanList')
                 wListObject = widget_list(wModelControlBase, event_pro = 's_objworld_wModelListEvent', xsize = 12, ysize = 5, value = '', uValue = {name:'ListObject', value:ptr_new(), active:0 }, kill_notify = 'CleanList')
                 wModelDeleteButton = widget_button(wModelControlBase, value = 'Delete', uname = 's_objworld:delete', uval = 'DEL')


                 wModelModeRadio = cw_bgroup(wModelControlBase, ['Translate', 'Rotate', 'Scale'], /exclusive, /no_release, set_value = 0, uValue = 'MODELMODE', uname = 's_objworld:modelmoderadio')

                 void = cw_fSlider(wModelControlBase, minimum = 0.1, maximum = 10., value = 1, scroll = 1, /edit, title = 'scale factor', uval = 'SLIDERSCALE')
                 void = cw_fSlider(wModelControlBase, minimum = 0., maximum = 360., value = 320, scroll = 1, /edit, uval = 'SLIDERALPHA')
                 void = cw_fSlider(wModelControlBase, minimum = 0., maximum = 360., value = 20, scroll = 1, /edit, uval = 'SLIDERBETA')
                 void = cw_fSlider(wModelControlBase, minimum = 0., maximum = 360., value = 20, scroll = 1, /edit, title = 'xyz-axis rotation', uval = 'SLIDERGAMMA')


        wStackerBase = widget_base(wTopRowBase, xpad = 0, ypad = 0, frame = 1)
            wDraw = widget_draw(wStackerBase, xsize = xdim,$
                                              ysize = ydim,$
                                              /button_ev,$
                                              uval = 'DRAW',$
                                              retain = 2,$
                                              /expose_ev,$
                                              uname = 's_objworld:draw',$
                                              graphics_level = 2 )
            wHotKeyReceptor = widget_text(wStackerBase, /all_events, uValue = 'HOTKEY', uname = 's_objworld:hotkey' )

    ;Realize the base widget & Get the window id of the drawable.
    widget_control, wTopBase, /realize
    widget_control, wdraw, get_value = win

    ;Build the oScene.
    oCurrView = s_objworldMakeView(xdim, ydim, {name:'ObjView', num:0})
    oCurrView->getProperty, dim = dim, loc = loc
    oScene = obj_new('IDLgrScene')
    oScene->add, oCurrView
    s_objworldGetViewObjs, oCurrView, oWorldRotModel, oBasePolygonAtom, oCurrTopModel

       ; make a font for the demo.
    thefont = [obj_new('IDLgrFont','times',size = 30), obj_new('IDLgrFont','hershey*3',size = 9),$
               obj_new('IDLgrFont','helvetica',size = 40), obj_new('IDLgrFont','helvetica',size = 12)]

       ; define basic model matrix
    worldRotModelMatrix = [[1., 0., 0., 0.],$
                           [0., 1., 0., 0.],$
                           [0., 0., 1., 0.],$
                           [0., 0., 0., 1.]]

    state = {addable_subjects:addable_subjects,$
         btndown:0b,$
         thefont:thefont,$
         pt0:fltarr(3),$
         pt1:fltarr(3),$
         oBasePolygonAtom:oBasePolygonAtom,$    ; Basic Poygon Graphic Atom
         oModelMan:obj_new('IDLexModelManip', translate = [1,1,1], selector_color = [255,255,255], manipulator_color = [255, 60, 60] ),$
         oViewMan:obj_new('IDLexViewManip', color = [255, 0, 0] ),$
         oTrackballMB1:obj_new('trackball', (loc + dim/2.0), dim[0] / 2.0 ),$
         oTrackballMB2:obj_new('trackball', (loc + dim/2.0), dim[0] / 2.0, mouse = 2b ),$
         oCurrTopModel:oCurrTopModel,$
         oSelected:oCurrTopModel,$
         strSelected:'CurrentTopModel selected',$
         oModelListArr:ptrarr(50),$ ; One list for each view.
         fModelList:1,$ ; Flag on Model List
         fSubModel:0,$ ; Flag on Sub Models of Model
         oWorldRotModel:oWorldRotModel,$
         worldRotModelMatrix:worldRotModelMatrix,$
         worldMatrixState:[320.,20.,20.,1.],$; alpha, beta, gamma, scale
         oCurrView:oCurrView,$
         oScene:oScene,$
         selectedViewCount:1,$; Selected View Window Number.
         highestViewCount:1,$; Highest Selectable View Window Number.
         win:win,$
         path:path,$
         groupLeader:groupLeader,$
         child_ImageStackManipulator_TLB:-1l,$
         debug:keyWord_set(debug),$
         colortable:colortable,$
         wTopBase:wTopBase,$
         wTopRowBase:wTopRowBase,$
         wViewControlBase:wViewControlBase,$
         wViewSelectButton:wViewSelectButton,$
         wViewDeleteButton:wViewDeleteButton,$
         wModelControlBase:wModelControlBase,$
         wModelDeleteButton:wModelDeleteButton,$
         wModelSelectButton:wModelSelectButton,$
         wModelModeRadio:wModelModeRadio,$
         wAddChildButton:wAddChildButton,$
         wModelUnselectButton:wModelUnselectButton,$
         wDragQLow:wDragQLow,$
         wDragQMedium:wDragQMedium,$
         wDragQHigh:wDragQHigh,$
           fDragQ:fDragQ,$                 ; [0:Low, 1:Medium, 2:High]
         wToolButton:wToolButton,$
         wToolView:wToolView,$
         wToolModel:wToolModel,$
           fTool:fTool,$          ; [0:Tool select:Model, 1:Tool select:View]
         wGridButton:wGridButton,$
         wBACKColorButton: wBACKColorButton,$
            fBACKColorButton: 0,$
         wCenterButton: wCenterButton,$
            fCenterButton: 0,$
         wHotKeyReceptor:wHotKeyReceptor,$
         wLoadButton:wLoadButton,$
         wSaveButton:wSaveButton,$
         wFileSelectButton:wFileSelectButton,$
         wFileUnselectButton:wFileUnselectButton,$
         wFileDeleteButton:wFileDeleteButton,$
         wListFile:wListFile,$
         wListFrame:wListFrame,$
         wListObject:wListObject,$
         wDraw:wDraw}

    ;state.oBasePolygonAtom->setProperty, hide = 1 - s_ToggleButtonOnOffState(state.wGridButton)
    ;Add some rotation on the whole thing.
    state.oWorldRotModel->rotate, [1,0,0], state.worldMatrixState[0]
    state.oWorldRotModel->rotate, [0,1,0], state.worldMatrixState[1]
    state.oWorldRotModel->rotate, [0,0,1], state.worldMatrixState[2]

    ; Placeholder NULL at end of each list.
    state.oModelListArr[0] = ptr_new(obj_new())

    if keyWord_set(init_oModel) then begin
       s_objworldAdd, state, init_oModel
       s_objworld_update_wListFrame, state
    endif else begin
       ;Add our restored objects to array of selectable objects.
       restore, demo_filepath('objw_surf.sav', subdir = ['examples','demo','demodata']  ), /relaxed_structure_assignment
       tmp_obj->translate, 0, 0, .001, /premultiply
       s_objworldAdd, state, tmp_obj
    endelse

    widget_control, wTopBase, set_uValue = state, /no_copy
    xmanager, 's_objworld', wTopBase, event_handler = 's_objworldevent', /no_block, cleanup = 's_objworldcleanup', group_leader = groupLeader
end



