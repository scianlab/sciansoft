;_____________________________IOISIOI____________________
; NAME:
;      s_SaveViewforModel
;
; PURPOSE:
;  only for save one image of current view for a selected model
; AUTHOR:
;   Felipe Santibañez (2011)
;   e_mail: fsantibanez@med.uchile.cl
;
; CALLING SEQUENCE:
;       s_SaveViewforModel,
;
; 
;_____________________________IOISIOI____________________

pro s_WorldViewforModel, xdim = xdim, ydim = ydim ,win = win,oClipboard = oClipboard, oScene = oScene, oCurrTopModel = oCurrTopModel, oWorldRotModel = oWorldRotModel, oModel = oModel, oStaticInputModel =oStaticInputModel
;    device, get_screen_size = screensize
;    xdim = screensize[1]*1.0 ; 0.9
;    ydim = xdim

    ;_________________________________________;Create widget
    wTopBase = widget_base( title = 's_3DViewModel |-> ')

            wDraw = widget_draw(wTopBase, xsize = xdim,$
                                              ysize = ydim,$
                                              /button_ev,$
                                              uval = 'DRAW',$
                                              retain = 2,$
                                              /expose_ev,$
                                              uname = 's_objworld:draw',$
                                              graphics_level = 2 )

    ;Realize the base widget & Get the window id of the drawable.
    widget_control, wTopBase, /realize
    widget_control, wdraw, get_value = win
    
    win->getProperty, dimension = mainDims, resolution = mainRes, color_model = cm, n_colors = icolors

    oClipboard = obj_new('IDLgrClipboard', dimensions = mainDims, resolution = mainRes, color_model = cm, n_colors = icolors)

    ;Build the oScene.
    oCurrView = s_objworldMakeViewModel(xdim, ydim, {name:'ObjView', num:0})

    oCurrView->getProperty, dim = dim, loc = loc
    oScene = obj_new('IDLgrScene')
    oScene->add, oCurrView
    s_objworldGetViewObjsModel, oCurrView, oWorldRotModel, oBasePolygonAtom, oCurrTopModel,oStaticModel

   oCurrTopModel->add, oModel    
   oStaticModel->add, oStaticInputModel
       ; define basic model matrix
    worldRotModelMatrix = [[1., 0., 0., 0.],$
                           [0., 1., 0., 0.],$
                           [0., 0., 1., 0.],$
                           [0., 0., 0., 1.]]
    ; el ultimo factor amplifica o achica la visualizacion---- 
    worldMatrixState = [0.,0.,00.,3.5]; 
    ;worldMatrixState = [0.,0.,00.,2.0*3.0];
    
    ;Add some rotation on the whole thing.
    oWorldRotModel->rotate, [1,0,0], worldMatrixState[0]
    oWorldRotModel->rotate, [0,1,0], worldMatrixState[1]
    oWorldRotModel->rotate, [0,0,1], worldMatrixState[2]
   
    oWorldRotModel->getProperty, transform = t
    t[3,3] = 1/worldMatrixState[3]
    oWorldRotModel->setProperty, transform = t
end

pro s_SaveViewforModel,win = win,oClipboard = oClipboard, oScene = oScene, time = time, vPath = vPath, modelName = modelName

     if(N_ELEMENTS(modelName) eq 0) then modelName = '_Tracking' ; default value XD 
     timeStr = strCompress(string(time))
     for j = strLen(timeStr), 4 do timeStr = strCompress('0'+timeStr, /rem)
     if(N_ELEMENTS(vPath) gt 0) then begin
        if(FILE_TEST(strCompress(vPath+modelName+ path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+modelName + path_sep())
        if(FILE_TEST(strCompress(vPath+modelName + path_sep()+'_Movies' + path_sep()),/DIRECTORY) eq 0b) then  FILE_MKDIR, strCompress(vPath+modelName + path_sep()+'_Movies' + path_sep())
 
        fileName = strCompress(vPath+strcompress(modelName + path_sep()+'_Movies' + path_sep()+'3DModelView_' +string(timeStr) +'.tif', /rem))
     endif else fileName = strCompress(s_getPathForSystem() + strcompress('3DModelView_' +string(timeStr) +'.tif', /rem))
     oAlias = 1

   ;oModel->translate, 0, 0, .001, /premultiply

   ; demo_draw, win, oScene
   ;s_objworldSceneAntiAlias, win, oScene, 8, oAlias = oAlias
   ;oClipboard->draw, oAlias, fileName = fileName
   ;obj_destroy, oAlias
   demo_draw, win, oScene
   oClipboard->draw, oScene, fileName = fileName
end

function s_objworldMakeViewModel, xdim, ydim, uval
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
  ;colorFondo = [255,255,255]
; colorFondo = [80,80,158]
; Projection = 1.. orthogonal... 2= perspective
    oView = obj_new('IDLgrView', projection = 1, eye = 3000, zclip = [2000.,-2000.], dim = [xdim, ydim],$
                                 viewplane_rect = myview, color = colorFondo, uValue = uval )
    ;Create rotating model.
    oAtom = obj_new('IDLgrModel')
    oModelView = obj_new('IDLgrModel')
    oModelView->add, oAtom
    oModelView->setProperty, uValue = 'InitialModel'
    
    ; Atom pols...
    oAtom->add, obj_new('IDLgrPolygon')
    ; Define the object tree add point.
    oAtom->add, obj_new('IDLgrModel')
    ; Define static model
    oAtom->add, obj_new('IDLgrModel')

    ;Add some lights.
    oModelView->add, obj_new('IDLgrLight', loc = [2,2,5], type = 2, color = [255,255,255], intensity = .5 )
    oModelView->add, obj_new('IDLgrLight', type = 0, intensity = .5, color = [255,255,255] )
    ;Place the model in the view.
    oView->add, oModelView
    
    return, oView
end

pro s_objworldGetViewObjsModel, view, oWorldRotModel, oBasePolygonAtom, model_top,model_static
       ;Luckily, this portion of the hierarchy is fixed.
    gg = view->get()
    oWorldRotModel   = gg->get(pos = 0)
    oBasePolygonAtom = oWorldRotModel->get(pos = 0)
    model_static     = oWorldRotModel->get(pos = 2)
    model_top        = oWorldRotModel->get(pos = 1)    
end