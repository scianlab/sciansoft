;_____________________________IOISIOI____________________
; NAME:
;      s_ROI_ParameterManipulator_Window
;
; PURPOSE:
;       - Visualizes and Processes Multi-Dimensional-Data
;
; AUTHOR:
;     Dr. Steffen Härtel (2003)
;     e_mail: shaertel@physik.uni-bremen.de
;
;_____________________________IOISIOI____________________

pro s_ROIPM_InitializeWidgets, wTopBase
    widget_control, wTopBase, get_uValue = state, /no_copy
       if widget_info(state.roiom_tlb, /valid) then begin
         widget_control, state.roiom_tlb, get_uValue = roiomState
          state.stack_tlb = roiomState.stack_tlb
         widget_control, state.roiom_tlb, set_uValue = roiomState

          ; get selected ImagePosition and selected cluster name
         s_ISM_getProjectInfo, stack_tlb = state.stack_tlb,$
                                                   tPos = tPos,$
                                                   selClusName = selClusName
          ; get ImageStackInfoObject
         widget_control, state.stack_tlb, get_uValue = stackState, /no_copy
          ImageStackInfoObject = *stackState.pImageStackInfoObject
         widget_control, state.stack_tlb, set_uValue = stackState, /no_copy

          ; get selected parameter path and image name
         ImageStackInfoObject->get, pParamStruct = pParamStruct
         parameterPath = *(*pParamStruct).pValues[(where(*(*pParamStruct).pNames eq 'Parameter Path'))[0]]

       endif else begin
          state.stack_tlb = -1
          state.parameterPath = s_getPathForSystem()
       endelse
    widget_control, wTopBase, set_uValue = state, /no_copy
end


pro s_ROIPM_Cleanup, wTopBase
        ; Come here when program dies. Free all created objects and pointers.
    widget_control, wTopBase, get_uValue = state, /no_copy

    if (n_elements(state) ne 0) then begin
       if obj_valid(state.oParamListContainer) then obj_destroy, state.oParamListContainer
       if widget_info(state.groupLeader, /valid_id) then begin
         widget_control, state.groupLeader, get_uValue = stateParent, /no_copy
         if (n_elements(stateParent) ne 0) then begin
          if stateParent.fROIParamWindow then stateParent.fROIParamWindow = s_ToggleButtonOnOffState(stateParent.wROIParamWindowOnOff)
          stateParent.child_ROIParamWindow_tlb = -1L
          widget_control, state.groupLeader, set_uValue = stateParent, /no_copy
         endif
       endif
    endif
end


pro s_ROI_ParameterManipulator_Window, groupLeader = groupLeader,$
                                       basePosition = basePosition,$
                                       application_tlb = application_tlb

    common  shareVar, clus_image, clus_manage, clus_limits, clus_where, clus_plotlimit, clus_win, clus_g_style, allcl_parameter,$
            allcl_lenght, allcl_dimension, clus_name , clus_base, clus_active, clus_perwin, clus_shade, clusTime


    ;  set variables to zero
    allcl_dimension = 0
    allcl_parameter = 0
    allcl_lenght = 0

       ;Check the validity of the group identifier.
    ngroup = n_elements(groupLeader)
    if (ngroup ne 0) then begin
       if (widget_info(groupLeader, /valid_id) ne 1) then begin
          print,'error: the group identifier is not valid.  |--> Returning to the main application.'
          return
       endif
    endif else groupLeader = 0l

    if (not(keyWord_set(basePosition))) then basePosition = [0,0]

    clusTime = lonArr(10)
    device, decomposed = 0
    tvlct, r, g, b, /get
    old_color = [transpose(r),transpose(g), transpose(b)]
    loadct, 3
    tvlct, r, g, b, /get
    r = [0, 1, 1, 0, 0, 1]       ;Specify the red component,..
    g = [0, 1, 0, 1, 0, 1]     ;green component, and..
    b = [0, 1, 0, 0, 1, 0]     ;blue component of each color.
    tvlct, 255*r, 255*g, 255*b ; color 0/BLACK 1/WHITE 2/RED 3/GREEN 4/BLUE 5/YELLOW
    tvlct, r, g, b, /get
    plot_color = [transpose(r),transpose(g), transpose(b)]
    tvlct,  old_color[0,*], old_color[1,*], old_color[2,*]

    allParamList = ['-NO SELECTION-']
    clus_limits = fltArr(n_elements(allParamList),6); Limits (#Parameter,MINP,min1/min2/max1/max2,MAXP)
    clus_plotlimit = fltArr(n_elements(allParamList),2)
    clus_shade = bytArr(1,1)

    clus_manage = lonArr(24)
    clus_manage[0] = 0          ;0: 1-Hist           [1]ON/[0]OFF
    clus_manage[1] = 0          ;1: 2-D Histogramm:   [1]ON/[0]OFF
    clus_manage[2] = 0          ;2: min/max-Celle       [1]ON/[0]OFF
    clus_manage[3] = 0        ;3: 2-D Fläche         [1]ON/[0]OFF
    clus_manage[4] = 0          ;4: 3-D Plot        [1]ON/[0]OFF
    clus_manage[5] = 50          ;5: X-Slider-INTERVALL
    clus_manage[6] = 50          ;6: Y-Slider-INTERVALL
    clus_manage[7] = 50          ;7: z-Slider-INTERVALL
    clus_manage[8] = -1          ;8: X-Achsen_Parameter
    clus_manage[9] = -1          ;9: Y-Achsen_Parameter
    clus_manage[10] = -1       ;10:z-Achsen_Parameter
    clus_manage[11] = 0          ;11: out_text in clus_base[0]
    clus_manage[12] = 30      ;12: X-ROT
    clus_manage[13] = 30      ;13: z-ROT
    clus_manage[14] = 0          ;14: # Parameterschaar
    clus_manage[15] = -1      ;15: active parameter
    clus_manage[16] = 0          ;16: statistic parameter /0_Slider, /1_Hand, /2_Density, /3_Alpha, /4_Neural, /5_K-Means
    clus_manage[17] = 0          ;   -void-
    clus_manage[18] = 1          ;18: symsize for 2D_plot (passive cell population)
    clus_manage[19] = 1          ;19: symsize for 2D_plot (active cell population)
    clus_manage[20] = 1          ;20: ACTIVE AREA (n-DIM-VOXEL) of CHOICE (1-9)
    clus_manage[21] = 0          ;CLUSTER_plot    [1]ON/[0]OFF
    clus_manage[22] = 0          ;Statistics:      [0]total/[1]
    clus_manage[23] = 0          ;Keep_Limits:    [0]OFF/[1]ON


    clus_g_style = bytArr(20)
    clus_g_style[0] = 0         ; XLOGONOFF   [1]ON/[0]OFF
    clus_g_style[1] = 0         ; Y-LOG   [1]ON/[0]OFF
    clus_g_style[2] = 0         ; z-LOG   [1]ON/[0]OFF
    clus_g_style[3] = 0         ; XYz-STYLE  [0]NORMAL [1]AREA_NORM [2]PEAK_NORM
    clus_g_style[4] = 0         ; XYz-STYLE  [0]color [1]lineStyle
    clus_g_style[5] = 0         ; XYz-STYLE  SMOOTHvalueS (0,2,3,5,10,15,30,50,100)
    clus_g_style[6] = 1         ; [1] HISTMODE / [0] LINEMODE
    clus_g_style[7] = 1         ; STATMODE  [1]ON/[0]OFF
    clus_g_style[8] = 0         ; plotFORM   [0]PLOTACTIVEANDPASSIVE [1]PLOTALLINONE  [2]PLOTCOLOR
    clus_g_style[9] = 0         ; INTERPOLATE TO ELIMINATE 0-VAUES     [1]ON/[0]OFF
    clus_g_style[10] = 0     ; FIT_MODE     [0]OFF [1] gaussFit [2] DOUBLE_GAUSS_FIT (3)GrowTH_FIT
    clus_g_style[11] = 0     ; FIT_ONLY     [1]ON /[0]OFF
    clus_g_style[12] = 0     ; CLUSTER       [0]# of cells/ [1]# of clusters
    clus_g_style[13] = 0     ; CLUSTER       [0] Min_Max Individual  / [1] Min_Max All
    clus_g_style[14] = 0     ; CLUSTER:1_2CORRECT     [0] Off / [1] On
    clus_g_style[15] = 0     ; 2D_plotSTYLE:  [0] Pixeldiagramm, [1] Contour_closed, [2] Contour_notclosed , (3) Filled Contours, (4) FILLED IMAGE
    clus_g_style[16] = 0    ; plot_SEPARATE:  [0] OFF,  [1] SEPARATE_2D, [2]  SEPARATE_3D   ,- separation capacity for publication in Aqua-science

    clus_base = lonArr(9)
    clus_base[0] = -1   ; Main-widget
    clus_base[1] = -1   ; Pop-Window D1-XYz-Histogramm
    clus_base[2] = -1   ; Parameter-Table
    clus_base[3] = -1   ; Pop-Window D2-Histogramm
    clus_base[4] = -1   ; Pop-Window D2-Surface
    clus_base[5] = -1   ; Pop-Window D3-Histogramm
    clus_base[6] = -1   ; Cluster-Window
    clus_base[7] = -1   ; Cluster-SLIDER-Window
    clus_base[8] = -1   ; Adjust-SLIDER-Window

    clus_win = intArr(16)
    clus_win[0] = !D.WINDOW    ;0: old_window
    clus_win[1] = 0         ;1: Pop-Window D2-Histogramm
    clus_win[2] = 0         ;2: Pop-Window D2-Surface
    clus_win[3] = 0         ;3: Pop-Window D3-Histogramm
    clus_win[4:6] = 0   ;4-6: D1-x/Y/Z Window
    clus_win[7:15] = 0     ;7-15: Cluster_Windows

    wTopBase = widget_base( title = 's_Param |->',$
                           xpad = 2,$
                           ypad = 2,$
                           xOffset = basePosition[0],$
                           MBar = menuBase,$
                           group_leader = groupLeader,$
                           tlb_size_events = 1,$
                           /column)
    clus_base[0] = wTopBase
    application_tlb = wTopBase  ; return parameter.

        ; Create menu buttons
    wMenuButton = widget_button(menuBase, value = 'Options', event_pro = 's_ROI_ParameterManipulator_Window_event')
       void = widget_button(wMenuButton, value = 'Open New Data Set', uval = 'OPENNEWDATASET')
       wDeleteActiveDataSet = widget_button(wMenuButton, value = 'Delete Active Data Set', uval = 'DELETEACTIVEDATASET')
       wPassActiveDataSet = widget_button(wMenuButton, value = 'Delete Active Data Set', uval = 'PASSACTIVEDATASET', sensitive = 0)
       void = widget_button(wMenuButton, value = 'Reset color Table', uval = 'RESETCOLORTABLE', /sep)
        wViewDataTableOnOff = widget_button( wMenuButton, value = 'View Data Table (off)', uval = 'VIEWDATATABLEONOFF', /dynamic, /sep)


    wMenuButton = widget_button(menuBase, value = 'Plots', event_pro = 's_ROI_ParameterManipulator_Window_event')
       w1DHistOnOff = widget_button(wMenuButton, value = '1D-Histogramm (off)', uval = '1DHISTONOFF')
       w2DHistOnOff = widget_button(wMenuButton, value = '2D-Histogramm (off)', uval = '2DHISTONOFF')
       w2DSurfaceOffButton = widget_button(wMenuButton, value = '2D-Surface (off)', uval = '2DSURFACEONOFF')
       w3DHistOnOff = widget_button(wMenuButton, value = '3D-Histogramm (off)', uval = '3DHISTONOFF')
       wSubMenuButton = widget_button(wMenuButton, value = '1D-Histogram Smooth ...', /menu, /sep)
         void = widget_button(wSubMenuButton, value = '... no Smooth',  uval = 'SMOOTHHISTOGRAMOFF')
         void = widget_button(wSubMenuButton, value = '... by 1',  uval = 'SMOOTHHISTOGRAMBY1')
         void = widget_button(wSubMenuButton, value = '... by 2',  uval = 'SMOOTHHISTOGRAMBY2')
         void = widget_button(wSubMenuButton, value = '... by 3',  uval = 'SMOOTHHISTOGRAMBY3')
         void = widget_button(wSubMenuButton, value = '... by 5',  uval = 'SMOOTHHISTOGRAMBY5')
         void = widget_button(wSubMenuButton, value = '... by 10',  uval = 'SMOOTHHISTOGRAMBY10')
         void = widget_button(wSubMenuButton, value = '... by 15',  uval = 'SMOOTHHISTOGRAMBY15')
         void = widget_button(wSubMenuButton, value = '... by 30',  uval = 'SMOOTHHISTOGRAMBY30')
         void = widget_button(wSubMenuButton, value = '... by 50',  uval = 'SMOOTHHISTOGRAMBY50')
       wSubMenuButton = widget_button(wMenuButton, value = '1D-Histogram Fit ...', /menu)
         wFitHistByGaussOnOff = widget_button(wSubMenuButton, value = '... by Simple Gauss (off)',  uval = 'FITHISTBYGAUSS')
         wFitHistBy2GaussOnOff = widget_button(wSubMenuButton, value = '... by Double Gauss (off)',  uval = 'FITHISTBY2GAUSS')
         wFitHistByGrowthOnOff = widget_button(wSubMenuButton, value = '... by Growth Analysis (off)',  uval = 'FITHISTBYGROWTH')
         wPlottedFitOnlyOnOff = widget_button(wSubMenuButton, value = '... Show Fit Only (off)',  uval = 'PLOTFITTEDONLY', /sep)
       wSubMenuButton = widget_button(wMenuButton, value = '1D-Histogram Normalize ...', /menu)
         wNormHistByAreaOnOff = widget_button(wSubMenuButton, value = '... by Area (off)',  uval = 'NORMHISTBYAREA')
         wNormHistByMaxOnOff = widget_button(wSubMenuButton, value = '... by Maximum (off)',  uval = 'NORMHISTBYMAX')
       wSubMenuButton = widget_button(wMenuButton, value = '1D-Histogram Plot ...', /menu)
         wPlotNoZeroOnOff = widget_button(wSubMenuButton, value = 'Plot No Zero (off)', uval = 'PLOTNOZEROONOFF')
         wPlotStatisticsOnOff = widget_button(wSubMenuButton, value = 'Plot Statistics (on )', uval = 'PLOTSTATISTICSONOFF')
         void = widget_button(wSubMenuButton, value = '... Active and Passive',  uval = 'PLOTACTIVEANDPASSIVE')
         void = widget_button(wSubMenuButton, value = '... Both In One',  uval = 'PLOTALLINONE')
       wSubMenuButton = widget_button(wMenuButton, value = '2D Separate Populations ...', /menu)
         wSeparate2DOnOff = widget_button(wSubMenuButton, value = '... in 2-Dimensions (off)',  uval = 'SEPARATEPOPULATIONSIN2D')
         wSeparate3DOnOff = widget_button(wSubMenuButton, value = '... in 3-Dimensions (off)',  uval = 'SEPARATEPOPULATIONSIN3D')
       wPlotPercentOff = widget_button(wMenuButton, value = '2D Plot as Percent (off)', uval = 'PLOTPERCENTONOFF')
       wSubMenuButton = widget_button(wMenuButton, value = 'All Plots: Set Limits From ...', /menu)
         void = widget_button(wSubMenuButton, value = '... 2D-Slider Values',  uval = 'SETLIMITSFROMSLIDERVALUES')
         void = widget_button(wSubMenuButton, value = '... Active Data Set',  uval = 'SETLIMITSFROMACTIVEDATASET')
         void = widget_button(wSubMenuButton, value = '... All Data Sets',  uval = 'SETLIMITSFROMALLDATASETS')
         void = widget_button(wSubMenuButton, value = '... Choose Manually',  uval = 'SETLIMITSFROMCHOOSEMANUALLY')
         wKeepLimitsOnOff = widget_button(wSubMenuButton, value = 'Keep Limits (off)', uval = 'KEEPLIMITSONOFF')
       wPrincipalComponentOnOff = widget_button(wMenuButton,  value = 'Principal Component Analisys (off)', uvalue = 'PRINCIPALCOMPONENTANALYSIS', /sep, sensitive =1)
       wSubMenuButton = widget_button(wMenuButton, value = 'Plot Cluster ...', /menu, sensitive = 0 )
         void = widget_button(wSubMenuButton, value = '... (off)',  uval = 'PLOTCLUSTERONOFF')
         void = widget_button(wSubMenuButton, value = '... Plot Time On',  uval = 'PLOTCLUSTERTIMEON')
         void = widget_button(wSubMenuButton, value = '... Plot Cell Number On',  uval = 'PLOTCELLNUMBERON')
         void = widget_button(wSubMenuButton, value = '... Plot Cluster Number On',  uval = 'PLOTCLUSTERNUMBERON')
         void = widget_button(wSubMenuButton, value = '... Corrected Plot',  uval = 'PLOTCORRECT')
         void = widget_button(wSubMenuButton, value = '... Limit max-Plot',  uval = 'plotLIMITMAX')

       wListBase = widget_base(wTopBase, /row, event_pro = 's_ROI_ParameterManipulator_Window_event')
        wXListBase = widget_base(wListBase, /column)
             void = widget_button(wXListBase,  value = 'X-Axis |-> ', /menu)
              dummy = widget_button(void,  value = 'X-Log (OnOff)', uval = 'XLOGONOFF')
              dummy = widget_button(void,  value = 'X-Adquis (OnOff)', uval = 'XADQUISONOFF')
          wXList = widget_list(wXListBase,  xSize = 15, ySize = 4, value = allParamList,$
                                  uValue= {name:'xAxis', value : ptr_new(allParamList), active:0 },$
                                  event_pro = 's_ROIPM_List_Event',$
                                  kill_notify = 'CleanList')
          wXSlid = widget_slider(wXListBase, maximum = 200, minimum = 1, value = clus_manage[5], Uvalue = 'X-Intervall')

        wYListBase = widget_base(wListBase, /column)
             void = widget_button(wYListBase,  value = 'Y-Axis |-> ', /menu)
              dummy = widget_button(void,  value = 'Y-Log (OnOff)', uval = 'YLOGONOFF')
              dummy = widget_button(void,  value = 'Y-Adquis (OnOff)', uval = 'YADQUISONOFF')
          wYList = widget_list(wYListBase,  xSize = 15, ySize = 4, value = allParamList,$
                                  uValue= {name:'yAxis', value : ptr_new(allParamList), active:0 },$
                                  event_pro = 's_ROIPM_List_Event',$
                                  kill_notify = 'CleanList')
          wYSlid = widget_slider(wYListBase, maximum = 200, minimum = 1, value = clus_manage[5], Uvalue = 'Y-Intervall')

        wZListBase = widget_base(wListBase, /column)
             void = widget_button(wZListBase,  value = 'z-Axis |-> ', /menu)
              dummy = widget_button(void,  value = 'z-Log (OnOff)', uval = 'ZLOGONOFF')
              dummy = widget_button(void,  value = 'z-Adquis (OnOff)', uval = 'ZADQUISONOFF')
          wZList = widget_list(wZListBase,  xSize = 15, ySize = 4, value = allParamList,$
                                  uValue= {name:'zAxis', value : ptr_new(allParamList), active:0 },$
                                  event_pro = 's_ROIPM_List_Event',$
                                  kill_notify = 'CleanList')
          wZSlid = widget_slider(wZListBase, maximum = 200, minimum = 1, value = clus_manage[5], Uvalue = 'z-Intervall')

    state = {groupLeader : groupLeader,$
              roiom_tlb : groupLeader,$
              oldCT : old_color,$
              newCT: plot_color,$
              stack_tlb : 0l,$
              parameterPath : '',$
              oParamListContainer: obj_new('IDL_Container'),$
              wTopBase : wTopBase,$
              wListBase : wListBase,$
              wXList : wXList,$
              wYList : wYList,$
              wZList : wZList,$
              wXListBase : wXListBase,$
              wYListBase : wYListBase,$
              wZListBase : wZListBase,$
              wDeleteActiveDataSet : wDeleteActiveDataSet,$
              wPassActiveDataSet : wPassActiveDataSet,$
              wViewDataTableOnOff : wViewDataTableOnOff,$
                 fViewDataTableControl: 0,$
                 child_DataTableWidget_tlb : 0l,$
                 viewDataTablePosition: [390, 545],$
              w1DHistOnOff : w1DHistOnOff,$
                 f1DHistControl: 0,$
              w2DHistOnOff : w2DHistOnOff,$
                 f2DHistControl: 0,$
                 xy2DHistPosition: [0,0],$
              w2DSurfaceOffButton:w2DSurfaceOffButton,$
                 f2DSurfaceControl: 0,$
              w3DHistOnOff:w3DHistOnOff,$
                 f3DHistControl: 0,$
              wFitHistByGaussOnOff : wFitHistByGaussOnOff,$
                 fFitHistByGaussControl: 0,$
              wFitHistBy2GaussOnOff : wFitHistBy2GaussOnOff,$
                 fFitHistBy2GaussControl: 0,$
              wFitHistByGrowthOnOff : wFitHistByGrowthOnOff,$
                 fFitHistByGrowthControl: 0,$
              wNormHistByAreaOnOff : wNormHistByAreaOnOff,$
                 fNormHistByAreaControl: 0,$
              wNormHistByMaxOnOff : wNormHistByMaxOnOff,$
                 fNormHistByMaxControl: 0,$
              wSeparate2DOnOff : wSeparate2DOnOff,$
                 fSeparate2DControl: 0,$
              wSeparate3DOnOff : wSeparate3DOnOff,$
                 fSeparate3DControl: 0,$
              wPrincipalComponentOnOff : wPrincipalComponentOnOff,$
                 fPrincipalComponentControl: 0,$
              wKeepLimitsOnOff : wKeepLimitsOnOff,$
                 fKeepLimitsControl : 0,$
              wPlotStatisticsOnOff : wPlotStatisticsOnOff,$
                 fplotStatisticsControl: 1,$
              wPlotPercentOff : wPlotPercentOff,$
                 fplotPercentControl: 0,$
              wPlotNoZeroOnOff : wPlotNoZeroOnOff,$
                 fplotNoZeroControl: 0,$
              wPlottedFitOnlyOnOff : wPlottedFitOnlyOnOff,$
                 fplottedFitOnlyControl: 0 }

    state.oParamListContainer->add, obj_new('IDLgrText', allParamList), position = 0
    widget_control, wTopBase, set_uvalue = state, /realize
    s_ROIPM_InitializeWidgets, wTopBase
    xManager, 's_ROIPM_Resize', wTopBase, CleanUp = 's_ROIPM_Cleanup', group_leader = groupLeader, /no_block
end


pro s_ROIPM_List_Event, ev
    common  shareVar
    widget_control, ev.top, get_uValue = state, /no_copy
       tvlct,  (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
    widget_control, ev.top, set_uValue = state, /no_copy

    widget_control, ev.id, get_uValue = uValue, /no_copy
       uValue.active = ev.index
       activeParam = ev.index
       selectedParam = (*uValue.value)[ev.index]
       selectedList = uValue.name
    widget_control, ev.id, set_uValue = uValue, /no_copy

    case selectedList of
       'xAxis': begin
                   if (selectedParam ne '-NO SELECTION-') then begin
                                  ;Single Click
                    if (ev.Clicks eq 1) then begin
                        clus_manage[8] = activeParam
                        if (clus_manage[0] eq 1) then s_apop_zellhist_1D, clus_manage[8], clus_win[4], clus_manage[5], [clus_g_style[0],clus_g_style[3:*]]
                    endif
                        ;Double Click
                    if (ev.Clicks eq 2)  then begin
                    endif
                           endif
         endcase
       'yAxis': begin
                   if (selectedParam ne '-NO SELECTION-') then begin
                                  ;Single Click
                    if (ev.Clicks eq 1) then begin
                        clus_manage[9] = activeParam
                        if (clus_manage[0] eq 1) then s_apop_zellhist_1D, clus_manage[9], clus_win[5], clus_manage[6], [clus_g_style[1],clus_g_style[3:*]]
                    endif
                        ;Double Click
                    if (ev.Clicks eq 2)  then begin
                    endif
                           endif
         endcase
       'zAxis': begin
                   if (selectedParam ne '-NO SELECTION-') then begin
                                  ;Single Click
                    if (ev.Clicks eq 1) then begin
                        clus_manage[10] = activeParam
                        if (clus_manage[0] eq 1) then s_apop_zellhist_1D, clus_manage[10], clus_win[6], clus_manage[7], [clus_g_style[2],clus_g_style[3:*]]
                    endif
                        ;Double Click
                    if (ev.Clicks eq 2)  then begin
                    endif
                           endif
         endcase
    endcase

    if ( ((selectedList eq 'xAxis') or (selectedList eq 'yAxis')) and (clus_manage[1] eq 1))  then begin
       if (widget_info(clus_base[3],/valid)) then begin
         widget_control, clus_base[3], tlb_get_Offset = xy2DHistPosition
         widget_control, ev.top, get_uValue = state, /no_copy
          state.xy2DHistPosition = xy2DHistPosition
         widget_control, ev.top, set_uValue = state, /no_copy
         widget_control, clus_base[3],/destroy
         clus_base[3] = -1
         clus_win[1] = 0
         clus_manage[1] = 0
        endif
       widget_control, ev.top, get_uValue = state, /no_copy
         state.xy2DHistPosition = xy2DHistPosition
       widget_control, ev.top, set_uValue = state, /no_copy
       s_ROIPM_2dHistPlot, xy2DHistPosition = xy2DHistPosition
       if ((clus_g_style[15]  lt 11) and (clus_g_style[7] eq 1)) then apop_2D_param
    endif

    if (clus_manage[4] eq 1) then s_apop_zellfl_3D, nonew = 1
       ;Cluster_Statistics
    if (clus_manage[8] ne -1) then if (clus_manage[21] eq 1) and (selectedParam eq '#_SubClust') and  (total(clus_active) gt 0) then s_apop_cluster_plot

    widget_control, ev.top, get_uValue = state, /no_copy
       tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_ROIPM_Resize_event, ev
    widget_control, ev.top, get_uValue = state, /no_copy

    evx = floor((ev.X-15)/3.)
    evy = ev.y -75

    ; Don't resize listbase on UNIX
    if (!VERSION.OS_FAMILY eq 'Windows') then begin
       widget_control, state.wListBase, scr_xsize = ev.X-10, scr_ysize = ev.y-10

       widget_control, state.wXListBase, scr_xsize=evx
       widget_control, state.wYListBase, scr_xsize=evx
       widget_control, state.wZListBase, scr_xsize=evx
    endif

       widget_control, state.wXList, scr_ysize=evy
       widget_control, state.wYList,  scr_ysize=evy
       widget_control, state.wZList,  scr_ysize=evy
    widget_control, ev.top, set_uValue = state, /no_copy
end


pro s_apop_time_slider
    ; Creates Slider_Time_Table for interactive time selection of growth events!
    ; Steffen Aug|98
    common  shareVar
    clus_base[7] = widget_base(title = 'TIMETABLE [Minutes])', /column)
    zahl = (strCompress(sindgen(total(clus_active)+2),/remove_all))(1:total(clus_active)+1)
    for i = 0, ceil(1.0*(total(clus_active)+1)/3)-1 do begin
       t2 = widget_base(clus_base[7], /row)
       for j = 0,(total(clus_active)-i*3-1)<2 do begin
               t1 = cw_fslider(t2, xSize = 102, edit = 1, minimum = 0,  maximum = 6000,$
                        value = clusTime[i*3+j], title = clus_name((where(clus_active eq 1))[i*3+j]),  Uvalue = 'CLUS_SLID'+zahl[i*3+j])
       endfor
    endfor
    t1 = cw_fslider(t2, xSize = 102, edit = 1, minimum = 0,  maximum = 3000,$
    value = clusTime(n_elements(zahl)-1), title = 'double-Time',  Uvalue = 'CLUS_SLID'+zahl(n_elements(zahl)-1) )
    widget_control, clus_base[7],tlb_set_xOffset = 515, /realize
    xManager, 's_apop_time_slider', clus_base[7], event_handler = 's_ROI_ParameterManipulator_Window_event', group_leader = clus_base[6]
end


pro s_apop_adquis_slider, xyz_adquis
    ; Creates 2 Sliders for interactive dada adquisition! (Additive and Multiplicative)
    ; Steffen Nov|98
    common  shareVar
    if ( xyz_adquis eq 'X_') then minmax = 1.0 *(clus_limits( clus_manage[8],5) -  clus_limits( clus_manage[8],0)) / 2
    if ( xyz_adquis eq 'Y_') then minmax = 1.0 *(clus_limits( clus_manage[9],5) -  clus_limits( clus_manage[9],0)) / 2
    if ( xyz_adquis eq 'Z_') then minmax = 1.0 *(clus_limits( clus_manage[10],5) -  clus_limits( clus_manage[10],0)) / 2
    clus_base[8] = widget_base(title = xyz_adquis+'ADQUISITION TABLE ',    /column)
    t2 = widget_base(clus_base[8], /column)
    t1 = cw_fslider(t2, xSize = 100, edit = 1, minimum = (-1.*minmax),  maximum = minmax, value = 0 , title = xyz_adquis+'ADDITIVE',  Uvalue = xyz_adquis+'ADQUIS_ADDI')
    t1 = cw_fslider(t2, xSize = 100, edit = 1, minimum = 0.01,  maximum = 10, value = 1, title = xyz_adquis+'MULTIPLICATIVE',  Uvalue = xyz_adquis+'ADQUIS_MULTI')
    t1 = widget_button(t2, value = 'Done', Uvalue = xyz_adquis+'DONE')
    widget_control, clus_base[8], tlb_set_xOffset = 515, /realize
    xManager, 's_apop_adquis_slider', clus_base[8], event_handler = 's_ROI_ParameterManipulator_Window_event', group_leader = clus_base[0]
end


pro s_ROIPM_paramTable_event, ev
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
    widget_control, clus_base[0], set_uValue = state, /no_copy

    widget_control, ev.id, get_uvalue = uvalue
    case (uvalue) of
    'Table': begin
          for i = 0,clus_manage[14]-1 do begin
              if (round(float(strMid(ev.value,0,(strPos(ev.value,':'))))) eq i+1) then begin
                 if (ev.select eq 1) then begin
                   clus_active[i] = 1
                   clus_manage[15] = i
                 endif
                 if (ev.select eq 0) then begin
                   clus_active[i] = 0
                   if (total(clus_active) eq 0) then begin
                    clus_active[0] = 1
                    widget_control, ev.id, get_value = value
                        value[0] = 1
                    widget_control, ev.id, set_value = value, /update
                   endif
                   clus_manage[15] = (where(clus_active eq 1))[0]
                 endif
                 widget_control, ev.top, get_uValue = state, /no_copy
                   widget_control, state.groupLeader, get_uValue = stateParent, /no_copy
                    widget_control, stateParent.wTopBase, tlb_set_title = 's_Param |-> '+ strCompress(string('Active: ',fix (clus_manage[15]+1),'.',clus_name(clus_manage[15]) ))
                   widget_control, state.groupLeader, set_uValue = stateParent, /no_copy
                 widget_control, ev.top, set_uValue = state, /no_copy
                 if (clus_manage[0] eq 1) then begin
                   if (clus_manage[8] ne -1) then s_apop_zellhist_1D, clus_manage[8], clus_win[4], clus_manage[5], [clus_g_style[0],clus_g_style[3:*]]
                   if (clus_manage[9] ne -1) then s_apop_zellhist_1D, clus_manage[9], clus_win[5], clus_manage[6], [clus_g_style[1],clus_g_style[3:*]]
                   if (clus_manage[10] ne -1) then s_apop_zellhist_1D, clus_manage[10], clus_win[6], clus_manage[7], [clus_g_style[2],clus_g_style[3:*]]
                 endif
                 if (clus_manage[1] eq 1) then begin
                   wset, clus_win[1]
                   erase
                   s_ROIPM_2dHistPlot, nonew = 1
                   if ((clus_g_style[15]  lt 11) and (clus_g_style[7] eq 1)) then apop_2D_param
                 endif
                 if (clus_manage[4] eq 1) then s_apop_zellfl_3D,  nonew = 1
              endif
           endfor
       endcase
    endcase

    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
    widget_control, clus_base[0], set_uValue = state, /no_copy
end

pro s_ROIPM_PT_Cleanup, tlb
    widget_control, tlb, get_uValue = state, /no_copy

    if (widget_info(state.groupLeader, /valid)) then begin
       widget_control, state.groupLeader, get_uValue = stateParent, /no_copy
         if (stateParent.fViewDataTableControl) then stateParent.fViewDataTableControl = s_ToggleButtonOnOffState(stateParent.wViewDataTableOnOff)
         stateParent.child_DataTableWidget_tlb = -1l
         xyPosition = stateParent.viewDataTablePosition
         widget_control, tlb, tlb_get_Offset = xyPosition
         stateParent.viewDataTablePosition = xyPosition
       widget_control, state.groupLeader, set_uValue = stateParent, /no_copy
    endif

    widget_control, tlb, set_uValue = state, /no_copy
    if widget_info(tlb, /valid) then widget_control, tlb, /destroy
end

pro s_ROIPM_paramTable, groupLeader = groupLeader, application_tlb = application_tlb, viewDataTablePosition = viewDataTablePosition
    common shareVar

    clus_base[2] = widget_base(title = 'Parameter Table', xOffset = viewDataTablePosition[0], yOffset = viewDataTablePosition[1], /column)

    application_tlb = clus_base[2]
    t2 = widget_base(clus_base[2],  /row, y_scroll_size = ( (n_elements(clus_active)>1) * 35)+25 )
    t1 = widget_base(t2, /column)
    zahl = (strCompress(sindgen(clus_manage[14]+1)))(1:clus_manage[14])
    t1 = cw_bgroup(t1, clus_name, set_value = clus_active, button_uvalue = zahl(0:clus_manage[14]-1)+':' + clus_name, /frame, /nonexclusive,  label_top = 'Data Sets:', Uvalue = 'Table')

    state = {groupLeader : groupLeader }

    widget_control, clus_base[2], set_uvalue = state, /realize
    xManager, 'S', clus_base[2], event_handler = 's_ROIPM_paramTable_event', CleanUp = 's_ROIPM_PT_Cleanup', group_Leader = groupLeader, /no_block
end


pro s_ROIPM_2dHistPlot_event, ev
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
       oAllParamList = state.oParamListContainer->get(position = 0)
       oAllParamList->getProperty, strings = allParamList
    widget_control, clus_base[0], set_uValue = state, /no_copy

    widget_control, ev.id, get_uvalue = uvalue
    case (uvalue) of
       '2dPlotPix':  clus_g_style[15] = 0
       '2dPlotCont': if (clus_g_style[15] gt 5) then  clus_g_style[15] = 1 else clus_g_style[15] = clus_g_style[15] + 1
       '2dPlotFill': if (clus_g_style[15] lt 14) then  clus_g_style[15] = (clus_g_style[15] + 1) > 11 else clus_g_style[15] = 11
       'S_1':    clus_manage[18] = ev.value
       'S_2':    clus_manage[19] = ev.value
       else:
    endcase

    for i = 1,4 do begin
       if (uvalue eq allParamList(clus_manage[8])+'_X'+(['0','1','2','3','4'])[i]) then $
         clus_limits(clus_manage[8],i) = clus_limits(clus_manage[8],0) + 1.*ev.value/255. * (clus_limits(clus_manage[8],5)-clus_limits(clus_manage[8],0))
       if (uvalue eq allParamList(clus_manage[9])+'_Y'+(['0','1','2','3','4'])[i]) then $
         clus_limits(clus_manage[9],i) = clus_limits(clus_manage[9],0) + 1.*ev.value/255. * (clus_limits(clus_manage[9],5)-clus_limits(clus_manage[9],0))
    endfor

    if (clus_manage[1] eq 1) then begin
       s_ROIPM_2dHistPlot, nonew = 1
       if (clus_g_style[15]  lt 4) then begin
          wset, clus_win[1]
          apop_2D_param
       endif
    endif
    if (clus_manage[0] eq 1) then begin
       if (clus_manage[8] ne -1) then s_apop_zellhist_1D, clus_manage[8], clus_win[4], clus_manage[5], [clus_g_style[0],clus_g_style[3:*]]
       if (clus_manage[9] ne -1) then s_apop_zellhist_1D, clus_manage[9], clus_win[5], clus_manage[6], [clus_g_style[1],clus_g_style[3:*]]
       if (clus_manage[10] ne -1) then s_apop_zellhist_1D, clus_manage[10], clus_win[6], clus_manage[7], [clus_g_style[2],clus_g_style[3:*]]
    endif
    if (clus_manage[4] eq 1) then s_apop_zellfl_3D,  nonew = 1

    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
    widget_control, clus_base[0], set_uValue = state, /no_copy
end


pro s_ROIPM_2dHistPlot, nonew = nonew, xy2DHistPosition = xy2DHistPosition
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
       oAllParamList = state.oParamListContainer->get(position = 0)
       oAllParamList->getProperty, strings = allParamList
    widget_control, clus_base[0], set_uValue = state, /no_copy

    minX = clus_limits(clus_manage[8],0)
    maxX = clus_limits(clus_manage[8],5)
    minY = clus_limits(clus_manage[9],0)
    maxY = clus_limits(clus_manage[9],5)

    if not(keyword_set(nonew)) then begin
       zahl = ['0','1','2','3','4']
       ;SLIDERSTATISTICS
       if (clus_manage[16] eq 0) then begin
         a = strCompress(string( '2D-Histogram |-> X-axis: ',allParamList(clus_manage[8]), '   |   y-axis: ', allParamList(clus_manage[9]) ))
         clus_base[3] = widget_base(title = a, /frame, xOffset = xy2DHistPosition[0], yOffset = xy2DHistPosition[1], /row, tlb_frame_attr = 8)

         t1 = widget_base(clus_base[3], /column)
         for i = 4,1,-1 do t2 = widget_slider(t1, scr_ySize = 74, suppress_value = 1,  minimum = 0, maximum = 255,$
                        value = round(255.*(clus_limits(clus_manage[9],i)-minY)/(maxY-minY)),$
                        Uvalue = allParamList(clus_manage[9])+'_Y'+zahl(i), /vertical)
         a = widget_base(clus_base[3], /column)
         draw = widget_draw(a, xSize = 520, ySize = 300)
         t1 = widget_base(a, /row)
         for i = 1,4 do t2 = widget_slider(t1, scr_xSize = 80, suppress_value = 1,  minimum = 0, maximum = 255,$
                        value = round(255.*(clus_limits(clus_manage[8],i)-minX)/(maxX-minX)),$
                        Uvalue = allParamList(clus_manage[8])+'_X'+zahl(i))
         title = ['P:','A:']
         for i = 1,2 do t2 = widget_slider(t1, title = title(i-1), suppress_value = 1, minimum = 1, maximum = 5,$
                           value = 1, /vertical , scr_ySize = 15,  scr_xSize = 25, Uvalue = 'S_'+zahl(i))
         t2 = widget_button(t1, value = 'Pix', uvalue = '2dPlotPix')
         t2 = widget_button(t1, value = 'Cont', uvalue = '2dPlotCont')
         t2 = widget_button(t1, value = 'Fill', uvalue = '2dPlotFill')
       endif

       widget_control, clus_base[3] , /realize
       clus_win[1] = !D.WINDOW
       clus_manage[1] = 1
    endif

    wset, clus_win[1]
    erase
    tvlct, r, g, b, /get
    widget_control, clus_base[0], get_uValue = state, /no_copy
       oAllParamList = state.oParamListContainer->get(position = clus_manage[15]+1)
       oAllParamList->getProperty, strings = paramNames
    widget_control, clus_base[0], set_uValue = state, /no_copy

    i = (where(paramNames eq allParamList(clus_manage[8])) )[0] > 0
    j = (where(paramNames eq allParamList(clus_manage[9])) )[0] > 0
    act_x = (lonArr( allcl_lenght(clus_manage[15],1) ))  +   allcl_parameter(clus_manage[15] , i , 0 : allcl_lenght(clus_manage[15],1)-1)
    act_y = (lonArr( allcl_lenght(clus_manage[15],1) ))  +   allcl_parameter(clus_manage[15] , j , 0 : allcl_lenght(clus_manage[15],1)-1)
    act_plot = hist_2d( (act_x - minX )  * (clus_manage[5] /  (maxX-minX)) ,  (act_y - minY )  * (clus_manage[6] /  (maxY-minY)) ,  max1 = clus_manage[5],  max2 = clus_manage[6])

    if (total(clus_active) gt 1) then begin
       whereDataSetActive = where(clus_active eq 1)
       for k = 0,n_elements(whereDataSetActive)-1 do if (whereDataSetActive[k] ne clus_manage[15])   then begin
             widget_control, clus_base[0], get_uValue = state, /no_copy
          oAllParamList = state.oParamListContainer->get(position = whereDataSetActive[k]+1)
          oAllParamList->getProperty, strings = paramNames
             widget_control, clus_base[0], set_uValue = state, /no_copy

         num_x = (where(paramNames eq allParamList(clus_manage[8])) )[0]
         num_y = (where(paramNames eq allParamList(clus_manage[9])) )[0]
             if (n_elements(pas_x) eq 0) then pas_x = fltArr(allcl_lenght(whereDataSetActive[k],1)) + allcl_parameter(whereDataSetActive[k],num_x,0:allcl_lenght(whereDataSetActive[k],1)-1) $
              else pas_x = [pas_x, fltArr(allcl_lenght(whereDataSetActive[k],1)) + allcl_parameter(whereDataSetActive[k],num_x,0:allcl_lenght(whereDataSetActive[k],1)-1)]
           if (n_elements(pas_y) eq 0) then pas_y = fltArr(allcl_lenght(whereDataSetActive[k],1)) + allcl_parameter(whereDataSetActive[k],num_y,0:allcl_lenght(whereDataSetActive[k],1)-1) $
              else pas_y = [pas_y, fltArr(allcl_lenght(whereDataSetActive[k],1)) + allcl_parameter(whereDataSetActive[k],num_y,0:allcl_lenght(whereDataSetActive[k],1)-1)]
      endif
      pas_plot = hist_2d(  (pas_x - minX )  * (clus_manage[5] / (maxX-minX)) , (pas_y - minY )  * (clus_manage[6] / (maxY-minY)) , max1 = clus_manage[5], max2 = clus_manage[6] )
    endif

    if (clus_g_style[15] ge 1) and (clus_g_style[15] le 14)  and (clus_g_style[16] eq 0)  then begin           ; BEGIN_plot_procedure

       a = act_plot
       if (total(clus_active) gt 1) then aa = pas_plot
       if (clus_g_style[3] eq 1) then begin
         a = a * (1.0 / total(a))
         if (total(clus_active) gt 1) then aa = aa * (1.0 / total(aa))
       endif
       if (clus_g_style[3] eq 2) then begin
         a = a * (1.0 / max(a))
         if (total(clus_active) gt 1) then aa = aa * (1.0 / max(aa))
       endif

       if (clus_g_style[15] ge 11) then  begin     ; FILLED CONTOURS
         if (clus_g_style[15] gt 12) then  tvlct, 255*sqrt(findgen(256)/255),255*sqrt(findgen(256)/255),255*sqrt(findgen(256)/255) else loadct, 0
         a = bytScl(congrid(a, 260, 240, /interp, /minus), top = 255)
         tv, a, 70,45,1
         if (total(clus_active) ne 1) then begin
               aa = bytScl(congrid(aa, 260, 240, /interp, /minus), top = 255)
                tv, aa, 70,45, 2
              endif
              tvlct, r, g, b
       endif else begin
         if (total(clus_active) eq 1) then i = 1 else i = 2
         col = 2
         for j = 1,i do begin
          if (clus_g_style[15] eq 1) then  contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device, thick = 1.2,  color = col, /noErase
          if (clus_g_style[15] eq 2) then  contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device, thick = 1.2,  color = col, /noErase, C_ANNOTATION = ['0']
          if (clus_g_style[15] eq 3) then  contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device, thick = 1.2,  color = col, /noErase
          if (clus_g_style[15] eq 4) then begin
              if (j eq 2) then tvlct, g,r,b
              contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device,  color = col, /noErase, /fill
              if (j eq 2) then tvlct, r,g,b
          endif
          if (clus_g_style[15] eq 5) then  begin
              if (j eq 2) then tvlct, g,r,b
              contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device,  color = col, thick = 1 , /noErase, /fill
              if (j eq 2) then tvlct, r,g,b
              contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device,  thick = 1.2,  color = col, /noErase
          endif
          if (clus_g_style[15] eq 6) then  begin
               if (j eq 2) then tvlct, g,r,b
              contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device, color = col, thick = 1 , /noErase, /fill
              if (j eq 2) then tvlct, r,g,b
              contour, a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,330,300], /device, thick = 1.2,  color = col, /noErase, C_ANNOTATION = ['0']
          endif
          col = 3
          if (total(clus_active) ne 1) then a = aa
         endfor
       endelse

       plot, [minX,maxX], [minY,maxY], xstyle = 1, ystyle = 1, pos = [70,45,330,285], /noErase, /device, xthick = 2, ythick = 2,$
              Xtitle = allParamList(clus_manage[8]), Ytitle = allParamList(clus_manage[9]),  color = 1, charSize = 0.9,  /nodata
       a = tvrd(70,45,261,241, true = 1)
       clus_perwin = bytArr(261,241) + 3*(a[1,*,*] gt 0) + 2*(a[0,*,*] gt 0)
    endif  ;end _CONTOUR_plot_procedure

       ; Plot Pixel
    if ((clus_g_style[15] eq 0) or (clus_g_style[15] eq 3)  or (clus_g_style[15] eq 14)) and  (clus_g_style[16] eq 0)  then begin
       if (clus_g_style[15] eq 0) then plot, [minX,maxX], [minY,maxY], xstyle = 1, ystyle = 1, pos = [70,45,330,285], /device,$
          Xtitle = allParamList(clus_manage[8]), Ytitle = allParamList(clus_manage[9]),  color = 1, charSize = 0.9,  /nodata
       if (total(clus_active) gt 1) then oplot, pas_x, pas_y, psym = 2, color = 3, symsize = 0.1* clus_manage[18]^1.5
       oplot, act_x, act_y,  psym = 6, color = 2, symsize = 0.1 * clus_manage[19]^1.5
       a = tvrd(70,45,261,241, true = 1)
       clus_perwin = bytArr(261,241) + 3*(a[1,*,*] gt 0) + 2*(a[0,*,*] gt 0)
    endif

       ; Plot for Publication
    if (clus_g_style[16] gt 0) then begin
       tvlct, r, g, b, /get
       loadct, 3
       a = act_plot
       if (total(clus_active) ne 1) then aa = pas_plot
       if (clus_g_style[3] eq 1) then begin
         a = a * (1.0 / total(a))
         if (total(clus_active) gt 1) then aa = aa * (1.0 / total(aa))
       endif
       if (clus_g_style[3] eq 2) then begin
         a = a * (1.0 / max(a))
         if (total(clus_active) gt 1) then aa = aa * (1.0 / max(aa))
       endif

       p_a = bytScl(congrid(a, 260, 240, /interp, /minus), top = (255))
       if (clus_g_style[3] eq 1) and  (total(clus_active) ne 1) then p_a = p_a * max(a) / ( max(a) >  max(aa) )
       tv,  (bytArr(520,300)) + 255
       tv,  255-p_a, 70, 45
           if (total(clus_active) ne 1) then begin
              p_b = bytScl(congrid(aa, 260, 240, /interp, /minus), top = 255)
         if (clus_g_style[3] eq 1) then p_b = p_b * max(aa) / ( max(a) >  max(aa) )
         tv, (255-p_b) < (255-p_a), 70, 45
       endif

       if (total(clus_active) eq 1) then i = 1 else i = 2
       a_a = p_a
       for j = 1,i do begin
         if (clus_g_style[15] eq 1) then  contour, a_a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,370,300], /device, thick = 1.2,  color = 0, /noErase
         if (clus_g_style[15] eq 2) then  contour, a_a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,370,300], /device, thick = 1.2,  color = 0, /noErase, C_ANNOTATION = ['0']
         if (clus_g_style[15] ge 3) then  contour, a_a,  nLevels = round(clus_manage[7]/5+1),  xstyle = 4, ystyle = 4, pos = [71,46,370,300], /device, thick = 1.2,  color = 0, /noErase
         if (total(clus_active) ne 1) then a_a = p_b
       endfor

       tv, congrid( 255-( bytArr(256,2) + [bindgen(256), bindgen(256)])   ,260 ,25) , 70, 260
       plot, [minX,maxX], [minY,maxY], xstyle = 1, ystyle = 1, pos = [70,45,330,285], /noErase, /device, xthick = 2, ythick = 2,$
          Xtitle = allParamList(clus_manage[8]), Ytitle = allParamList(clus_manage[9]),  background = 255,  color = 0,  charSize = 0.9,  /nodata


       if (clus_g_style[16] ge 1)  and (total(clus_active) gt 1) then begin  ; 1D and 2D-Statistics
         if (clus_manage[10] ne -1) then begin
          minz = clus_limits(clus_manage[10],0)
          maxz = clus_limits(clus_manage[10],5)

          i = (where(paramNames eq allParamList(clus_manage[10])) )[0]
          act_z = (lonArr( allcl_lenght(clus_manage[15],1) ))  +  allcl_parameter(clus_manage[15] , i , 0 : allcl_lenght(clus_manage[15],1)-1)

          whereDataSetActive = where(clus_active eq 1)
          for k = 0,n_elements(whereDataSetActive)-1 do if (whereDataSetActive[k] ne clus_manage[15]) then begin
              widget_control, clus_base[0], get_uValue = state, /no_copy
                 oAllParamList = state.oParamListContainer->get(position = whereDataSetActive[k]+1)
                 oAllParamList->getProperty, strings = paramNames
              widget_control, clus_base[0], set_uValue = state, /no_copy
              num_x = (where(paramNames eq allParamList(clus_manage[10])) )[0]
              if (n_elements(pas_z) eq 0) then pas_z = fltArr(allcl_lenght(whereDataSetActive[k],1)) + allcl_parameter(whereDataSetActive[k],num_x,0:allcl_lenght(whereDataSetActive[k],1)-1) $
                 else pas_z = [pas_z, fltArr(allcl_lenght(whereDataSetActive[k],1)) + allcl_parameter(whereDataSetActive[k],num_x,0:allcl_lenght(whereDataSetActive[k],1)-1)]
         endif
       endif

         xyouts, 345, 275, strCompress(string('1D_Separation: ' )), /device, charSize = 1, color = 0
              h_act_x = histogram( act_x, min = minX, max = maxX, binsize = (1.0*(maxX-minX)/clus_manage[5]) )       ; X_AXIS_SEPARATION_STATISTICS
              h_pas_x = histogram( pas_x, min = minX, max = maxX, binsize = (1.0*(maxX-minX)/clus_manage[5]) )
         h_act_x = 1.0 * h_act_x / total(h_act_x)
         h_pas_x = 1.0 * h_pas_x / total(h_pas_x)
         i = moment(act_x)
         j = moment(pas_x)
         signoise_ = (i[0] - j[0])/sqrt([i[1],j[1]])
         i = where (h_act_x gt 0)
         if (total(i) ne -1) then begin
          act_in_1 = total( h_act_x(i) *  h_act_x(i) / ( h_act_x(i) +  h_pas_x(i)) )
          act_in_2 = total( h_act_x(i) *  h_pas_x(i) / ( h_act_x(i) +  h_pas_x(i)) )
         endif
         i = where ( (h_pas_x*h_act_x) gt 0)
         if (total(i) ne -1) then overlap = total( h_pas_x(i) <  h_act_x(i) )
         i = where ( h_act_x gt 0)
         if (total(i) ne -1) then clean = total( h_act_x(i) ) -  total( h_pas_x(i) <  h_act_x(i) )
         xyouts, 345, 260, strCompress(string('X_overlap: ', overlap, clean )),  /device, charSize = 0.8, color = 0
         xyouts, 345, 250, strCompress(string('act_(act/pas): ', act_in_2, act_in_1)),  /device, charSize = 0.8, color = 0
         i = tm_test( double( (act_X-minX)/(maxX-minX)), double( (pas_X-minX)/(maxX-minX) ), /unequal)
         xyouts, 345, 235,strCompress(string('Student Test: ',byte(i[0]), '/ ', i[1] )),  /device, charSize = 0.8, color = 0
         xyouts, 345, 30, strCompress(string('Signal_Noise_X: ', signoise_[0], '/ ' ,  signoise_[1] )),  /device, charSize = 0.8, color = 0

              h_act_y = histogram( act_y, min = minY, max = maxY, binsize = (1.0*(maxY-minY)/clus_manage[6]) )       ; Y_AXIS_SEPARATION_STATISTICS
              h_pas_y = histogram( pas_y, min = minY, max = maxY, binsize = (1.0*(maxY-minY)/clus_manage[6]) )
         h_act_y = 1.0 * h_act_y / total(h_act_y)
         h_pas_y = 1.0 * h_pas_y / total(h_pas_y)
         i = moment(act_y)
         j = moment(pas_y)
         signoise_ = (i[0] - j[0]) / sqrt([i[1],j[1]])
         i = where (h_act_y gt 0)
         if (total(i) ne -1) then begin
          act_in_1 = total( h_act_y(i) *  h_act_y(i) / ( h_act_y(i) +  h_pas_y(i)) )
          act_in_2 = total( h_act_y(i) *  h_pas_y(i) / ( h_act_y(i) +  h_pas_y(i)) )
         endif
         i = where ( (h_pas_y * h_act_y) gt 0)
         if (total(i) ne -1) then overlap = total( h_pas_y(i) <  h_act_y(i) )
         i = where ( h_act_y gt 0)
         if (total(i) ne -1) then clean = total( h_act_y(i) ) -  total( h_pas_y(i) <  h_act_y(i) )
         xyouts, 345, 215, strCompress(string('Y_: ', overlap, clean )),  /device, charSize = 0.8, color = 0
         xyouts, 345, 205, strCompress(string('act_(act/pas): ', act_in_2, act_in_1 )),  /device, charSize = 0.8, color = 0
         i = tm_test( double( (act_y-minY)/(maxY-minY)), double( (pas_y-minY)/(maxY-minY) ), /unequal)
         xyouts, 345, 190, strCompress(string('Student Test: ',   byte(i[0]), '/ ' , i[1] )),  /device, charSize = 0.8, color = 0
         xyouts, 345, 20, strCompress(string('Signal_Noise_Y: ', signoise_[0], '/ ' ,  signoise_[1] )),  /device, charSize = 0.8, color = 0

         if (clus_manage[10] ne -1) then begin
               h_act_z = histogram( act_z, min = minz, max = maxz, binsize = (1.0*(maxz-minz)/clus_manage[7]) )     ; Z_AXIS_SEPARATION_STATISTICS
                h_pas_z = histogram( pas_z, min = minz, max = maxz, binsize = (1.0*(maxz-minz)/clus_manage[7]) )
          h_act_z = 1.0 * h_act_z / total(h_act_z)
          h_pas_z = 1.0 * h_pas_z / total(h_pas_z)
          i = moment(act_z)
          j = moment(pas_z)
          signoise_ = (i[0] - j[0])/sqrt([i[1],j[1]])

          i = where (h_act_z gt 0)
          if (total(i) ne -1) then begin
              act_in_1 = total( h_act_z(i) *  h_act_z(i) / ( h_act_z(i) +  h_pas_z(i)) )
              act_in_2 = total( h_act_z(i) *  h_pas_z(i) / ( h_act_z(i) +  h_pas_z(i)) )
          endif
          i = where ( (h_pas_z*h_act_z) gt 0)
          if (total(i) ne -1) then overlap = total( h_pas_z(i) <  h_act_z(i) )
          i = where ( h_act_z gt 0)
          if (total(i) ne -1) then clean = total( h_act_z(i) ) -  total( h_pas_z(i) <  h_act_z(i) )
          xyouts, 345, 170, strCompress(string('Z_ ', allParamList(clus_manage[10]) , overlap, clean )),  /device, charSize = 0.8, color = 0
          xyouts, 345, 160, strCompress(string('act_(act/pas): ', act_in_2, act_in_1 )),  /device, charSize = 0.8, color = 0
          i = tm_test( double( (act_z-minz)/(maxz-minz)), double( (pas_z-minz)/(maxz-minz) ), /unequal)
          xyouts, 345, 145, strCompress(string('Student Test: ',  byte(i[0]), '/ ', i[1] )),  /device, charSize = 0.8, color = 0
          xyouts, 345, 10, strCompress(string('Signal_Noise_Z: ', signoise_[0], '/ ' ,  signoise_[1] )),  /device, charSize = 0.8, color = 0
         endif

         xyouts, 345, 120, strCompress(string('2D_Separation: ' )), /device, charSize = 1, color = 0           ;      XY_AXIS_SEPARATION_STATISTICS
         act_plot = 1.0 * act_plot / total(act_plot)
         pas_plot = 1.0 * pas_plot / total(pas_plot)
         i = where (act_plot gt 0)
         if (total(i) ne -1) then begin
          act_in_1 = total( act_plot(i) *  act_plot(i) / ( act_plot(i) +  pas_plot(i)) )
          act_in_2 = total( act_plot(i) *  pas_plot(i) / ( act_plot(i) +  pas_plot(i)) )
         endif
         i = where ( (pas_plot*act_plot) gt 0)
         if (total(i) ne -1) then overlap = total( act_plot(i) <  pas_plot(i) )
         i = where ( act_plot gt 0)
       if (total(i) ne -1) then clean = total( act_plot(i) ) -  total( act_plot(i) <  pas_plot(i) )
       xyouts, 345, 105, strCompress(string('XY_: ', overlap, clean )),  /device, charSize = 0.8, color = 0
       xyouts, 345, 95, strCompress(string('act_(act/pas): ', act_in_2, act_in_1 )),  /device, charSize = 0.8, color = 0

       if (clus_manage[10] ne -1) and  (clus_g_style[16] eq 2) then begin
         xyouts, 345, 75, strCompress(string('3D_Separation: ' )), /device, charSize = 1, color = 0          ;        XYZ_AXIS_SEPARATION_STATISTICS

         act_x = round( 1.0 * (act_x -  minX) / (maxX-minX) * (clus_manage[5]) )
         pas_x = round( 1.0 * (pas_x -  minX) / (maxX-minX) * (clus_manage[5]) )
         act_y = round( 1.0 * (act_y -  minY) / (maxY-minY) * (clus_manage[6]) )
         pas_y = round( 1.0 * (pas_y -  minY) / (maxY-minY) * (clus_manage[6]) )
         act_z = round( 1.0 * (act_z -  minz) / (maxz-minz) * (clus_manage[7]) )
         pas_z = round( 1.0 * (pas_z -  minz) / (maxz-minz) * (clus_manage[7]) )

         cubus_act = lonArr(clus_manage[5]+1, clus_manage[6]+1, clus_manage[7]+1)
         cubus_pas = cubus_act

         for i = 0, clus_manage[5] do begin
          j = where(act_x eq i)
          if (total(j) ne -1) then cubus_act(i,*,*) = hist_2d( act_y(j), act_z(j), max1 = clus_manage[6], max2 = clus_manage[7] )
          j = where(pas_x eq i)
          if (total(j) ne -1) then cubus_pas(i,*,*) = hist_2d( pas_y(j), pas_z(j), max1 = clus_manage[6], max2 = clus_manage[7] )
         endfor
         cubus_act = 1.0 * cubus_act / total(cubus_act)
         cubus_pas = 1.0 * cubus_pas / total(cubus_pas)

         i = where (cubus_act gt 0)
         if (total(i) ne -1) then begin
          act_in_1 = total( cubus_act[i] *  cubus_act[i] / ( cubus_act[i] +  cubus_pas[i]) )
          act_in_2 = total( cubus_act[i] *  cubus_pas[i] / ( cubus_act[i] +  cubus_pas[i]) )
         endif
         i = where ( (cubus_pas*cubus_act) gt 0)
         if (total(i) ne -1) then overlap = total( cubus_act[i] < cubus_pas[i] )
         i = where ( cubus_act gt 0)
         if (total(i) ne -1) then clean = total( cubus_act[i] ) -  total( cubus_act[i] <  cubus_pas[i] )
          xyouts, 345, 60, strCompress(string('XYZ_: ', overlap, clean )),  /device, charSize = 0.8, color = 0
          xyouts, 345, 50, strCompress(string('act_(act/pas): ', act_in_2, act_in_1 )),  /device, charSize = 0.8, color = 0
         endif
       endif ; 3D-Statistics end
       tvlct, r, g, b
    endif ;___________________________________END Plot for PUBLICATION____________________

         ;FIT_MODE_GAUSSFIT:   STATISTICAL EVALUATION
;   if (clus_g_style[10] eq 1)  and (total(clus_active) ne 1) and  (clus_g_style[15] ge 1) and (clus_g_style[15] le 14) then begin
;     a1 = total(stat_a,1)
;     aa1 = total(stat_aa,1)
;     a = total(1.0*a1 * (1.0*a1/((a1+aa1)>1)) / total(a1)) * 100
;     xyouts, 90,268, strCompress(string('Y__Hit: ',a,'%')), /device, charSize = 1, color = 2, charthick = 1.3
;     a = total(1.0*aa1 * (1.0*aa1/((a1+aa1)>1)) / total(aa1)) * 100
;     xyouts, 200,268, strCompress(string('Y__Hit: ',a,'%')), /device, charSize = 1, color = 3, charthick = 1.3
;     a1 = total(stat_a,2)
;     aa1 = total(stat_aa,2)
;     a = total(1.0*a1 * (1.0*a1/((a1+aa1)>1)) / total(a1)) * 100
;     xyouts, 90,255, strCompress(string('X__Hit: ',a,'%')), /device, charSize = 1, color = 2, charthick = 1.3
;     a = total(1.0*aa1 * (1.0*aa1/((a1+aa1)>1)) / total(aa1)) * 100
;     xyouts, 200,255, strCompress(string('X__Hit: ',a,'%')), /device, charSize = 1, color = 3, charthick = 1.3
;     a = total(1.0*stat_a * (1.0*stat_a/((stat_a+stat_aa)>1)) / total(stat_a)) * 100
;     xyouts, 90,242, strCompress(string('2D_Hit: ',a,'%')), /device, charSize = 1, color = 2, charthick = 1.3
;     a = total(1.0*stat_aa * (1.0*stat_aa/((stat_a+stat_aa)>1)) / total(stat_aa)) * 100
;     xyouts, 200,242, strCompress(string('2D_Hit: ',a,'%')), /device, charSize = 1, color = 3, charthick = 1.3
;   endif
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
    widget_control,clus_base[0], set_uValue = state, /no_copy

    if not (keyword_set(nonew)) then xManager, 'S', clus_base[3],  event_handler = 's_ROIPM_2dHistPlot_event', group_leader = clus_base[0]
end


pro apop_2D_param
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
       oAllParamList = state.oParamListContainer->get(position = 0)
       oAllParamList->getProperty, strings = allParamList
    widget_control, clus_base[0], set_uValue = state, /no_copy

    ;SLIDERSTATISTICS
    if (clus_manage[16] eq 0) then begin
       xmin = clus_limits(clus_manage[8],0)
       xmax = clus_limits(clus_manage[8],5)
       xi = [clus_limits(clus_manage[8],1)<clus_limits(clus_manage[8],2), clus_limits(clus_manage[8],1)>clus_limits(clus_manage[8],2),$
          clus_limits(clus_manage[8],3)<clus_limits(clus_manage[8],4), clus_limits(clus_manage[8],3)>clus_limits(clus_manage[8],4)]

       ymin = clus_limits(clus_manage[9],0)
       ymax = clus_limits(clus_manage[9],5)
       yi = [clus_limits(clus_manage[9],1)<clus_limits(clus_manage[9],2), clus_limits(clus_manage[9],1)>clus_limits(clus_manage[9],2),$
          clus_limits(clus_manage[9],3)<clus_limits(clus_manage[9],4), clus_limits(clus_manage[9],3)>clus_limits(clus_manage[9],4)]

       a = clus_perwin * 0
       jx = 259. * (xi-xmin)/(xmaX-xmin)
       jy = 239. * (yi-ymin)/(ymaX-ymin)
       a(jx[0]:jx[1], 1:*) = 60
       a(jx[2]:jx(3), 1:*) = a(jx[2]:jx(3), 1:*) + 60
       a(1:*, jy[0]:jy[1]) = a(1:*, jy[0]:jy[1]) + 60
       a(1:*, jy[2]:jy(3)) = a(1:*, jy[2]:jy(3)) + 60
       tv, clus_perwin + (a*(clus_perwin eq 0)), 70, 45

       NG = intArr(5,5)
       NNG = 0
       NXG = 0
       NYG = 0
       whereDataSetActive = where(clus_active eq 1)
       for k = 0,n_elements(whereDataSetActive)-1 do begin
         widget_control,  clus_base[0], get_uValue = state, /no_copy
          oAllParamList = state.oParamListContainer->get(position = whereDataSetActive[k] + 1)
          oAllParamList->getProperty, strings = paramNames
         widget_control,  clus_base[0], set_uValue = state, /no_copy

         num_x = (where(paramNames eq allParamList(clus_manage[8])) )[0]
         num_y = (where(paramNames eq allParamList(clus_manage[9])) )[0]

         n = allcl_lenght(whereDataSetActive[k],1)                        ; Number of all cells
         nnx = allcl_parameter(whereDataSetActive[k], num_x, 0:n-1)    ; Parametervektors
         nny = allcl_parameter(whereDataSetActive[k], num_y, 0:n-1)

         nx = fix(total( ( nnx ge xmin ) * ( nnx le xmax ) ))    ; Number of cells in limits
         ny = fix(total( ( nny ge ymin ) * ( nny le ymax ) ))

         nxx = bytArr(5,n)
         nxx(0,*) = ( nnx ge xmin ) * ( nnx lt xi[0] )
         nxx(1,*) = ( nnx ge xi[0] ) * ( nnx le xi[1] )
         nxx(2,*) = ( nnx gt xi[1] ) * ( nnx lt xi[2] )
         nxx(3,*) = ( nnx ge xi[2] ) * ( nnx le xi(3) )
         nxx(4,*) = ( nnx gt xi(3) ) * ( nnx le xmax )
         nyy = bytArr(5,n)
         nyy(0,*) = ( nny ge ymin ) * ( nny lt yi[0] )
         nyy(1,*) = ( nny ge yi[0] ) * ( nny le yi[1] )
         nyy(2,*) = ( nny gt yi[1] ) * ( nny lt yi[2] )
         nyy(3,*) = ( nny ge yi[2] ) * ( nny le yi(3) )
         nyy(4,*) = ( nny gt yi(3) ) * ( nny le ymax )

         nn = intArr(5,5)
         for i = 0,4 do for j = 0,4 do nn(i,j) = total( nxx(i,*) * nyy(j,*) )

         if (whereDataSetActive[k] eq clus_manage[15]) then begin
          tv, bytArr(160,300),339,0
          if (clus_manage[22] eq 0) then begin
              xyouts, 345, 135, strCompress(string('Active: ',fix(clus_manage[15]+1),'.',clus_name(clus_manage[15]),': (',n,')' )), /device, charSize = 1, color = 2
              xyouts, 355, 120, strCompress(string('x: [',nx,']','  Y: [',ny,']')),  /device, charSize = 0.95, color = 2
              for i = 0,4 do for j = 0,4 do xyouts, 350+i*30, 42+j*15,strCompress(string(nn(i,j))), /device, charSize = 0.9, color = 2
          endif else begin
              xyouts, 345, 135, strCompress(string('Active %: ',fix(clus_manage[15]+1),'.',clus_name(clus_manage[15]),': (',n,')' )), /device, charSize = 1, color = 2
              xyouts, 355, 120, strCompress(string('x: [',nx,']','  Y: [',ny,']')),  /device, charSize = 0.95, color = 2
              for i = 0,4 do for j = 0,4 do xyouts, 350+i*30, 42+j*15,strCompress(string(round(100.*nn(i,j)/(nx<ny)) )), /device, charSize = 0.9, color = 2
          endelse
          if (clus_manage[23] eq 1) then xyouts, 350, 10,'Keep_limits_on', /device, charSize = 0.9, color = 2
          if (clus_manage[23] eq 0) then xyouts, 350, 10,'Keep_limits_on', /device, charSize = 0.9, color = 0
          if (clus_manage[23] eq 0) then xyouts, 350, 10,'Keep_limits_off', /device, charSize = 0.9, color = 2

         endif else begin
          NG = NG + nn
          NNG = NNG + n
          NXG = NXG + nx
          NYG = NYG + ny
         endelse
       endfor
       if (max(NG) gt 0) then begin
         if (clus_manage[22] eq 0) then begin
          xyouts, 345, 263, strCompress(string('Passiv: SUM = (',NXG,')')), /device, charSize = 1, color = 3
          xyouts, 355, 248, strCompress(string('x: [',NXG,']','  Y: [',NYG,']')), /device, charSize = 0.95, color = 3
          for i = 0,4 do for j = 0,4 do xyouts, 350+i*30, 170+j*15,strCompress(string(NG(i,j))), /device, charSize = 0.9, color = 3
         endif else begin
          xyouts, 345, 263, strCompress(string('Passiv %: SUM = (',NXG,')')), /device, charSize = 1, color = 3
          xyouts, 355, 248, strCompress(string('x: [',NXG,']','  Y: [',NYG,']')), /device, charSize = 0.95, color = 3
          for i = 0,4 do for j = 0,4 do xyouts, 350+i*30, 170+j*15,strCompress(string(round(100.*NG(i,j)/(NXG<NYG) ))), /device, charSize = 0.9, color = 3
         endelse
       endif
    endif
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
    widget_control,clus_base[0], set_uValue = state, /no_copy
end

;________________________________ZELL_FIT_ROUTINES__________________________________________
; S_APOP_DOUBLEGAUSS.
; S_APOP_CELLGrowTHFIT.
; S_APOP_CELLRETARDOFIT.

pro s_apop_doublegauss, x, a, f, pder
    ; f(x,a) = a[0] *  exp( (x - a[1])^2/(-2.0*a[2]^2)) + a[3] * exp( (x - a[4])^2/(-2.0*a[5]^2))
    b1x = exp( (x - a[1])^2/(-2.0*a[2]^2))
    b2x = exp( (x - a[4])^2/(-2.0*a[5]^2))
    f = a[0] * b1x +  a[3] * b2x
    pder = [  [b1x], [b1x  * a[0] * (X-a[1]) / a[2]^2],  [b1x  * a[0] *(X-a[1])^2 / a[2]^3 ],$  ; calculate the partial derivatives.
              [b2x], [b2x  * a[3] * (X-a[4]) / a[5]^2],  [b2x  * a[3] *(X-a[4])^2 / a[5]^3 ]]
end

pro s_apop_cellgrowthfit, x, a, f, pder
    ; f (x,a) = a[0] * 2^(x/a[1]) = a[0] * exp(ln2 * x/a[1]  )     ; f(x) = a^(u(x)),    f '(x) = ln(a) * a^(u(x)) * du(x)/dx
    b1x = 2^(1.0*x/a[1])
    f = a[0] * b1x
    pder = [ [b1x],  [a[0]  * ALOG[2] * b1x  * (-1.0* x*a[1]^(-2.)) ] ]      ; calculate the partial derivatives.
end

pro s_apop_cellretardofit, x, a, f, pder,  S_CONSTANTS = s_constants
    ; f (x,a) s = _constants[0]                 for x < = a[0]
    ; = s_constants[0] * 2^(X-a[0])/ s_constants[1])       for x > a[0]
    num = total(x le a[0])
    f = fltArr(num) +  s_constants[0]
    f = [ f,   s_constants[0] * 2^((1.0*x(1>num:*)-a[0])/ s_constants[1])]
    print, a[0]
    pder = x(0:(num-1)>0) * 0.0                    ; calculate the partial derivatives.
    pder = [pder , [s_constants[0] * ALOG[2] *  2^((1.0*x((1>num):*)-a[0]) /  s_constants[1]) *  (-1.0/s_constants[1])] ]
end
;__________ENDE__________________ZELL_FIT_ROUTINES______________________________________________


pro s_apop_zellhist_1D, para_id, win_id, intervall, g_style
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
       oAllParamList = state.oParamListContainer->get(position = 0)
       oAllParamList->getProperty, strings = allParamList
    widget_control, clus_base[0], set_uValue = state, /no_copy

    if (clus_manage[15] ne -1) then begin
       amin = clus_limits(para_id,0)
       amax = clus_limits(para_id,5)
       if (amin eq amax) then begin
          amax = amin + 1
          intevall = 1
       endif
       i = [clus_limits(para_id,1)<clus_limits(para_id,2), clus_limits(para_id,1)>clus_limits(para_id,2),$
          clus_limits(para_id,3)<clus_limits(para_id,4), clus_limits(para_id,3)>clus_limits(para_id,4)]
       b = ''
       whereDataSetActive = where(clus_active eq 1)
       for k = 0, n_elements(whereDataSetActive)-1 do begin
             widget_control, clus_base[0], get_uValue = state, /no_copy
          oAllParamList = state.oParamListContainer->get(position = whereDataSetActive[k]+1)
          oAllParamList->getProperty, strings = paramNames
             widget_control, clus_base[0], set_uValue = state, /no_copy

         whParamActive = (where(paramNames eq allParamList(para_id)))[0]
         if (whParamActive eq -1) then begin
              widget_control, clus_base[0], get_uValue = state, /no_copy
              tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
              widget_control, clus_base[0], set_uValue = state, /no_copy
         endif else begin
          if (whereDataSetActive[k] eq clus_manage[15]) then begin
               a = histogram(allcl_parameter(whereDataSetActive[k],whParamActive,0:allcl_lenght(whereDataSetActive[k],1)-1),$
                        min = amin, max = amax, binsize = (1.0*(amaX-amin)/intervall) )
              SumOfa = total(allcl_parameter(whereDataSetActive[k],whParamActive,0:allcl_lenght(whereDataSetActive[k],1)-1))
          endif else begin
              if (n_elements(b) eq 1) then $
              b = histogram(allcl_parameter(whereDataSetActive[k],whParamActive,0:allcl_lenght(whereDataSetActive[k],1)-1),$
                        min = amin, max = amax, binsize = (1.0*(amaX-amin)/intervall) ) else $
              b = histogram(allcl_parameter(whereDataSetActive[k],whParamActive,0:allcl_lenght(whereDataSetActive[k],1)-1),$
                        min = amin, max = amax, binsize = (1.0*(amaX-amin)/intervall), input = b)
              SumOfb = total(allcl_parameter(whereDataSetActive[k],whParamActive,0:allcl_lenght(whereDataSetActive[k],1)-1))
          endelse
         endelse
       endfor

       wset, win_id & erase
       ;SLIDERSTATISTICS
       if (clus_manage[16] eq 0) and (g_style[5] and (g_style[8] eq 0) ) then begin
         aa = bytArr(250,147)
         j = 29 + 216 * (i-amin)/(amaX-amin)
         aa(j[0]:j[1] , 1:*) = aa(j[0]:j[1], 1:*) + 60
         aa(j[2]:j[3] , 1:*) = aa(j[2]:j[3], 1:*) + 60
         tv, aa, 0,49

         n = allcl_lenght(clus_manage[15],1)
         nn = allcl_parameter(clus_manage[15], whParamActive,0:n-1)
        n0 = fix(total( ( nn ge amin ) * ( nn le amax ) ))
         n1 = fix(total( ( nn ge amin ) * ( nn lt i[0] ) ))
         n2 = fix(total( ( nn ge i[0] ) * ( nn le i[1] ) ))
         n3 = fix(total( ( nn gt i[1] ) * ( nn lt i[2] ) ))
         n4 = fix(total( ( nn ge i[2] ) * ( nn le i[3] ) ))
         n5 = fix(total( ( nn gt i[3] ) * ( nn le amax ) ))

         xyouts, 5, 26, strCompress(string('Parameter Sum: ', SumOfa )), /device, charSize = 0.9, color = 2
         xyouts, 5, 15, strCompress(string('Cells # ',clus_name(clus_manage[15]),': ',n,'   [in Limis]: ',n0)), /device, charSize = 0.8, color = 2
         xyouts, 35, 182,strCompress(string('#', n1)), /device, charSize = 0.8, color = 2
         xyouts, 70, 182,strCompress(string('#', n2)), /device, charSize = 0.8, color = 2
         xyouts, 120, 182,strCompress(string('#', n3)), /device, charSize = 0.8, color = 2
         xyouts, 170, 182,strCompress(string('#', n4)), /device, charSize = 0.8, color = 2
         xyouts, 210, 182,strCompress(string('#', n5)), /device, charSize = 0.8, color = 2
         xyouts, 5, 4,  strCompress(string('%',float([n1,n2,n3,n4,n5])/n0*100)), /device, charSize = 0.8, color = 2
       endif

       xAxis = (findgen(n_elements(a))) * (amaX-amin)/(n_elements(a)-1) + amin
       if (g_style[6] eq 1) and  (n_elements(b) gt 1) then begin
         a = a + b
         b = ''
       endif
       if (g_style[7] eq 1)  then begin
         an0 = where(a ne 0)
         if ( total(an0) ne -1) then a = interPol(a(an0), xAxis(an0), xAxis)
         if (n_elements(b) gt 1) then begin
          an0 = where(b ne 0)
          if ( total(an0) ne -1) then b = interPol(b(an0), xAxis(an0), xAxis)
         endif
       endif
       if (g_style(3) gt 0) and (n_elements(a) gt g_style(3)) then begin
         a = s_meanvector(a,g_style(3))
         if (n_elements(b) gt 1) then b = s_meanvector(b,g_style(3))
       endif
       if (g_style[1] eq 1) then begin
         a = 1.0 * a / total(a)
         if (n_elements(b) gt 1) then b = 1.0 * b / total(b)
       endif
       if (g_style[1] eq 2) then begin
         a = 1.0 * a / max(a)
         if (n_elements(b) gt 1) then b = 1.0 * b / max(b)
       endif
    ;  if (g_style(11) eq 1) and (allParamList(clus_manage[8]) eq '#_SubClust') then begin
    ;   if (total(clus_active) gt 0) and (win_id eq clus_win[4]) then s_apop_cluster_plot
    ;   return
    ;  endif

       if (g_style[9] eq 0) then begin
         if (g_style[0] eq 0) then plot, xAxis, a, psym = 10*g_style[4], color = 2, pos = [28,50,245,195], /device,$
                 yRange = [0,1.1*(max(a)>max(b))], xstyle = 1,  charSize = 0.8 ,xthick = 1, ythick = 1, /noErase, thick = 1.4 else $
              plot, xAxis, a, psym = 10*g_style[4], color = 2,  pos = [28,50,245,195], /device,$
                 yRange = [0,1.1*(max(a)>max(b))],  charSize = 0.8, /xlog, xthick = 1, ythick = 1, /noErase, thick = 1.4
       endif

       if (n_elements(b) gt 1) and (g_style[9] eq 0) then oplot, xAxis, b, psym = 10*g_style[4], color = 3, thick = 1.4
       if (g_style[8] gt 0)  then begin
         if (g_style[8] eq 1)  then begin

          e_xmean = total(1.0 * xAxis * a) / total(a)
          afit = gaussFit(xAxis, a, aparam, nterms = 3, estimates = [max(a), e_xmean, (max(xAxis)-min(xAxis))/3])
          if (g_style[9] eq 0)  then oplot, xAxis, afit, color = 2, thick = 2 $
                else plot, xAxis, afit, color = 2, pos = [28,50,245,195], /device,$
                 yRange = [0,1.1*(max(a)>max(b))], xstyle = 1, xthick = 1, ythick = 1, thick = 2, charSize = 0.8

          if (n_elements(b) gt 1) then begin
              e_xmean = total(1.0 * xAxis * b) / total(b)
              bfit = gaussFit(xAxis, b, bparam, nterms = 3, estimates = [(max(b)),(e_xmean),(max(xAxis)-min(xAxis))/3])
              oplot, xAxis, bfit, color = 3, thick = 2

                   stat_a = total(1.0*afit * (1.0*afit/((afit+bfit)>1)) / total(afit)) * 100
    ;          xyouts, 40,172, strCompress(string('G-Hit: ',stat_a,'%')), /device, charSize = 0.9, color = 2, charthick = 1.2
              stat_aa = total(1.0*bfit * (1.0*bfit/((afit+bfit)>1)) / total(bfit)) * 100
    ;          xyouts, 140,172, strCompress(string('G-Hit: ',stat_aa,'%')), /device, charSize = 0.9, color = 3, charthick = 1.2

              if (g_style[9] eq 0) then begin
                 stat_a = total(1.0*a * (1.0*a/((a+b)>1)) / total(a)) * 100
    ;            xyouts, 55,162, strCompress(string('Hit: ',stat_a,'%')), /device, charSize = 0.9, color = 2, charthick = 1.2
                 stat_aa = total(1.0*b * (1.0*b/((a+b)>1)) / total(b)) * 100
    ;            xyouts, 155,162, strCompress(string('Hit: ',stat_aa,'%')), /device, charSize = 0.9, color = 3, charthick = 1.2
              endif

          endif
          if (g_style[5] eq 1) then begin
              xyouts, 40,183, strCompress(string('Gauss_fit: = I exp-((X-µ)²/2(sd)²)')), /device, charSize = 0.9, color = 1
              xyouts, 5, 16, strCompress(string('(I,µ,sd) = ', aparam[0],',  ', aparam[1],' +- ',aparam[2])), /device, charSize = 0.9, color = 2
              if (n_elements(b) gt 1) then xyouts, 5, 5, strCompress(string('(I,µ,sd) = ', bparam[0],',  ', bparam[1],' +- ',bparam[2])), /device, charSize = 0.9, color = 3
           endif
         endif
         if (g_style[8] eq 2)  then begin
          e_xmean = total(1.0 * xAxis * a) / total(a)
          if (i[0] ne i[1]) and (i[2] ne i[3]) then begin
              y = a
              a = [ y(round(((i[0]+i[1])/2-amin)/(amaX-amin)*(n_elements(y)))) , (i[0]+i[1])/2 , abs(i[0]-i[1])/2,$
               y(round(((i[2]+i[3])/2-amin)/(amaX-amin)*(n_elements(y)))) , (i[2]+i[3])/2 , abs(i[2]-i[3])/2 ]
              weights = (fltArr(n_elements(y))) + 1.
              afit = curveFit(xAxis, y, weights, a,  function_name = 's_apop_doublegauss')
              aparam = a
              a = y
               if (g_style[9] eq 0)  then oplot, xAxis, afit, color = 2, lineStyle = 0, thick = 2 $
               else plot, xAxis, afit, psym = 10*g_style[4], color = 2, lineStyle = 0, pos = [28,50,245,195], /device,$
              yRange = [0,1.1*(max(a)>max(b))], xstyle = 1, charSize = 0.8, xthick = 1, ythick = 1, thick = 2
              for ii = 0,1 do begin
                 afit = aparam(ii*3)*exp((xAxis-aparam(ii*3+1))^2/(-2.*aparam(ii*3+2)^2))
                  if (g_style[9] eq 0)  then oplot, xAxis, afit, color = 5, thick = 2 $
                  else plot, xAxis, afit, psym = 10*g_style[4], color = 5, pos = [28,50,245,195], /device,$
                 yRange = [0,1.1*(max(a)>max(b))], xstyle = 1, charSize = 0.8, xthick = 1, ythick = 1, thick = 2
              endfor
               if (g_style[5] eq 1) then begin
                 xyouts, 40,180, strCompress(string('Gauss_fit: = I exp-((X-µ)²/2(sd)²)')), /device, charSize = 0.9, color = 5
                 xyouts, 40,167, strCompress(string('GS: = ')), /device, charSize = 0.9, color = 2
                 xyouts, 80,167, strCompress(string('G1 + G2')), /device, charSize = 0.9, color = 5
                 xyouts, 5, 17, strCompress(string('(I,µ,sd): = ', aparam[0],',  ', aparam[1],' +- ',aparam[2])), /device, charSize = 0.9, color = 5
                 xyouts, 5, 4, strCompress(string('(I,µ,sd): = ', aparam(3),',  ', aparam(4),' +- ',aparam(5))), /device, charSize = 0.9, color = 5
               endif
          endif else  xyouts, 40,180, strCompress(string('Choose approximation parameters')), /device, charSize = 0.9, color = 5
         endif
       endif
    ;  plot, xAxis, a, pos = [28,50,245,195], /device, yRange = [0,1.1*(max(a)>max(b))], xstyle = 1, charSize = 0.8 , /noErase, xthick = 1, ythick = 1,  color = 1, /nodata

       if (g_style[0] eq 0) then plot, xAxis, a, pos = [28,50,245,195], /device, yRange = [0,1.1*(max(a)>max(b))], xstyle = 1, charSize = 0.8 , /noErase, xthick = 1, ythick = 1,  color = 1, /nodata $
         else  plot, xAxis, a, psym = 10*g_style[4], color = 1,  pos = [28,50,245,195], /device, yRange = [0,1.1*(max(a)>max(b))],  charSize = 0.8, /xlog, xthick = 1, ythick = 1, /noErase, thick = 1.4,  /nodata
    endif else a = s_apop_shout('No active Parameter')

    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
    widget_control, clus_base[0], set_uValue = state, /no_copy
end


pro s_apop_zellfl_3D,  nonew = nonew
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       tvlct,  (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
       oAllParamList = state.oParamListContainer->get(position = 0)
       oAllParamList->getProperty, strings = allParamList
        oAllParamList = state.oParamListContainer->get(position = clus_manage[15]+1)
       oAllParamList->getProperty, strings = paramNames
    widget_control, clus_base[0], set_uValue = state, /no_copy

    bx = ''
    whereDataSetActive = where(clus_active eq 1)
    for k = 0,n_elements(whereDataSetActive)-1 do begin
       i = (where(paramNames eq allParamList(clus_manage[8])) )[0]
       j = (where(paramNames eq allParamList(clus_manage[9])) )[0]
       l = (where(paramNames eq allParamList(clus_manage[10])) )[0]

       if (whereDataSetActive[k] eq clus_manage[15]) then begin
              ax = allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1)
              ay = allcl_parameter(clus_manage[15],j,0:allcl_lenght(clus_manage[15],1)-1)
              az = allcl_parameter(clus_manage[15],l,0:allcl_lenght(clus_manage[15],1)-1)
       endif else begin
         if (n_elements(bx) eq 1) then begin
               bx = allcl_parameter(whereDataSetActive[k],i,0:allcl_lenght(whereDataSetActive[k],1)-1)
               by = allcl_parameter(whereDataSetActive[k],j,0:allcl_lenght(whereDataSetActive[k],1)-1)
               bz = allcl_parameter(whereDataSetActive[k],l,0:allcl_lenght(whereDataSetActive[k],1)-1)
         endif else begin
                bx = [bx, allcl_parameter(whereDataSetActive[k],i,0:allcl_lenght(whereDataSetActive[k],1)-1)]
                by = [by, allcl_parameter(whereDataSetActive[k],j,0:allcl_lenght(whereDataSetActive[k],1)-1)]
                bz = [bz, allcl_parameter(whereDataSetActive[k],l,0:allcl_lenght(whereDataSetActive[k],1)-1)]
              endelse
       endelse
    endfor
    if (n_elements(bx) eq 1) then begin
          dimax = size(ax)
          inarr = fltArr(3,dimax(3))
          inarr(0,*) = ax(0,0,*)
          inarr(1,*) = ay(0,0,*)
       inarr(2,*) = az(0,0,*)
          Graph3D, 'Grafico de prueba',1,1,[clus_limits(clus_manage[8],0),$
                   clus_limits(clus_manage[8],5)],$
                   [clus_limits(clus_manage[9],0),clus_limits(clus_manage[9],5)],$
                   [clus_limits(clus_manage[10],0),clus_limits(clus_manage[10],5)],$
                   allParamList(clus_manage[8]),$
                   allParamList(clus_manage[9]),$
                   allParamList(clus_manage[10]),$
                   1,[254,1,7],[254,101,105],$
                   [22,17,234],[116,113,244],inarr,1
    endif
    if (n_elements(bx) gt 1) then begin
          dimax = size(ax)
          inarr1 = fltArr(3,dimax(3))
          inarr1(0,*) = ax(0,0,*)
          inarr1(1,*) = ay(0,0,*)
          inarr1(2,*) = az(0,0,*)

          dimbx = size(bx)
          inarr2 = fltArr(3,dimbx(3))
          inarr2(0,*) = bx(0,0,*)
          inarr2(1,*) = by(0,0,*)
          inarr2(2,*) = bz(0,0,*)

          Graph3D, 'Grafico de prueba',1,2,[clus_limits(clus_manage[8],0),$
                   clus_limits(clus_manage[8],5)],$
                   [clus_limits(clus_manage[9],0),clus_limits(clus_manage[9],5)],$
                   [clus_limits(clus_manage[10],0),clus_limits(clus_manage[10],5)],$
                   allParamList(clus_manage[8]),$
                   allParamList(clus_manage[9]),$
                   allParamList(clus_manage[10]),$
                   1,[254,1,7],[254,101,105],$
                   [22,17,234],[116,113,244],inarr1,inarr2
    endif

;-------------------------------------------------------------------------
wset, clus_win[3]

    s_plot_3dbox, ax, ay, az, /XY_PLANE, /YZ_PLANE, /XZ_PLANE,$
         gridstyle = 1,$
         s_color = [4,2,2,2],$
         s_oplot = 0,$
         s_symsize = 0.1 * clus_manage[19]^2,$
         AX = clus_manage[12] , AZ = clus_manage[13] ,$
         Xtitle = allParamList(clus_manage[8]),$
         Ytitle = allParamList(clus_manage[9]),$
         Ztitle = allParamList(clus_manage[10]),$
         xRange = [clus_limits(clus_manage[8],0),clus_limits(clus_manage[8],5)],$
         yRange = [clus_limits(clus_manage[9],0),clus_limits(clus_manage[9],5)],$
         zRange = [clus_limits(clus_manage[10],0),clus_limits(clus_manage[10],5)],$
         psym = 6, charSize = 1.6

    if (n_elements(bx) gt 1) then  s_plot_3dbox, bx, by, bz, /XY_PLANE, /YZ_PLANE, /XZ_PLANE,$
                              s_color = [5,3,3,3],$
                              s_symsize = 0.1 * clus_manage[18]^2,  s_oplot = 1, psym = 2
;-------------------------------------------------------------------------

end

pro s_plot_3dbox, x, Y, Z, color = color, background = background,   $
                    XY_PLANE = XY_PLANE, YZ_PLANE = YZ_PLANE,        $
                    XZ_PLANE = XZ_PLANE, SOLID_WALLS = SOLID_WALLS,$
                    psym = psym, gridstyle = gridstyle, title = title,$
              Xtitle = Xtitle, Ytitle = Ytitle, Ztitle = Ztitle,$
              SUBtitle = SUBtitle, lineStyle = lineStyle,      $
                    XYSTYLE = XYSTYLE, YZSTYLE = YZSTYLE, XZSTYLE = XZSTYLE,$
                    _EXTRA = e, s_color = s_color, s_oplot = s_oplot, s_symsize = s_symsize

;  set up simple error handling
   On_ERROR, 2

;  Lets make sure that all arrays are the same size
   Xcnt = n_elements(x)
   Ycnt = n_elements(Y)
   Zcnt = n_elements(Z)
   if (Xcnt ne Ycnt) or (Xcnt ne Zcnt) then $
          Message, 'x, Y and Z arrays must have same number of elements '

;  Check the values of the keywords
   if(n_elements(s_oplot)  eq 0)then  s_oplot = 0
   if(n_elements(s_color)  eq 0)then  s_color = [255,255,255,255]
   if(n_elements(psym)  eq 0)then      psym = 0
   if(n_elements(color) eq 0)then      color = !P.color
   if(n_elements(background) eq 0)then background = !P.background
   if(n_elements(gridstyle) eq 0)then  gridstyle = 1
   if(n_elements(lineStyle) eq 0)then  lineStyle = 0
   if(n_elements(XYSTYLE) eq 0)then    XYSTYLE = lineStyle
   if(n_elements(XZSTYLE) eq 0)then    XZSTYLE = lineStyle
   if(n_elements(YZSTYLE) eq 0)then    YZSTYLE = lineStyle

   if(not keyword_set(title))then      title = ''
   if(not keyword_set(SUBtitle))then   SUBtitle = ''
   if(not keyword_set(Xtitle))then     Xtitle = ''
   if(not keyword_set(Ytitle))then     Ytitle = ''
   if(not keyword_set(Ztitle))then     Ztitle = ''
   if(not keyword_set(zRange))then     zRange = [min(Z, max = zmax), zmax]

if (s_oplot ne 1) then begin
;  Use SURFACE to set up the coordinates system, handle titles ect...

   Surface, fltArr(Xcnt,Xcnt), x, Y, /nodata, /SAVE, TICKLEN = 1,        $
      color = color, Xgridstyle = gridstyle, Ygridstyle = gridstyle,         $
      Zgridstyle = gridstyle, background = background, XTICK_GET = xt,       $
      YTICK_GET = yt, ZTICK_GET = zt,zRange = zRange, SUBtitle = SUBtitle,     $
      title = title, Ytitle = Ytitle, Ztitle = Ztitle, Xtitle = Xtitle, _EXTRA = e

   name = replicate(' ' ,30)   ; Make up null tick names

;  See if the user wants to have 'Solid' box walls
   if(keyword_set(SOLID_WALLS))then begin
   ;  Using the values of Crange, fill in the box walls with the value of color
      PolyFill, [!x.Crange[0], !x.Crange, !x.Crange[1]],$ ;bottom
                [!Y.Crange, !Y.Crange[1], !Y.Crange[0]],  replicate(!Z.Crange[0], 4), /T3D, color = color
      PolyFill, [!x.Crange[0], !x.Crange, !x.Crange[1]],$ ; Back
                replicate(!Y.Crange[1],4), [!Z.Crange, !Z.Crange[1], !Z.Crange[0]],  /T3D, color = color
      PolyFill, replicate(!x.Crange[1],4), /T3D, color = color,$ ;side
                [!Y.Crange, !Y.Crange[1], !Y.Crange[0]],  [!Z.Crange[0], !Z.Crange, !Z.Crange[1]]

   ;  Now Replot the surface data
      color = background  ; reverse colors

      Surface, fltArr(Xcnt,Xcnt), x, Y, /nodata, /SAVE, TICKLEN = 1,       $
              /noErase, color = color, xTickName = name, yTickName = name,     $
              zTickName = name, Xgridstyle = gridstyle, Ygridstyle = gridstyle,$
              Zgridstyle = gridstyle, zRange = zRange, _EXTRA = e
   endif  ;if solid walls set.

;  Now complete the drawing of the axis box
   Axis, !x.Crange[1], !Y.Crange[1], !Z.Crange[1], /yAxis, /T3D, yTickName = name, color = color, yTickLen = 0
   Axis, !x.Crange[1], !Y.Crange[0], !Z.Crange[0], zAxis = -1, /T3D, zTickName = name, color = color, zTickLen = 0
   Axis, !x.Crange[0], !Y.Crange[1], !Z.Crange[0], /xAxis, /T3D, xTickName = name, color = color, xTickLen = 0
   Axis, !x.Crange[0], !Y.Crange[1], !Z.Crange[1], /xAxis, /T3D, xTickName = name, color = color, xTickLen = 0
   Axis, !x.Crange[1], !Y.Crange[1], !Z.Crange[0], zAxis = -1, /T3D, zTickName = name, color = color, zTickLen = 0
   Axis, !x.Crange[1], !Y.Crange[0], !Z.Crange[1], yAxis = -1, /T3D, yTickName = name, color = color, yTickLen = 0
   Axis, !x.Crange[1], !Y.Crange[0], !Z.Crange[0], /yAxis, /T3D, yTickName = name, color = color, yTickLen = 0
endif; s_oplot

; now plot the data
  plots, x, Y, Z, /T3D , psym = psym, color = s_color[0] ,lineStyle = lineStyle, _EXTRA = e, symsize = s_symsize

; and now plot the data along the walls of the box if requested
  if(keyword_set(XY_PLANE))then plots, x, y, replicate(!Z.Crange[0], Zcnt), /T3D, color = s_color[1], psym = 3
  if(keyword_set(YZ_PLANE))then plots, replicate(!x.Crange[1], Xcnt), Y, Z, /T3D, color = s_color[2],  psym = 3
  if(keyword_set(XZ_PLANE))then plots, x, replicate(!Y.Crange[1], Ycnt), Z, /T3D, color = s_color(3),  psym = 3
end


pro s_apop_axislimits_event, ev
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       oAllParamList = state.oParamListContainer->get(position = 0)
       oAllParamList->getProperty, strings = allParamList
    widget_control, clus_base[0], set_uValue = state, /no_copy

    widget_control, ev.top, get_uvalue = limits, /no_copy
       widget_control, ev.id, get_uvalue = value
       if (n_elements(value) eq 0) then value = ''
       name = strMid(tag_names(ev, /structure_name), 7, 4)
       case (name) of
       'BUTT': begin
           if (value eq 'DONE') then begin
              widget_control, /destroy, ev.top
              return
          endif
          if (value eq 'U+DONE') then begin
              clus_limits = limits
              for j = 0,n_elements(allParamList)-1 do  clus_limits(j,1:4) = [clus_limits(j,0)>clus_limits(j,1),  clus_limits(j,0)>clus_limits(j,2),$
                                        clus_limits(j,5)<clus_limits(j,3), clus_limits(j,5)<clus_limits(j,4)]
              widget_control, ev.top, set_uvalue = limits, /no_copy
              widget_control, ev.top, /destroy
              return
          endif
         endcase
       'TEXT': begin
          widget_control, ev.id, get_value = value, get_uvalue = uvalue, set_value = value
          number = strMid(uvalue,rStrPos(uvalue,'[')+2,1)
          namen = strMid(uvalue,0,rStrPos(uvalue,'['))
          limits(where(allParamList eq namen),round(float(number))) = value
         endcase
       else:
       endcase
    widget_control, ev.top, set_uvalue = limits, /no_copy
end


pro s_apop_axislimits
    common shareVar
    widget_control, clus_base[0], get_uValue = state, /no_copy
       oAllParamList = state.oParamListContainer->get(position = 0)
       oAllParamList->getProperty, strings = allParamList
    widget_control, clus_base[0], set_uValue = state, /no_copy

    limits = clus_limits
    Base_0 = widget_base(title = 'Steffen_APOP_GRENZEN_EDIT', /column)
    t1 = widget_button(Base_0, value = 'Go back without changes !', Uvalue = 'DONE')
    b1 = widget_base(Base_0, /column, /frame, x_scroll_size = 600, y_scroll_size = 400)
    for i = 0, n_elements(allParamList)-1 do begin
       t2 = widget_base(b1, /row)
       tit = allParamList(i)
       ti = strCompress(string(tit,'[',0,']:'))
       t1 = widget_LABEL(t2, value = ti)
       t1 = widget_text(t2, value = strCompress(string(limits(i,0))), Uvalue = ti, /editable, xSize = 10, ySize = 1)
           for j = 1,5 do begin
         ti = strCompress(string('[',j,']:'))
         t1 = widget_LABEL(t2, value = ti)
         t1 = widget_text(t2, value = strCompress(string(limits(i,j))), Uvalue = tit+ti, /editable, xSize = 10, ySize = 1)
       endfor
    endfor
    t1 = widget_button(Base_0, value = 'Go back and accept changes !', Uvalue = 'U+DONE')
    widget_control, Base_0, set_uvalue = limits, /realize
    xManager, 's_apop_axislimits', Base_0
end


pro s_ROI_ParameterManipulator_Window_event, ev
    common shareVar

    widget_control, ev.top, get_uValue = state, /no_copy
       tvlct, (state.newCT)[0,*], (state.newCT)[1,*], (state.newCT)[2,*]
    widget_control, ev.top, set_uValue = state, /no_copy

    widget_control, ev.id, get_uvalue = value
    if (n_elements(value) eq 0) then value = ''
    name = strMid(tag_names(ev, /structure_name), 7, 4)

    case (name) of
    'BUTT': begin
        case (value) of
        'NewColor': begin
             device, decomposed = 0
             loadct, 3
          tvlct, r, g, b, /get
          r = [0, 1, 1, 0, 0, 1]     ;Specify the red component,..
          g = [0, 1, 0, 1, 0, 1]   ;green component, and..
          b = [0, 1, 0, 0, 1, 0]   ;blue component of each color.
          tvlct, 255*r, 255*g, 255*b  ; color 0/BLACK 1/WHITE 2/RED 3/GREEN 4/BLUE 5/YELLOW
          tvlct, r, g, b, /get
              widget_control, ev.top, get_uValue = state, /no_copy
              state.newCT = [transpose(r),transpose(g), transpose(b)]
              widget_control, ev.top, set_uValue = state, /no_copy
           endcase

        'KEEPLIMITSONOFF': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fKeepLimitsControl = s_ToggleButtonOnOffState(state.wKeepLimitsOnOff)
              clus_manage[23] = state.fKeepLimitsControl
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'PLOTSTATISTICSONOFF': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fplotStatisticsControl = s_ToggleButtonOnOffState(state.wPlotStatisticsOnOff)
              clus_g_style[7] = state.fplotStatisticsControl
             widget_control, ev.top, set_uValue = state, /no_copy
          endcase
        'PLOTNOZEROONOFF': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fplotNoZeroControl = s_ToggleButtonOnOffState(state.wPlotNoZeroOnOff)
              clus_g_style[9] = state.fplotNoZeroControl
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'PLOTPERCENTONOFF': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fplotPercentControl = s_ToggleButtonOnOffState(state.wPlotPercentOff)
              clus_manage[22] = state.fplotPercentControl
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase

        'NORMHISTBYAREA': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fNormHistByAreaControl = s_ToggleButtonOnOffState(state.wNormHistByAreaOnOff)
              if (state.fNormHistByAreaControl) then begin
                  clus_g_style[3] = 1
                  if (state.fNormHistByMaxControl) then state.fNormHistByMaxControl = s_ToggleButtonOnOffState(state.wNormHistByMaxOnOff)
              endif else clus_g_style[16] = 0
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'NORMHISTBYMAX': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fNormHistByMaxControl = s_ToggleButtonOnOffState(state.wNormHistByMaxOnOff)
              if (state.fNormHistByMaxControl) then begin
                  clus_g_style[3] = 2
                  if (state.fNormHistByAreaControl) then state.fNormHistByAreaControl = s_ToggleButtonOnOffState(state.wNormHistByAreaOnOff)
              endif else clus_g_style[3] = 0
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'SEPARATEPOPULATIONSIN2D': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fSeparate2DControl = s_ToggleButtonOnOffState(state.wSeparate2DOnOff)
              if (state.fSeparate2DControl) then begin
                  clus_g_style[16] = 1
                  if (state.fSeparate3DControl) then state.fSeparate3DControl = s_ToggleButtonOnOffState(state.wSeparate3DOnOff)
              endif else clus_g_style[16] = 0
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'SEPARATEPOPULATIONSIN3D': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fSeparate3DControl = s_ToggleButtonOnOffState(state.wSeparate3DOnOff)
              if (state.fSeparate3DControl) then begin
                  clus_g_style[16] = 2
                  if (state.fSeparate2DControl) then state.fSeparate2DControl = s_ToggleButtonOnOffState(state.wSeparate2DOnOff)
              endif else clus_g_style[16] = 0
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'FITHISTBYGAUSS': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fFitHistByGaussControl = s_ToggleButtonOnOffState(state.wFitHistByGaussOnOff)
              if (state.fFitHistByGaussControl) then begin
                  clus_g_style[10] = 1
                  if (state.fFitHistBy2GaussControl) then state.fFitHistBy2GaussControl = s_ToggleButtonOnOffState(state.wFitHistBy2GaussOnOff)
                  if (state.fFitHistByGrowthControl) then state.fFitHistByGrowthControl = s_ToggleButtonOnOffState(state.wFitHistByGrowthOnOff)
              endif else clus_g_style[10] = 0
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'FITHISTBY2GAUSS': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fFitHistBy2GaussControl = s_ToggleButtonOnOffState(state.wFitHistBy2GaussOnOff)
              if (state.fFitHistBy2GaussControl) then begin
                  clus_g_style[10] = 2
                  if (state.fFitHistByGaussControl) then state.fFitHistByGaussControl = s_ToggleButtonOnOffState(state.wFitHistByGaussOnOff)
                  if (state.fFitHistByGrowthControl) then state.fFitHistByGrowthControl = s_ToggleButtonOnOffState(state.wFitHistByGrowthOnOff)
              endif else clus_g_style[10] = 0
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'FITHISTBYGROWTH': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fFitHistByGrowthControl = s_ToggleButtonOnOffState(state.wFitHistByGrowthOnOff)
              if (state.fFitHistByGrowthControl) then begin
                  clus_g_style[10] = 3
                  if (state.fFitHistByGaussControl) then state.fFitHistByGaussControl = s_ToggleButtonOnOffState(state.wFitHistByGaussOnOff)
                  if (state.fFitHistBy2GaussControl) then state.fFitHistBy2GaussControl = s_ToggleButtonOnOffState(state.wFitHistBy2GaussOnOff)
              endif else clus_g_style[10] = 0
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase
        'PLOTFITTEDONLY': begin
             widget_control, ev.top, get_uValue = state, /no_copy
              state.fplottedFitOnlyControl = s_ToggleButtonOnOffState(state.wPlottedFitOnlyOnOff)
              clus_g_style[11] = state.fplottedFitOnlyControl
             widget_control, ev.top, set_uValue = state, /no_copy
           endcase

        'SMOOTHHISTOGRAMOFF': clus_g_style[5] = 0
        'SMOOTHHISTOGRAMBY1': clus_g_style[5] = 1
        'SMOOTHHISTOGRAMBY2': clus_g_style[5] = 2
        'SMOOTHHISTOGRAMBY3': clus_g_style[5] = 3
        'SMOOTHHISTOGRAMBY5': clus_g_style[5] = 5
        'SMOOTHHISTOGRAMBY10': clus_g_style[5] = 10
        'SMOOTHHISTOGRAMBY15': clus_g_style[5] = 15
        'SMOOTHHISTOGRAMBY30': clus_g_style[5] = 30
        'SMOOTHHISTOGRAMBY50': clus_g_style[5] = 15

        'PLOTACTIVEANDPASSIVE': clus_g_style[8] = 0
        'PLOTALLINONE': clus_g_style[8] = 1
        'PLOTCELLNUMBERON':  clus_g_style[12] = 0
        'PLOTCLUSTERNUMBERON':  clus_g_style[12] = 1
        'XLOGONOFF':  clus_g_style[0] = 1 - clus_g_style[0]
        'YLOGONOFF': clus_g_style[1] = 1-clus_g_style[1]
        'ZLOGONOFF': clus_g_style[2] = 1-clus_g_style[2]


        'XADQUISONOFF': if (clus_manage[8] ne -1) then s_apop_adquis_slider, 'X_' else a = s_apop_shout('Choose X-Parameter !')
        'YADQUISONOFF': if (clus_manage[9] ne -1) then s_apop_adquis_slider, 'Y_' else a = s_apop_shout('Choose Y-Parameter !')
        'ZADQUISONOFF': if (clus_manage[10] ne -1) then s_apop_adquis_slider,'Z_' else a = s_apop_shout('Choose z-Parameter !')

        'plotLIMITMAX': clus_g_style[13] = 1 - clus_g_style[13]
        'PLOTCORRECT': clus_g_style[14] = 1 - clus_g_style[14]

        'A_1': clus_manage[20] = 1
        'A_2': clus_manage[20] = 2
        'A_3': clus_manage[20] = 3
        'A_4': clus_manage[20] = 4
        'A_5': clus_manage[20] = 5
        'A_6': clus_manage[20] = 6
        'A_7': clus_manage[20] = 7
        'A_8': clus_manage[20] = 8
        'A_9': clus_manage[20] = 9
        'VIEWDATATABLEONOFF': begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                     state.fViewDataTableControl = s_ToggleButtonOnOffState(state.wViewDataTableOnOff)
                   if (state.fViewDataTableControl) then begin
                    if (clus_manage[14] ne 0) then begin
                        wTopBase = state.wTopBase
                        viewDataTablePosition = state.viewDataTablePosition
                        widget_control, ev.top, set_uValue = state, /no_copy
                            s_ROIPM_paramTable, groupLeader = wTopBase, application_tlb = application_tlb, viewDataTablePosition = viewDataTablePosition
                        widget_control, ev.top, get_uValue = state, /no_copy
                        state.child_DataTableWidget_tlb = application_tlb
                    endif else a = s_apop_shout('Parameter missing !')
                   endif else begin
                    if (widget_info(state.child_DataTableWidget_tlb, /valid)) then begin
                        child_DataTableWidget_tlb = state.child_DataTableWidget_tlb
                        widget_control, ev.top, set_uValue = state, /no_copy
                           s_ROIPM_PT_Cleanup, child_DataTableWidget_tlb
                           clus_base[2] = -1
                        widget_control, ev.top, get_uValue = state, /no_copy
                       endif
                   endelse
                  widget_control, ev.top, set_uValue = state, /no_copy
              endcase
       '1DHISTONOFF': begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                     state.f1DHistControl = s_ToggleButtonOnOffState(state.w1DHistOnOff)
                     f1DHistControl = state.f1DHistControl
                     wTopBase = state.wTopBase
                  widget_control, ev.top, set_uValue = state, /no_copy
                  if (f1DHistControl) then begin
                   if (total(clus_manage(8:10)) eq -3) then a = s_apop_shout('XYz-Auswahl?')
                   if (total(clus_manage[15]) eq -1) then a = s_apop_shout('Parameter - Auswahl?')
                   if (total(clus_manage(8:10)) ne -3)  and  (total(clus_manage[15]) ne -1) then begin
                    if (clus_manage[0] ne 1) then begin
                        clus_base[1] = widget_base(title = '1D-histogram plots', tlb_frame_attr = 8, /row)
                        t2 = widget_base(clus_base[1], /row, space = 5)
                           t1 = widget_draw(t2, xSize = 250, ySize = 200, /button_events, retain = 2)
                           widget_control, clus_base[1], tlb_set_yOffset = 355, group_leader = wTopBase,  /realize
                           widget_control, get_value = window, t1
                           clus_win[4] = !D.WINDOW

                           t1 = widget_draw(t2, xSize = 250, ySize = 200, /button_events, retain = 2)
                           widget_control, clus_base[1], /realize
                           widget_control, get_value = window, t1
                           clus_win[5] = !D.WINDOW

                           t1 = widget_draw(t2, xSize = 250, ySize = 200, /button_events, retain = 2)
                           widget_control, clus_base[1], /realize
                           widget_control, get_value = window, t1
                           clus_win[6] = !D.WINDOW
                    endif
                    clus_manage[0] = 1
                    if (clus_manage[8] ne -1) then s_apop_zellhist_1D, clus_manage[8], clus_win[4], clus_manage[5], [clus_g_style[0],clus_g_style[3:*]]
                    if (clus_manage[9] ne -1) then s_apop_zellhist_1D, clus_manage[9], clus_win[5], clus_manage[6], [clus_g_style[1],clus_g_style[3:*]]
                    if (clus_manage[10] ne -1) then s_apop_zellhist_1D, clus_manage[10], clus_win[6], clus_manage[7], [clus_g_style[2],clus_g_style[3:*]]
                   endif
                  endif else begin
                   if (widget_info(clus_base[1], /valid)) then begin
                    widget_control, clus_base[1], /destroy
                    clus_base[1] = 0
                    clus_win(4:6) = 0
                    clus_manage[0] = 0
                   endif
                  endelse
         endcase
       '2DHISTONOFF': begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                     state.f2DHistControl = s_ToggleButtonOnOffState(state.w2DHistOnOff)
                     f2DHistControl = state.f2DHistControl
                     xy2DHistPosition = state.xy2DHistPosition
                  widget_control, ev.top, set_uValue = state, /no_copy
                  if (f2DHistControl) then begin
                     if (total(clus_manage(8:9)) eq -2) then a = s_apop_shout('x or Y - dimension missing !')
                   if (total(clus_manage[15]) eq -1) then a = s_apop_shout('Parameter missing !')
                   if ((total(clus_manage[15]) ne -1) and (total(clus_manage(8:9)) ne -2) ) then s_ROIPM_2dHistPlot, xy2DHistPosition = xy2DHistPosition
                  endif else begin
                   if (widget_info(clus_base[3], /valid)) then begin
                    widget_control, clus_base[3], tlb_get_Offset = xy2DHistPosition
                       widget_control, ev.top, get_uValue = state, /no_copy
                        state.xy2DHistPosition = xy2DHistPosition
                       widget_control, ev.top, set_uValue = state, /no_copy
                    widget_control, clus_base[3], /destroy
                    clus_base[3] = -1
                    clus_win[1] = 0
                    clus_manage[1] = 0
                       endif
                 endelse
         endcase
       '2DSURFACEONOFF': begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                     state.f2DSurfaceControl = s_ToggleButtonOnOffState(state.w2DSurfOnOff)
                  widget_control, ev.top, set_uValue = state, /no_copy
         endcase
       '3DHISTONOFF': begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                     state.f3DHistControl = s_ToggleButtonOnOffState(state.w3DHistOnOff)
                     f3DHistControl = state.f3DHistControl
                  widget_control, ev.top, set_uValue = state, /no_copy
                  if (f3DHistControl) then begin
                   if (clus_manage[8] eq -1) or (clus_manage[9] eq -1) or (clus_manage[10] eq -1) then a = s_apop_shout('x, Y oder Z - Dimension fehlt')
                   if ((size(allcl_parameter))[1] eq 0) then a = s_apop_shout('Parameter missing !')
                   if ( ((clus_manage[8] ne -1) or (clus_manage[9] ne -1) or (clus_manage[10] ne -1)) and ((size(allcl_parameter))[1] ne 0)) then s_apop_zellfl_3D
                  endif else begin
                   if (widget_info(clus_base[5], /valid)) then begin
                    widget_control, clus_base[5], /destroy
                    clus_base[5] = -1
                    clus_win[3] = 0
                    clus_manage[4] = 0
                     endif
                  endelse
         endcase
       'PRINCIPALCOMPONENTANALYSIS':begin
                  widget_control, ev.top, get_uValue = state, /no_copy
                     state.fPrincipalComponentControl = s_ToggleButtonOnOffState(state.wPrincipalComponentOnOff)
                     fPrincipalComponentControl = state.fPrincipalComponentControl
                        oAllParamList = state.oParamListContainer->get(position = clus_manage[15]+1)
                   oAllParamList->getProperty, strings = paramNames
                  widget_control, ev.top, set_uValue = state, /no_copy
                  if (fPrincipalComponentControl) then Data_Analisys_Main,  Allcl_Parameter, Allcl_Dimension, Clus_name, paramNames
         endcase
       'PLOTCLUSTERONOFF': begin
                 widget_control, clus_base[0], get_uValue = state, /no_copy
                   oAllParamList = state.oParamListContainer->get(position = 0)
                   oAllParamList->getProperty, strings = allParamList
                 widget_control, clus_base[0], set_uValue = state, /no_copy
                 if (clus_base[6] ne -1) then begin
                   if  (widget_info(clus_base[6],/valid)) then widget_control, clus_base[6], /destroy
                   clus_base[6] = -1
                   clus_win(7:*) = 0
                   clus_manage[21] = 0
                 endif else begin
                   clus_manage[21] = 1
                   if  (allParamList(clus_manage[8]) eq '#_SubClust') and  (total(clus_active) gt 0) then s_apop_cluster_plot
                 endelse
          endcase
       'PLOTCLUSTERTIMEON':  begin
                  if (ev.select eq 1) and (clus_base[7] eq -1) then s_apop_time_slider
                   if (ev.select eq 1) and (clus_base[6] ne -1) then $
                    if (widget_info(clus_base[6], /valid))  and (clus_base[7] ne -1)  then  if  (widget_info(clus_base[7], /valid) ne 1) then s_apop_time_slider
          endcase
         else:
        endcase

    ;Hand_Statistics
       if (ev.select eq 1) and (clus_manage[16]  eq 1) then begin
         if (value eq 'CLOSE_AREA(S)') then begin
          if ((size(clus_shade))[1] ne clus_manage[5]) or ((size(clus_shade))[2] ne clus_manage[6]) then $
          clus_shade = intArr(clus_manage[5],clus_manage[6])
          wset, clus_win[1]
          a = tvrd(71, 46, 259, 238, true = 1)
          a = congrid(a,clus_manage[5],clus_manage[6])
          a = (a eq (70+clus_manage[20]*15)) * a
          a = congrid(a,259,238)
          tv, clus_image + a*(clus_image eq 0) , 71,46
          for i = 0,9 do begin
          endfor
         endif
         if (value eq 'SAVE_AREA(S)') then  file = dialog_pickfile(/write, path = pfad , filter = '*.dat', file = 'file name without .tif !!!!', /noconf)
         if (value eq 'LOAD_AREA(S)') then file = dialog_pickfile(/read, path = pfad , filter = '*.dat', file = 'file name without .tif !!!!', /noconf)
       endif


    ; Parameter load
       if    (value eq 'OPENNEWDATASET') or (value eq 'PASSACTIVEDATASET') then begin

         if (value eq 'OPENNEWDATASET') then begin
          widget_control, ev.top, get_uValue = state, /no_copy
              paramPath = state.parameterPath
          widget_control, ev.top, set_uValue = state, /no_copy
               fileName = dialog_pickfile(/read, path = paramPath, get_path = path,  filter = ['*.sav'], /multiple)
           endif else fileName = s_getPathForSystem()+'obj.tmp'

         widget_control, ev.top, get_uValue = state, /no_copy
          oAllParamList = state.oParamListContainer->get(position = 0)
          oAllParamList->getProperty, strings = allParamList
          paramNames = allParamList
         widget_control, ev.top, set_uValue = state, /no_copy

            if (fileName[0] ne '') then begin
          widget_control, ev.top, get_uValue = state, /no_copy
              if (value eq 'OPENNEWDATASET') then state.parameterPath = path
          widget_control, ev.top, set_uValue = state, /no_copy
               if (n_elements(fileName) gt 1) then fileName[[0,n_elements(fileName)-1]] = fileName[ [n_elements(fileName)-1, 0] ]

                for j = 0, n_elements(fileName)-1 do begin

              restore, fileName[j], restored_objects = oStackTrackContainer, /relaxed
              oStackTrackContainer = oStackTrackContainer[0]
              if (value eq 'OPENNEWDATASET') then fileName[j] = strMid(fileName[j], 0, rStrPos(fileName[j],'.sav')) else fileName[j] = strMid(fileName[j], 0, rStrPos(fileName[j],'.dat'))
              if (obj_valid(oStackTrackContainer)) then begin
                 if (obj_class(oStackTrackContainer) eq 'C_SROIPARAM_STACKTRACKCONTAINER') then begin
                   paramNames = *(oStackTrackContainer->getpActiveTrackParamNameList())
                   pParamVect = ptrArr(n_elements(paramNames))
                   sizeVect = intArr(n_elements(paramNames))
                   for i = 0, n_elements(paramNames)-1 do begin
                    oStackTrackObj = oStackTrackContainer->getSelectedStackTrackObjFromParamName(paramName = paramNames[i])
                    pParamVect[i] = ptr_new(oStackTrackObj->getAllParamVect(), /no_copy)
                    sizeVect[i] = n_elements(*(pParamVect[i]))
                   endfor
                   obj_destroy, oStackTrackObj
                   whereMaxVect = where(sizeVect eq max(sizeVect))
                   paramNames = paramNames[whereMaxVect]
                   paramLimits = fltArr(4,n_elements(whereMaxVect))
                   paramMatrix = fltArr(n_elements(whereMaxVect), max(sizeVect))
                   for i = 0, n_elements(whereMaxVect)-1 do begin
                    paramMatrix[i,*] = *(pParamVect[whereMaxVect[i]])
                    paramLimits[*,i] = [min(paramMatrix[i,*]), min(paramMatrix[i,*]), max(paramMatrix[i,*]), max(paramMatrix[i,*]) ]
                   endfor
                 endif
                 for i = 0, n_elements(pParamVect)-1 do ptr_free, pParamVect[i]
                 obj_destroy, oStackTrackContainer
              endif

              clus_manage[14] = clus_manage[14] + 1
              clus_manage[15] = clus_manage[14] - 1

              a = fileName[j]+','
              if (clus_manage[14] eq 1) then begin
                 if (value eq 'OPENNEWDATASET') then clus_name = strMid(a,(rStrPos(a,path_sep()))+1,(rStrPos(a,','))-(rStrPos(a,path_sep()))-1) else clus_name = ev.name

                 widget_control, ev.top, get_uValue = state, /no_copy
                   state.oParamListContainer->remove, /all
                   state.oParamListContainer->add, obj_new('IDLgrText', paramNames), position = 0
                   state.oParamListContainer->add, obj_new('IDLgrText', paramNames), position = 1
                 widget_control, ev.top, set_uValue = state, /no_copy
                 allParamList = paramNames

                 if (size(paramMatrix, /n_dimension) eq 1) then begin
                   allcl_parameter = (fltArr(1,(size(paramMatrix))[1], (size(paramMatrix))[0]))[0,*,*] + paramMatrix
                   allcl_lenght = (intArr(1,2))[0,*] + [n_elements(paramNames),(size(paramMatrix))[0]]
                   allcl_dimension = (intArr(1,2))[0,*] + [(size(paramMatrix))[1],(size(paramMatrix))[0]]
                 endif else begin
                   allcl_parameter = (fltArr(1,(size(paramMatrix))[1], (size(paramMatrix))[2]))[0,*,*] + paramMatrix
                   allcl_lenght = (intArr(1,2))[0,*] + [n_elements(paramNames), (size(paramMatrix))[2]]
                   allcl_dimension = (intArr(1,2))[0,*] + [(size(paramMatrix))[1],(size(paramMatrix))[2]]
                 endelse
                 clus_active = 1
              endif else begin
                 if (value eq 'OPENNEWDATASET') then clus_name = [clus_name, strMid(a,(rStrPos(a,path_sep()))+1,(rStrPos(a,','))-(rStrPos(a,path_sep()))-1)] else clus_name = [clus_name, ev.name]
                 widget_control, ev.top, get_uValue = state, /no_copy
                   state.oParamListContainer->add, obj_new('IDLgrText', paramNames), position = clus_manage[14]
                   oAllParamList = state.oParamListContainer->get(position = 0)
                   oAllParamList->getProperty, strings = allParamList
                   for i = 0, n_elements(paramNames)-1 do if ((where(allParamList eq paramNames[i]))[0] eq -1) then allParamList = [allParamList, paramNames[i]]
                   oAllParamList->setProperty, strings = allParamList
                 widget_control, ev.top, set_uValue = state, /no_copy
                 a = allcl_parameter
                 allcl_parameter = fltArr(clus_manage[14],(size(a))[2]>(size(paramMatrix))[1],  (size(a))(3)>(size(paramMatrix))[2])
                 allcl_parameter(0:clus_manage[14]-2, 0:(size(a))[2]-1, 0:(size(a))(3)-1) = a
                 allcl_parameter(clus_manage[14]-1,0:(size(paramMatrix))[1]-1,0:(size(paramMatrix))[2]-1) = paramMatrix[*,*]
                 allcl_lenght = [allcl_lenght, transpose([n_elements(paramNames),(size(paramMatrix))[2]])]
                 allcl_dimension = [allcl_dimension,transpose([(size(paramMatrix))[1],(size(paramMatrix))[2]])]
                 clus_active = [clus_active, 1]
              endelse

              if (clus_manage[23] eq 0) and (clus_manage[15] ne -1) then begin
                 clus_limits = fltArr(n_elements(allParamList),6)
                 for i = 0, n_elements(paramNames)-1 do begin
                   k = (where(allParamList eq paramNames[i]))[0]
                   if (k ne -1) then begin
                    clus_limits(k,5) = clus_limits(k,5) > max(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                    if (clus_manage[14] eq 1) then if ((clus_limits(k,0))[0] eq 0) then clus_limits(k,0) = min(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                    clus_limits(k,0) = clus_limits(k,0) < min(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                    if ((moment(clus_limits(k,1:4)))[1] eq 0) then clus_limits(k,1:4) = [clus_limits(k,0),clus_limits(k,0),clus_limits(k,5),clus_limits(k,5)]
                   endif
                 endfor
              endif

               endfor   ;fileName[0]
         endif else i = s_apop_shout('Nothing loaded')    ;       fileName[0] ne ''

         ; update xyz-Parameter Lists
         widget_control, ev.top, get_uvalue = state, /no_copy
          widget_control, state.wXList, set_value = allParamList
          widget_control, state.wXList, get_uValue = uvalueListParameter, /no_copy
              if (ptr_valid(uvalueListParameter.value)) then ptr_free, uvalueListParameter.value
              uvalueListParameter.value = ptr_new(allParamList)
              clus_manage[8] = uvalueListParameter.active
              widget_control, state.wXList, set_list_select = uvalueListParameter.active
          widget_control, state.wXList, set_uValue = uvalueListParameter, /no_copy

          widget_control, state.wYList, set_value = allParamList
          widget_control, state.wYList, get_uValue = uvalueListParameter, /no_copy
              if (ptr_valid(uvalueListParameter.value)) then ptr_free, uvalueListParameter.value
              uvalueListParameter.value = ptr_new(allParamList)
              clus_manage[9] = uvalueListParameter.active
              widget_control, state.wYList, set_list_select = uvalueListParameter.active
          widget_control, state.wYList, set_uValue = uvalueListParameter, /no_copy

          widget_control, state.wZList, set_value = allParamList
          widget_control, state.wZList, get_uValue = uvalueListParameter, /no_copy
              if (ptr_valid(uvalueListParameter.value)) then ptr_free, uvalueListParameter.value
              uvalueListParameter.value = ptr_new(allParamList)
              clus_manage[10] = uvalueListParameter.active
              widget_control, state.wZList, set_list_select = uvalueListParameter.active
          widget_control, state.wZList, set_uValue = uvalueListParameter, /no_copy
         widget_control, ev.top, set_uvalue = state, /no_copy

         if (clus_manage[23] eq 0) and (clus_manage[15] ne -1) then begin
          for i = 0, n_elements(paramNames)-1 do begin
              j = (where(allParamList eq paramNames[i]))[0]
              if (j ne -1) then begin
                 clus_limits(j,5) = clus_limits(j,5) > max(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                 if (clus_manage[14] eq 1) then if ((clus_limits(j,0))[0] eq 0) then clus_limits(j,0) = min(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                 clus_limits(j,0) = clus_limits(j,0) < min(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                 if ((moment(clus_limits(j,1:4)))[1] eq 0) then clus_limits(j,1:4) = [clus_limits(j,0),clus_limits(j,0),clus_limits(j,5),clus_limits(j,5)]
              endif
          endfor
         endif
       endif; Parameter_Load


    ; Delete Param-Set
       if (value eq 'DELETEACTIVEDATASET') and (ev.select) then begin
         if (clus_manage[14] eq 0) then a = s_apop_shout('Parameter set missing !')
         if (clus_manage[14] eq 1) then begin
          clus_name = ''
          allParamList = ['-NO SELECTION-']
          widget_control, ev.top, get_uValue = state, /no_copy
              state.oParamListContainer->remove, /all
              state.oParamListContainer->add, obj_new('IDLgrText', allParamList), position = 0
          widget_control, ev.top, set_uValue = state, /no_copy
          allcl_parameter = ''
          allcl_lenght = ''
          clus_active = ''
          clus_manage[15] = -1
          clus_manage[14] = 0
         endif
         if (clus_manage[15] eq 0) and (clus_manage[14] gt 1)  then begin
          clus_name = clus_name(1:*)
          clus_active = clus_active(1:*)
          clus_active[0] = 1
          widget_control, ev.top, get_uValue = state, /no_copy
              oTest = state.oParamListContainer->get(position = 1)
              if (obj_valid(oTest)) then begin
                 state.oParamListContainer->remove, position = 1
                 obj_destroy, oTest
              endif
          widget_control, ev.top, set_uValue = state, /no_copy
          allcl_parameter = allcl_parameter(1:*,*,*)
          allcl_lenght = allcl_lenght(1:*,*)
          clus_manage[14] = clus_manage[14]-1
          clus_manage[15] = 0
         endif
         if (clus_manage[15] eq clus_manage[14]-1) and (clus_manage[14] gt 1) then begin
          widget_control, ev.top, get_uValue = state, /no_copy
              oTest = state.oParamListContainer->get(position = clus_manage[15]+1)
              if (obj_valid(oTest)) then begin
                 state.oParamListContainer->remove, position = clus_manage[15]+1
                 obj_destroy, oTest
              endif
          widget_control, ev.top, set_uValue = state, /no_copy
          clus_manage[14] = clus_manage[14]-1
          clus_manage[15] = 0
          clus_name = clus_name(0:clus_manage[14]-1)
          clus_active = clus_active(0:clus_manage[14]-1)
          clus_active[0] = 1
          allcl_parameter = allcl_parameter(0:clus_manage[14]-1,*,*)
          allcl_lenght = allcl_lenght(0:clus_manage[14]-1,*)
         endif
         if (clus_manage[15] lt clus_manage[14]-1) and (clus_manage[14] gt 1) and (clus_manage[15] ne 0) then begin
          clus_name = [clus_name(0:clus_manage[15]-1),clus_name(clus_manage[15]+1:*)]
          clus_active = [clus_active(0:clus_manage[15]-1),clus_active(clus_manage[15]+1:*)]
          clus_active[0] = 1
          widget_control, ev.top, get_uValue = state, /no_copy
              oTest = state.oParamListContainer->get(position = clus_manage[15]+1)
              if (obj_valid(oTest)) then begin
                 state.oParamListContainer->remove, position = clus_manage[15]+1
                 obj_destroy, oTest
              endif
          widget_control, ev.top, set_uValue = state, /no_copy
          allcl_parameter = [allcl_parameter(0:clus_manage[15]-1,*,*),allcl_parameter(clus_manage[15]+1:*,*,*)]
          allcl_lenght = [allcl_lenght(0:clus_manage[15]-1,*),allcl_lenght(clus_manage[15]+1:*,*)]
          clus_manage[14] = clus_manage[14]-1
          clus_manage[15] = 0
         endif
       endif

       widget_control, ev.top, get_uValue = state, /no_copy
         oAllParamList = state.oParamListContainer->get(position = 0)
         oAllParamList->getProperty, strings = allParamList
         nParams = n_elements(allParamList)
       widget_control, ev.top, set_uValue = state, /no_copy
       clus_manage[8] = clus_manage[8] < (nParams-1)
       clus_manage[9] = clus_manage[9] < (nParams-1)
       clus_manage[10] = clus_manage[10] < (nParams-1)

         ; Change Limits
       if ( (value eq 'SETLIMITSFROMSLIDERVALUES')or(value eq 'SETLIMITSFROMACTIVEDATASET')or(value eq 'SETLIMITSFROMALLDATASETS') ) $
       and (clus_manage[15] ne -1) and (ev.select eq 1) then begin

          ; Set Limits from  SLID
         if (value eq 'SETLIMITSFROMSLIDERVALUES') then begin
          if (clus_manage[8] ne -1) then begin
              clus_limits(clus_manage[8],[0,1,2]) = replicate(max(clus_limits(clus_manage[8],1:2)),3)
              clus_limits(clus_manage[8],[3,4,5]) = replicate(min(clus_limits(clus_manage[8],3:4)),3)
          endif
          if (clus_manage[9] ne -1) then begin
              clus_limits(clus_manage[9],[0,1,2]) = replicate(max(clus_limits(clus_manage[9],1:2)),3)
              clus_limits(clus_manage[9],[3,4,5]) = replicate(min(clus_limits(clus_manage[9],3:4)),3)
          endif
          if (clus_manage[10] ne -1) then begin
              clus_limits(clus_manage[10],[0,1,2]) = replicate(max(clus_limits(clus_manage[10],1:2)),3)
              clus_limits(clus_manage[10],[3,4,5]) = replicate(min(clus_limits(clus_manage[10],3:4)),3)
          endif
         endif
    ; Set Limits from Active Data Set
         if (value eq 'SETLIMITSFROMACTIVEDATASET') then begin
          widget_control, ev.top, get_uValue = state, /no_copy
              oAllParamList = state.oParamListContainer->get(position = 0)
              oAllParamList->getProperty, strings = allParamList
              oAllParamList = state.oParamListContainer->get(position = clus_manage[15] + 1)
              oAllParamList->getProperty, strings = paramNames
          widget_control, ev.top, set_uValue = state, /no_copy

          for i = 0,n_elements(paramNames)-1 do begin
              j = (where(allParamList eq paramNames[i]))[0]
              if (j[0] ne -1) then begin
                 clus_limits(j,0) = min(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                 clus_limits(j,5) = max(allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1))
                 clus_limits(j,1:4) = [clus_limits(j,0) > clus_limits(j,1),  clus_limits(j,0) > clus_limits(j,2),$
                         clus_limits(j,5) < clus_limits(j,3),  clus_limits(j,5) < clus_limits(j,4)]
              endif
          endfor
         endif
    ; Set Limits from all Data Sets
         if (value eq 'SETLIMITSFROMALLDATASETS') then begin
          widget_control, ev.top, get_uValue = state, /no_copy
              oAllParamList = state.oParamListContainer->get(position = 0)
              oAllParamList->getProperty, strings = allParamList
          widget_control, ev.top, set_uValue = state, /no_copy
          for k = 0,clus_manage[14]-1 do for i = 0,allcl_lenght(k,0)-1 do begin

              widget_control, ev.top, get_uValue = state, /no_copy
                 oAllParamList = state.oParamListContainer->get(position = k + 1)
                 oAllParamList->getProperty, strings = paramNames
              widget_control, ev.top, set_uValue = state, /no_copy

              j = (where(allParamList eq paramNames[i]))[0]
              if (j[0] ne -1) then begin
                 if (k eq 0) then begin
                   clus_limits(j,0) = min(allcl_parameter(k,i,0:allcl_lenght(k,1)-1))
                   clus_limits(j,5) = max(allcl_parameter(k,i,0:allcl_lenght(k,1)-1))
                   clus_limits(j,1:4) = [clus_limits(j,0) > clus_limits(j,1), clus_limits(j,0) > clus_limits(j,2),$
                                clus_limits(j,5) < clus_limits(j,3),  clus_limits(j,5) < clus_limits(j,4)]
                 endif else begin
                   clus_limits(j,0) = clus_limits(j,0) < min(allcl_parameter(k,i,0:allcl_lenght(k,1)-1))
                   clus_limits(j,5) = clus_limits(j,5) > max(allcl_parameter(k,i,0:allcl_lenght(k,1)-1))
                   clus_limits(j,1:4) = [clus_limits(j,0) > clus_limits(j,1), clus_limits(j,0) > clus_limits(j,2),$
                                clus_limits(j,5) < clus_limits(j,3),  clus_limits(j,5) < clus_limits(j,4)]
                 endelse
              endif
            endfor
          endif
       endif

    ; Change Limits individually
       if ((value eq 'SETLIMITSFROMCHOOSEMANUALLY') and (clus_manage[15] ne -1) and (ev.select eq 1)) then s_apop_axislimits
    endcase
    'DRAW': begin
          value = string(FORMAT = "(a,' x(', I0, ')', ' Y(', I0, ')', ' Press(',' + 'I0, ')', ' Release(', I0, ')')", value, ev.x, ev.y, ev.press, ev.release)
          if (clus_manage[16] eq 1) then begin
              if ((size(clus_shade))[1] ne clus_manage[5]) or ((size(clus_shade))[2] ne clus_manage[6]) then $
              clus_shade = intArr(clus_manage[5],clus_manage[6])
              wset, clus_win[1]
              cursor, x1, y1, /device,/nowait
                if (ev.press ne 0) then while (!ERR eq 1) do begin
                 cursor, x1, y1, /device, /nowait
                 if (x1 ge 70) and (x1 le 331) and (y1 ge 45) and (y1 le 285) and (!ERR eq 1) then begin
                   clus_shade( (round((-70. + x1)*clus_manage[5]/260.)-1)>0, (round((-45. + y1)*clus_manage[6]/240.)-1)>0) = 70+clus_manage[20]*15
                   tv,  clus_image + congrid(clus_shade,259,238 ), 71, 46
                 endif
              endwhile
          widget_control, ev.id, /clear_events
          endif
    endcase
    'SLID': begin
          case (value) of
              'X-Intervall':   clus_manage[5] = ev.value
              'Y-Intervall':   clus_manage[6] = ev.value
              'z-Intervall':   clus_manage[7] = ev.value
              'X-ROT':   clus_manage[12] = ev.value
              'z-ROT':   clus_manage[13] = ev.value
          endcase
    endcase
    else: begin
          widget_control, ev.top, get_uValue = state, /no_copy
              oAllParamList = state.oParamListContainer->get(position = 0)
              oAllParamList->getProperty, strings = allParamList
              oAllParamList = state.oParamListContainer->get(position = clus_manage[15]+1)
              oAllParamList->getProperty, strings = paramNames
          widget_control, ev.top, set_uValue = state, /no_copy

          if(n_elements(value) ge 1) then if (value eq 'Table') then begin
              for i = 0,clus_manage[14]-1 do begin
                 if (round(float(strMid(ev.value,0,(strPos(ev.value,':'))))) eq i+1) then begin
                   if (ev.select) then begin
                    clus_active[i] = 1
                    clus_manage[15] = i
                   endif
                   if (ev.select eq 0) then begin
                    clus_active[i] = 0
                    if (total(clus_active) eq 0) then clus_active[0] = 1
                    clus_manage[15] = (where(clus_active eq 1))[0]
                   endif
                   widget_control, ev.top, get_uValue = state, /no_copy
                    widget_control, state.wTopBase, tlb_set_title = 's_Param |-> '+ strCompress(string('Active: ',fix (clus_manage[15]+1),'.',clus_name(clus_manage[15]) ))
                   widget_control, ev.top, set_uValue = state, /no_copy
                 endif
            endfor
         endif

         if (strMid(value,0,8) eq 'X_ADQUIS') then begin
          whParamActive = where(paramNames eq allParamList( clus_manage[8]))
          if (value eq 'X_ADQUIS_MULTI') then $
              allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1) = $
              ev.value * allcl_parameter(  clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)
          if (value eq 'X_ADQUIS_ADDI') then $
              allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1) = $
              ev.value + allcl_parameter(  clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)
          clus_limits( clus_manage[8], 0:2) = min ( allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)  )
          clus_limits( clus_manage[8], 3:5) = max ( allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)  )
         endif
         if (strMid(value,0,8) eq 'Y_ADQUIS') then begin
          whParamActive = where(paramNames eq allParamList( clus_manage[9]))
          if (value eq 'Y_ADQUIS_MULTI') then $
              allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1) = $
              ev.value * allcl_parameter(  clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)
          if (value eq 'Y_ADQUIS_ADDI') then $
              allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1) = $
              ev.value + allcl_parameter(  clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)
          clus_limits( clus_manage[9], 0:2) = min ( allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)  )
          clus_limits( clus_manage[9], 3:5) = max ( allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)  )
         endif
         if (strMid(value,0,8) eq 'Z_ADQUIS') then begin
          whParamActive = where(paramNames eq allParamList( clus_manage[10]))
          if (value eq 'Z_ADQUIS_MULTI') then $
              allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1) = $
              ev.value * allcl_parameter(  clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)
          if (value eq 'Z_ADQUIS_ADDI') then $
              allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1) = $
              ev.value + allcl_parameter(  clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)
          clus_limits( clus_manage[10], 0:2) = min ( allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)  )
          clus_limits( clus_manage[10], 3:5) = max ( allcl_parameter( clus_manage[15], whParamActive, 0:allcl_lenght( clus_manage[15], 1)-1)  )
         endif

       endcase
    endcase

    ; 2D-Surface
    if (clus_manage[3] eq 1) then begin
       widget_control, ev.top, get_uValue = state, /no_copy
         oAllParamList = state.oParamListContainer->get(position = 0)
         oAllParamList->getProperty, strings = allParamList
         oAllParamList = state.oParamListContainer->get(position = clus_manage[15]+1)
         oAllParamList->getProperty, strings = paramNames
       widget_control, ev.top, set_uValue = state, /no_copy

       wset, clus_win[2]
       i = (where(paramNames eq allParamList(clus_manage[8])) )[0]
       j = (where(paramNames eq allParamList(clus_manage[9])) )[0]
       tvlct, r, g, b, /get
       tvlct, bindgen(255), bindgen(255), bindgen(255)

       shade_surf, congrid( hist_2d( round( $
       (allcl_parameter(clus_manage[15],i,0:allcl_lenght(clus_manage[15],1)-1) - clus_limits(clus_manage[8],0) ) $
       / (clus_limits(clus_manage[8],5)-clus_limits(clus_manage[8],0)) * clus_manage[5] ) ,$
         round( $
       (allcl_parameter(clus_manage[15],j,0:allcl_lenght(clus_manage[15],1)-1) - clus_limits(clus_manage[9],0) ) $
       / (clus_limits(clus_manage[9],5)-clus_limits(clus_manage[9],0)) * clus_manage[6] ) ) ,$
          clus_manage[5]*5, clus_manage[6] *5 ) ,$
          AX = clus_manage[12] ,$
          AZ = clus_manage[13] ,$
          Xtitle = allParamList(clus_manage[8]),$
          Ytitle = allParamList(clus_manage[9]),$
          Ztitle = 'COUNTS', charSize = 1.6
       tvlct, g, r, b

       xx = -1
       yy = -1
       whereDataSetActive = where(clus_active eq 1)
       for k = 0,n_elements(whereDataSetActive)-1 do if (whereDataSetActive[k] ne clus_manage[15])   then begin
         widget_control, ev.top, get_uValue = state, /no_copy
          oAllParamList = state.oParamListContainer->get(position = whereDataSetActive[k]+1)
          oAllParamList->getProperty, strings = paramNames
         widget_control, ev.top, set_uValue = state, /no_copy
         num_x = (where(paramNames eq allParamList(clus_manage[8])) )[0]
         num_y = (where(paramNames eq allParamList(clus_manage[9])) )[0]
             if (xx eq -1) then xx = allcl_parameter(whereDataSetActive[k],num_x,0:allcl_lenght(whereDataSetActive[k],1)-1) $
             else xx = [xx,allcl_parameter(whereDataSetActive[k],num_x,0:allcl_lenght(whereDataSetActive[k],1)-1)]
             if (yy eq -1) then yy = allcl_parameter(whereDataSetActive[k],num_y,0:allcl_lenght(whereDataSetActive[k],1)-1) $
             else yy = [yy,allcl_parameter(whereDataSetActive[k],num_y,0:allcl_lenght(whereDataSetActive[k],1)-1)]
        endif

       shade_surf, congrid( hist_2d( round( (xx - clus_limits(clus_manage[8],0) ) $
       / (clus_limits(clus_manage[8],5)-clus_limits(clus_manage[8],0)) * clus_manage[5] ) ,$
         round( (yy - clus_limits(clus_manage[9],0) ) $
       / (clus_limits(clus_manage[9],5)-clus_limits(clus_manage[9],0)) * clus_manage[6] ) ) ,$
          clus_manage[5]*5, clus_manage[6] *5 ) ,$
              / noErase, AX = clus_manage[12] ,  AZ = clus_manage[13]
    endif

    if (widget_info(clus_base[7],/valid)) then if (strMid(value,0,4) eq 'CLUS') then begin
       zahl = (strCompress(sindgen(total(clus_active)+2),/remove_all))(1:total(clus_active)+1)
       for i = 0,total(clus_active) do if (strMid(value,9,1) eq zahl(i)) then clusTime(i) = ev.value
    endif

    whereUpdate = (where(tag_names(ev) eq 'UPDATE'))[0]
    if (whereUpdate ne -1) then update = ev.update else update = 1

       ; update Child_Widgets
    if (update) then begin
       widget_control, ev.top, get_uValue = state, /no_copy
         wTopBase = state.wTopBase
         child_DataTableWidget_tlb = state.child_DataTableWidget_tlb
       widget_control, ev.top, set_uValue = state, /no_copy
       if (widget_info(child_DataTableWidget_tlb, /valid)) and (clus_manage[14] gt 0) then begin
         s_ROIPM_PT_Cleanup, child_DataTableWidget_tlb
         widget_control, ev.top, get_uValue = state, /no_copy
          viewDataTablePosition = state.viewDataTablePosition
         widget_control, ev.top, set_uValue = state, /no_copy
         s_ROIPM_paramTable, groupLeader = wTopBase, application_tlb = application_tlb, viewDataTablePosition = viewDataTablePosition
         widget_control, ev.top, get_uValue = state, /no_copy
          state.child_DataTableWidget_tlb = application_tlb
          clus_base[2] = application_tlb
          if not(state.fViewDataTableControl) then state.fViewDataTableControl = s_ToggleButtonOnOffState(state.wViewDataTableOnOff)
         widget_control, ev.top, set_uValue = state, /no_copy
       endif
       if (clus_manage[0]) and (clus_manage[14] gt 0) then begin
         if (clus_manage[8] ne -1) then s_apop_zellhist_1D, clus_manage[8], clus_win[4], clus_manage[5], [clus_g_style[0],clus_g_style[3:*]]
         if (clus_manage[9] ne -1) then s_apop_zellhist_1D, clus_manage[9], clus_win[5], clus_manage[6], [clus_g_style[1],clus_g_style[3:*]]
         if (clus_manage[10] ne -1) then s_apop_zellhist_1D, clus_manage[10], clus_win[6], clus_manage[7], [clus_g_style[2],clus_g_style[3:*]]
       endif
       if (clus_manage[1]) and (clus_manage[14] gt 0) then begin
         wset, clus_win[1]
         erase
         s_ROIPM_2dHistPlot, nonew = 1
         if ((clus_g_style[15]  lt 11) and (clus_g_style[7] eq 1)) then  apop_2D_param
       endif
       if (clus_manage[4])  and (clus_manage[14] gt 0) then s_apop_zellfl_3D,  nonew = 1
       if (clus_manage[15] ne -1) then begin
         widget_control, ev.top, get_uValue = state, /no_copy
          widget_control, state.wTopBase, tlb_set_title = 's_Param |-> '+ strCompress(string('Active: ',fix (clus_manage[15]+1),'.',clus_name(clus_manage[15]) ))
         widget_control, ev.top, set_uValue = state, /no_copy
       endif
    endif ;update

    widget_control, ev.top, get_uValue = state, /no_copy
       tvlct,  (state.oldCT)[0,*], (state.oldCT)[1,*], (state.oldCT)[2,*]
    widget_control, ev.top, set_uValue = state, /no_copy
    print, 'name:',name,'value:', value
end
