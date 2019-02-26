;_____________________________IOISIOI____________________
; NAME:
;       s_Surface_Method_ZoomCutImage
;
; PURPOSE:
;       Cut & Zoom Image
;
; AUTHOR:
;		Dr. Steffen Härtel (2001)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;       	s_Surface_Method_ZoomCutImage, image
;
;  KEYWORD PARAMETERS:
;		   None
;_____________________________IOISIOI____________________
;+
; NAME:
;       s_Surface_Method_ZoomCutImage
;
; PURPOSE:
;
;       The purpose of this program is to display an image which
;       can be zoomed by drawing a rubberband box on top of it. The
;      "zoomed" image appears in its own window.
;
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;      Image Processing, Widgets.
;
; CALLING SEQUENCE:
;
;      s_Surface_Method_ZoomCutImage, image
;
; INPUTS:
;
;      image:     A 2D array of image data.
;
; KEYWORD PARAMETERS:
;
;       BOTTOM:   The lowest color index of the colors to be used (see
;                 NCOLORS). The default is 0.
;
;       COLORINDEX: The color index for the rubberband box. This index will
;                 be loaded with a green color. Whatever color is there will
;                 be restored when the ZIMAGE program exits. The default is
;                 NCOLORS + BOTTOM.
;
;       NCOLORS:  This is the number of colors to use when displaying the
;                 image. The default is !D.N_COLORS-2.
;
;       GROUP_LEADER: This keyword is used to assign a group leader to this
;                 program. This program will be destroyed when the group
;                 leader is destroyed. Use this keyword if you are calling
;                 ZIMAGE from another widget program.
;
;       NOINTERPOLATION: Setting this keyword causes nearest neighbor resampling of
;                 of the zoomed image instead of the default bilinear interpolation
;                 of resampled pixels.
;
;
; OUTPUTS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;
;       The COLORINDEX color is turned to green while the rubberband box
;       is being drawn. The color is restored after the box is drawn.
;
; RESTRICTIONS:
;
;       Uses XCOLORS from the Coyote Library:
;          http://www.dfanning.com/programs/xcolors.pro
;
; PROCEDURE:
;
;       Clicking the left mouse button allows you to drag a rubberband box
;       over the portion of the window you want to zoom into.
;
;       Clicking the right mouse button calls up hidden controls that allow
;       you to load different color tables and set the zoom factor.
;
;       The rubberband box is drawn with pixmaps and the "device copy"
;       technique.
;
;       This is an excellent example of how you can take advantage of the
;       widget program *as* the loop do to something (i.e., draw the box)
;       that in a regular IDL program would have to be done in a loop. Motion
;       events are only turned on for the draw widget when the box has to be
;       drawn.
;
; EXAMPLE:
;
;        To display an image you can zoom into, type:
;
;        ZIMAGE, image
;
; MODIFICATION HISTORY:
;
;        Written by: David Fanning, 15 August 96.
;        Fixed a !D.N_Colors problem. 17 June 98.
;        Made modifications so program works in 24-bit environment. 28 July 98. DWF.
;        Fixed a problem with the pop-up controls under certain circumstances.
;          13 Oct 98. DWF.
;        Added 24-bit color response. 13 Oct 98. DWF.
;        Added ability for each window to have its own color changing tool. 9 Oct 99. DWF.
;        Small changes, error checking. 24 April 2000. DWF.
;        Modified draw widget error handling to be consistent with current programming
;         practices. 26 April 2001. DWF.
;-
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 1999-2001 Fanning Software Consulting
;
; This software is provided "as-is", without any express or
; implied warranty. In no event will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. If you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; For more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################

PRO CUT_CONTROL, event

     ; Event handler to select surface style.
Widget_Control, event.top, Get_UValue=info, /No_Copy
     ; Event handler to select Button UValue.
Widget_Control, event.id, Get_Value=ButtonValue, /No_Copy
CASE ButtonValue OF
	'Cut Mode': BEGIN
			info.CutControl = 1 - info.CutControl					; Switch Flag (1/0 ON/OFF) for Cut Control
			Widget_Control, info.cutID, sensitive = 0
			Widget_Control, info.zoomID, sensitive = 1
			Widget_Control, info.returnID, sensitive = 1
			Widget_Control, info.zoomfactorID, sensitive = 0
 		ENDCASE
	'Zoom Mode': BEGIN
			info.CutControl = 1 - info.CutControl					; Switch Flag (1/0 ON/OFF) for Cut Control
			Widget_Control, info.cutID, sensitive = 1
			Widget_Control, info.zoomID, sensitive = 0
			Widget_Control, info.returnID, sensitive = 0
			Widget_Control, info.zoomfactorID, sensitive = 1
 		ENDCASE
ENDCASE

    ;Put the info structure back.
Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-CUT_CONTROL, event-----------------------------------------------------------------shaertel-2001-



PRO ZIMAGE_COLORS_SM, event

Widget_Control, event.top, Get_UValue=info, /No_Copy
    ; What kind of event is this?

thisEvent = Tag_Names(event, /Structure)
CASE thisEvent OF
    'WIDGET_BUTTON': BEGIN
	        TVLCT, info.r, info.g, info.b, info.bottom
	        XColors, Group=event.top, NColors = info.ncolors,$
	            Bottom=info.bottom, NotifyID=[event.id, event.top],$
	            Title='ZImage Colors (' + StrTrim(info.drawIndex,2) + ')'
	        Widget_Control, info.controlID, Map=0
	        info.map = 0
        ENDCASE
    'XCOLORS_LOAD':BEGIN

	            ; Extract the new color table vectors from XCOLORS.
	        info.r = event.r[info.bottom:info.bottom+info.ncolors-1]
	        info.g = event.g[info.bottom:info.bottom+info.ncolors-1]
	        info.b = event.b[info.bottom:info.bottom+info.ncolors-1]

	        Device, Get_Visual_Depth=thisDepth
	        IF thisDepth GT 8 THEN BEGIN

	                ; Redisplay the image.
	            WSet, info.drawIndex
	            TV, BytScl(info.image, Top=info.ncolors-1) + info.bottom
	            WSet, info.pixIndex
	            Device, Copy=[0, 0, info.xsize, info.ysize, 0, 0, info.drawIndex]

	                ; Is a zoom window open? If so, redisplay it as well.
	            IF Widget_Info(info.zoomDrawID, /Valid_ID) THEN BEGIN
	                WSet, info.zoomWindowID
	                TV, *info.subimage
	            ENDIF

	           ENDIF
        ENDCASE
ENDCASE
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;*******************************************************************



PRO ZIMAGE_QUITTER, event
     ; Event handler to select surface style.
Widget_Control, event.top, Get_UValue=info, /No_Copy
     ; Event handler to select Button UValue.
Widget_Control, event.id, Get_UValue=ButtonUValue, /No_Copy

IF (ButtonUValue EQ 'RETURNCUT') THEN BEGIN
	(*info.pPassSInfo).ReducedSizeXleft = info.ReducedSizeXleft
	(*info.pPassSInfo).ReducedSizeXright =  info.ReducedSizeXright
	(*info.pPassSInfo).ReducedSizeYupper =  info.ReducedSizeYupper
	(*info.pPassSInfo).ReducedSizeYlower =  info.ReducedSizeYlower

	s_Surface_Analysis, *(info.subimage)

ENDIF

Widget_Control, event.top, /Destroy
END ;*******************************************************************



PRO ZIMAGE_CLEANUP, tlb

   ; The purpose of this program is to delete the pixmap window
   ; when the program ZIMAGE is destroyed. Get the info structure,
   ; which holds the pixmap window index number and delete the window.
	Widget_Control, tlb, Get_UValue=info, /No_Copy
	IF N_Elements(info) NE 0 THEN BEGIN
	    WDelete, info.pixIndex
	    Ptr_Free, info.subimage
	ENDIF
END ; **ZIMAGE_CLEANUP **********************************************************



PRO ZIMAGE_FACTOR, event

	   ; The purpose of this event handler is to set the zoom factor.
	Widget_Control, event.top, Get_UValue=info, /No_Copy
	Widget_Control, event.id, Get_UValue=factor
	info.zoomfactor = factor(event.index)
	Widget_Control, info.controlID, Map=0
	info.map = 0
	Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; **ZIMAGE_FACTOR **********************************************************



PRO ZIMAGE_BUTTON_DOWN, event

   ; This event handler ONLY responds to button down events from the
   ; draw widget. If it gets a DOWN event, it does three things: (1) sets
   ; the static and dynamic corners of the zoom box, (2) changes the
   ; event handler for the draw widget to ZOOMBOX_DRAWBOX and turns on MOTION
   ; events, and (3) takes over color index 1 of the color table for the
   ; zoom box drawing color.
   ; Put the info structure back into its storage location.

	Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; of ZIMAGE_BUTTON_DOWN *****************************************************



PRO ZIMAGE_DRAW_EVENTS, event

   ; This event handler continuously draws and erases the zoom box until it
   ; receives an UP event from the draw widget. Then it turns draw widget
   ; motion events OFF.

   ; Get the info structure out of the top-level base.
Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; What type of an event is this?
possibleEventTypes = [ 'PRESS', 'RELEASE', 'MOTION', 'SCROLL' ]
thisEvent = possibleEventTypes[event.type]

CASE thisEvent OF
   'PRESS': BEGIN
		    ; Is this the left or right button?
		    ; If RIGHT, then map or unmap controls.
		   buttonPressed = What_Button_Pressed(event)
		   IF buttonPressed EQ 'RIGHT' THEN BEGIN
		   	info.map = 1 - info.map
	        	Widget_Control, info.controlID, Map = info.map
			Widget_Control, event.top, Set_UValue=info, /No_Copy
		     RETURN
		   ENDIF

		    ; Set the static corners of the box to current
		    ; cursor location.
		   info.xs = event.x
		   info.ys = event.y

		    ; Turn draw MOTION events ON.
		   Widget_Control, event.id, Draw_Motion_Events=1

		   ; Take over the color index for the zoom box drawing color. Store the
		   ; current (r,g,b) values for color index so you can restore the
		   ; current colors after the zoom box is drawn.
		   TVLct, r, g, b, /Get
		   info.r_old = r[info.colorIndex]
		   info.g_old = g[info.colorIndex]
		   info.b_old = b[info.colorIndex]
	   ENDCASE
   'RELEASE': BEGIN
		    ; Is this the left or right button?
		    ; If RIGHT, then do nothing.
		   buttonReleased = What_Button_Released(event)
		   IF buttonReleased EQ 'RIGHT' THEN BEGIN
		      Widget_Control, event.top, Set_UValue=info, /No_Copy
		      RETURN
		   ENDIF

		      ; If this is an UP event, you need to erase the zoombox, restore
		      ; the user's color table, turn motion events OFF, and
		      ; draw the "zoomed" plot in both the draw widget and the pixmap.
		      ; Erase the zoombox one final time by copying the plot from the pixmap.
		   WSet, info.drawIndex
		   TVLCT, info.r, info.g, info.b
;		   Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

		      ; Restore the user's color table.
		   TVLct, info.r_old, info.g_old, info.b_old, info.colorIndex

		      ; Turn motion events off.
		    Widget_Control, event.id, Draw_Motion_Events=0

		      ; Draw the "zoomed" image. Start by getting the LAST zoom
		      ; box outline. These are indices into image array.
		   event.x = 0 > event.x < (info.xsize - 1)
		   event.y = 0 > event.y < (info.ysize - 1)
		   x = [info.xs, event.x]
		   y = [info.ys, event.y]

		      ; Make sure the user didn't just click in the window.
		   IF info.xs EQ event.x OR info.ys EQ event.y THEN BEGIN
		      Widget_Control, event.top, Set_UValue=info, /No_Copy
		      RETURN
		   ENDIF

		      ; Make sure the x and y values are ordered as [min, max].
		   IF info.xs GT event.x THEN x = [event.x, info.xs]
		   IF info.ys GT event.y THEN y = [event.y, info.ys]

			IF (info.CutControl EQ 1) THEN BEGIN			; Decide if the image has to be cut or zoomed
				zoomFactor = 1
				info.ReducedSizeXleft = x[0]		; Left x coordinate of the Surface chosen for further Analysis
				info.ReducedSizeXright = x[1]		; Right x coordinate of the Surface chosen for further Analysis
   				info.ReducedSizeYupper =	y[0]		; Upper y coordinate of the Surface chosen for further Analysis
   				info.ReducedSizeYlower = y[1]		; Lower y coordinate of the Surface chosen for further Analysis
				wTitle = StrCompress('Cutted Image Dimensions ('+ String(x[1]-x[0]) +' [x], '+ String(y[1]-y[0]) + ' [y] )')
			ENDIF ELSE BEGIN
				zoomFactor = info.zoomFactor
				wTitle = StrCompress('Zoomed Image Dimensions ('+ String(x[1]-x[0]) +' [x], '+ String(y[1]-y[0]) + ' [y] )')
			ENDELSE

		      ; Set the zoom factor and determine the new X and Y sizes of the Zoom Window.
		   zoomXSize = (x[1] - x[0] + 1) * zoomFactor
		   zoomYSize = (y[1] - y[0] + 1) * zoomFactor

		      ; Subset the image, and apply the zoom factor to it.
		   imageSubset = info.image(x[0]:x[1], y[0]:y[1])
		   zoomedImage = Congrid(imageSubset, zoomXSize, zoomYSize, Interp=info.interp)

		      ; If the Zoom Window exists, make it the proper size and load
		      ; the zoomed image into it. If it does not exists, create it.
		   IF Widget_Info(info.zoomDrawID, /Valid_ID) THEN BEGIN

		         ; Zoomed window exists. Make it correct size and load image.
		      Widget_Control, info.zoomDrawID, XSize=zoomXSize, YSize=zoomYSize, TLB_Set_Title = wTitle
		      WSet, info.zoomWindowID
		      IF Ptr_Valid(info.subimage) THEN *info.subimage = BytScl(zoomedImage, Top=info.ncolors-1,$
															            Max=Max(info.image), Min=Min(info.image)) + info.bottom ELSE $
															         info.subimage = Ptr_New(BytScl(zoomedImage, Top=info.ncolors-1,$
															            Max=Max(info.image), Min=Min(info.image)) + info.bottom)
		      TV, *info.subimage

		   ENDIF ELSE BEGIN

		         ; Get offset positions for the non-existing zoom window.
		      Widget_Control, event.top, TLB_Get_Size=sizes, TLB_Get_Offset=offsets
		      xpos = sizes[0] + offsets[0] + 20
		      ypos = offsets[1] + 40

		         ; Zoom window does not exist. Create it.
		      zoomtlb = Widget_Base(Title = wTitle, Group=event.top, XOffset=xpos, YOffset=ypos)
		      zoomdraw = Widget_Draw(zoomtlb, XSize=zoomXSize, YSize=zoomYSize)
		      Widget_Control, zoomtlb, /Realize
		      Widget_Control, zoomdraw, Get_Value=windowID
		      info.zoomDrawID = zoomdraw
		      info.zoomWindowID = windowID
		      WSet, windowID
		      IF Ptr_Valid(info.subimage) THEN  *info.subimage = BytScl(zoomedImage, Top=info.ncolors-1,$
										            Max=Max(info.image), Min=Min(info.image)) + info.bottom ELSE $
										         info.subimage = Ptr_New(BytScl(zoomedImage, Top=info.ncolors-1,$
										            Max=Max(info.image), Min=Min(info.image)) + info.bottom)
		      TV, *info.subimage
		    ENDELSE

		      ; If the controls were mapped, unmap them.
		   IF info.map EQ 1 THEN BEGIN
		      Widget_Control, info.controlID, Map=0
		      info.map = 0
		   ENDIF

  	 ENDCASE
   'MOTION': BEGIN

	   ; Most of the action in this event handler occurs here while we are waiting
	   ; for an UP event to occur. As long as we don't get it, keep erasing the
	   ; old zoom box and drawing a new one.

	   ; Erase the old zoom box.
	   WSet, info.drawIndex
	   TVLCT, info.r, info.g, info.b, info.bottom
	   Device, Copy = [0, 0, info.xsize, info.ysize, 0, 0, info.pixIndex]

	   ; Update the dynamic corner of the zoom box to the current cursor location.
	   info.xd = event.x
	   info.yd = event.y

	   ; Load a green color in color index 1 to draw the zoom box with.
	   TVLCT, 0B, 255B, 0B, info.colorIndex

	   ; Draw the zoom box.
	   PlotS, [info.xs, info.xs, info.xd, info.xd, info.xs],$
	       [info.ys, info.yd, info.yd, info.ys, info.ys],$
	       /Device, Color=info.colorIndex
   ENDCASE

ENDCASE

   ; Put the info structure back into its storage location.
Widget_Control, event.top, Set_UValue=info, /No_Copy
END ; of ZIMAGE_DRAW_EVENTS ******************************************************************



PRO s_Surface_Method_ZoomCutImage, image, ColorIndex = colorIndex, Bottom=bottom,$
   Group_Leader = group_leader, NColors = ncolors, NoInterpolation=interp,$
   pSurfaceInfo = pSurfaceInfo

   ; This procedure allows the user to "zoom" into an image
   ;  by drawing a box around the part of the image to zoom into.
   ; The zoom box will be drawn and erased by using a pixmap and
   ; the "Device Copy" technique.
   ; It also allows the user to "cut" an image
   ;  by drawing a box around the part of the image to cut it.
   ;  Modification by Steffen Härtel for s_Surface_Analysis 2001.

   ; On an error condition, return to the main level of IDL.
On_Error, 1

   ; Was an image passed into the procedure?
   ; If not, find one in the Examples directory.
IF N_Params() EQ 0 THEN BEGIN
   filename = Filepath(SubDirectory=['examples','data'], 'worldelv.dat')
   OpenR, lun, filename, /Get_Lun
   image = BytArr(360,360)
   ReadU, lun, image
   Free_Lun, lun
ENDIF

   ; Make sure a window has been opened in this IDL session for
   ; accurate color number determination.

Window, /Pixmap, XSize=10, YSize=10
WDelete, !D.Window

   ; Check for keywords. Set defaults to size of image if necessary.

imageSize = Size(image)
IF imageSize[0] NE 2 Then Message, 'Image parameter must be 2D.'
xsize = imageSize[1]
ysize = imageSize[2]
IF N_Elements(ncolors) EQ 0 THEN ncolors = (!D.N_Colors - 2) < 254
IF N_Elements(bottom) EQ 0 THEN bottom = 0B
IF N_Elements(colorIndex) EQ 0 THEN colorIndex = (ncolors + bottom) < 255
interp = 1 - Keyword_Set(interp)

   ; Works with 2D images.
Device, Decomposed=0

   ; Create a top-level base for this program. No resizing of this base.
tlb = Widget_Base(TLB_Frame_Attr=1)

   ; Create two bases. One for controls and the other for the
   ; draw widget. Leave the control base unmapped for now.

controlID = Widget_Base(tlb, Map=0, Column=1)
modeID = Widget_Button(controlID, Value='Operation Mode', /Menu)
cutID = Widget_Button(modeID, Value='Cut Mode', UValue = 'CUTMODE', Event_Pro='CUT_CONTROL', Sensitive = 0)
zoomID = Widget_Button(modeID, Value='Zoom Mode', UValue = 'ZOOMMODE', Event_Pro='CUT_CONTROL')

colors = Widget_Button(controlID, Value='Load Colors', Event_Pro='ZIMAGE_COLORS_SM')
factorString = ['2x', '3x', '5x', '8x']
factors = [2, 3, 5, 8]
zoomfactorID = Widget_DropList(controlID, Value=factorString, Event_Pro='ZIMAGE_FACTOR', UValue=factors, Title='Zoom Factor', Sensitive = 0)

exitID = Widget_Button(controlID, Value='Exit', /Menu)
returnID = Widget_Button(exitID, Value='Exit and Return Cut Values', UValue = 'RETURNCUT', Event_Pro='ZIMAGE_QUITTER')
quitter = Widget_Button(exitID, Value='Exit Program', UValue = 'RETURN', Event_Pro='ZIMAGE_QUITTER')


drawbase = Widget_Base(tlb, Map=1)
draw = Widget_Draw(drawbase, XSize=xsize, YSize=ysize, Button_Events=1, Event_Pro='ZIMAGE_DRAW_EVENTS')

   ; Realize the program.
Widget_Control, tlb, /Realize

   ; Get the window index number of the draw widget.
   ; Make the draw widget the current graphics window
   ; and display the image in it.
Widget_Control, draw, Get_Value=drawIndex
WSet, drawIndex
TV, BytScl(image, Top=ncolors-1, Max=Max(image), Min=Min(image)) + bottom

   ; Set the title of the window.
Widget_Control, tlb, TLB_Set_Title = StrCompress('Image Dimensions ('+ String(xsize) +' [x], '+ String(ysize) + ' [y] )')

  ; Create a pixmap window the same size as the draw widget window.
  ; Store its window index number in a local variable. Display the
  ; image you just put in the draw widget in the pixmap window.

Window, /Free, XSize=xsize, YSize=ysize, /Pixmap
pixIndex = !D.Window
TV, BytScl(image, Top=ncolors-1, Max=Max(image), Min=Min(image)) + bottom

   ; Get color vectors for this application.
TVLCT, r, g, b, /Get
r = r[bottom:bottom+ncolors-1]
g = g[bottom:bottom+ncolors-1]
b = b[bottom:bottom+ncolors-1]

   ; Create an info structure to hold information required by the program.
info = { CutControl: 1,$	; A flag to activate/desactivate the cut option (1/ON, 2/OFF)
   image:image,$               ; The original image.
   subimage:Ptr_New(),$        ; The scaled and resized subimage.
   xsize:xsize,$               ; The x size of the image window.
   ysize:ysize,$               ; The y size of the image window.
   ReducedSizeXleft: -1,$		; Left x coordinate of the Surface chosen for further Analysis
   ReducedSizeXright: -1,$		; Right x coordinate of the Surface chosen for further Analysis
   ReducedSizeYupper: -1,$		; Upper y coordinate of the Surface chosen for further Analysis
   ReducedSizeYlower: -1,$		; Lower y coordinate of the Surface chosen for further Analysis
   pPassSInfo: Ptr_New(),$		; PointerArray on pSurfaceInfo
   drawIndex:drawIndex,$       ; The draw window index number.
   pixIndex:pixIndex,$         ; The pixmap window index number.
   ncolors:ncolors,$           ; The number of colors for the image.
   bottom:bottom,$             ; The bottom color index.
   colorIndex:colorIndex,$     ; The drawing color index.
   xs:0,$                      ; X static corner of the zoom box.
   ys:0,$                      ; Y static corner of the zoom box.
   xd:0,$                      ; X dynamic corner of the zoom box.
   yd:0,$                      ; Y dynamic corner of the zoom box.
   cutID:cutID,$			; Cut - Button ID.
   zoomID:zoomID,$			; Zoom - Button ID.
   zoomfactorID:zoomfactorID,$	; ZoomFaktor - Button ID.
   returnID:returnID,$		; Exit/Return- Button ID.
   zoomDrawID:-1L,$            ; Zoomed image draw widget ID.
   zoomWindowID:-1,$           ; Zoomed image window index number.
   r:r,$                       ; The red color vector.
   g:g,$                       ; The green color vector.
   b:b,$                       ; The blue color vector.
   r_old:0,$                   ; The user's red color value.
   g_old:0,$                   ; The user's green color value.
   b_old:0,$                   ; The user's blue color value.
   zoomfactor:2,$              ; The initial zoom factor.
   interp:interp,$             ; A flag to select nearest neighbor or bilinear resampling.
   map:0,$                     ; A flag to tell if the controls are mapped.
   controlID:controlID}         ; The identifier of the control base to map.

IF ( Ptr_Valid(pSurfaceInfo)) THEN  info.pPassSInfo = Ptr_New(*pSurfaceInfo)

   ; Store the info structure in the user value of the top-level base.
Widget_Control, tlb, Set_UValue=info, /No_Copy

   ; Register this program and set up the event loop.
XManager, 's_Surface_Method_ZoomCutImage', tlb, Cleanup='ZImage_CLEANUP', Group_Leader = group_leader, /No_Block
END ; of ZIMAGE ****************************************************************************
