;+
; NAME:
;       XCOLORS
;
; PURPOSE:
;
;       The purpose of this routine is to interactively change color tables
;       in a manner similar to XLOADCT. No common blocks are used so
;       multiple copies of XCOLORS can be on the display at the same
;       time (if each has a different TITLE). XCOLORS has the ability
;       to notify a widget ev handler, an object method, or an IDL
;       procedure if and when a new color table has been loaded. The
;       ev handler, object method, or IDL procedure is then responsibe
;       for updating the program's display on 16- or 24-bit display systems.
;
; AUTHOR:
;
;       FANNING SOFTWARE CONSULTING
;       David Fanning, Ph.D.
;       1645 Sheely Drive
;       Fort Collins, CO 80526 USA
;       Phone: 970-221-0438
;       E-mail: davidf@dfanning.com
;       Coyote's Guide to IDL Programming: http://www.dfanning.com
;
; CATEGORY:
;
;       Widgets, Object, Command line.
;
; CALLING SEQUENCE:
;
;       XCOLORS
;
; INPUTS:
;
;       None.
;
; KEYWORD PARAMETERS:
;
;       BLOCK: if this keyword is set, the program will try to block the
;          IDL command line. Note that this is only possible if no other
;          widget program is currently blocking the IDL command line. It
;          is much more reliable to make XCOLORS a modal widget (see the MODAL
;          keyword), although this can generally only be done when XCOLORS
;          is called from another widget program.
;
;       BOTTOM: The lowest color index of the colors to be changed.
;
;       COLORINFO: This output keyword will return either a pointer to
;          a color information structure (if the program is called in
;          a non-modal fashion) or a color information structure (if the program
;          is called in modal or blocking fashion). The color information
;          structure is an anonymous structure defined like this:
;
;             struct = { R: BytArr(!D.Table_Size),$ ; The current R color vector.
;                        G: BytArr(!D.Table_Size),$ ; The current G color vector.
;                        B: BytArr(!D.Table_Size),$ ; The current B color vector.
;                        NAME: "",$                 ; The name of the current color table.
;                        INDEX: 0 }                  ; The index number of the current color table.
;
;          if a pointer to the structure is obtained, you will be responsible
;          for freeing it to prevent memory leakage:
;
;             XColors, ColorInfo = colorInfoPtr
;             Print, "Color Table Name: ", (*colorInfoPtr).Name
;             ptr_free, colorInfoPtr
;
;          Note that that Name field will be "Unknown" and the Index field will
;          be -1 until a color table is actually selected by the user. You are
;          responsible for checking this value before you use it.
;
;          When called in modal or blocking fashion, you don't have to worry about freeing
;          the pointer, since no pointer is involved:
;
;             XColors, /Block, ColorInfo = colorInfoData
;             Help, colorInfoData, /Structure
;             Print, "Color Table Name: ", colorInfoData.Name
;
;       DATA: This keyword can be set to any valid IDL variable. if
;          the variable is defined, the specified object method or notify
;          procedure will be passed this variable via a DATA keyword. This
;          keyword is defined primarily so that Notify Procedures are compatible
;          with the XLOADCT way of passing data. if is not strictly required,
;          since the _EXTRA keyword inheritance mechanism will allow passing
;          of *any* keyword parameter defined for the object or procedure that is
;          to be notified.
;
;       DRAG: Set this keyword if you want colors loaded as you drag
;          the sliders. Default is to update colors only when you release
;          the sliders.
;
;       _EXTRA: This keyword inheritance mechanism will pick up and
;          pass along to any method or procedure to be notified and keywords
;          that are defined for that procedure. Note that you should be sure
;          that keywords are spelled correctly. Any mis-spelled keyword will
;          be ignored.
;
;       FILE: A string variable pointing to a file that holds the
;          color tables to load. The normal colors1.tbl file is used by default.
;
;       GROUP_LEADER: The group leader for this program. When the group
;          leader is destroyed, this program will be destroyed.
;
;       MODAL: Set this keyword (along with the GROUP_LEADER keyword) to
;          make the XCOLORS dialog a modal widget dialog. Note that NO
;          other events can occur until the XCOLORS program is destroyed
;          when in modal mode.
;
;       nColors: This is the number of colors to load when a color table
;          is selected.
;
;       NOTIFYID: A 2-column by n-row array that contains the IDs of widgets
;          that should be notified when XCOLORS loads a color table. The first
;          column of the array is the widgets that should be notified. The
;          second column contains IDs of widgets that are at the top of the
;          hierarchy in which the corresponding widgets in the first column
;          are located. (The purpose of the top widget IDs is to make it
;          possible for the widget in the first column to get the "info"
;          structure of the widget program.) An XCOLORS_LOAD ev will be
;          sent to the widget identified in the first column. The ev
;          structure is defined like this:
;
;          ev = {XCOLORS_LOAD, ID:0L, TOP:0L, HANDLER:0L,$
;             R:BytArr(!D.N_COLORS < 256), G:BytArr(!D.N_COLORS < 256),$
;             B:BytArr(!D.N_COLORS < 256), INDEX:0, NAME:""}
;
;          The ID field will be filled out with NOTIFYID[0, n] and the TOP
;          field will be filled out with NOTIFYID[1, n]. The R, G, and B
;          fields will have the current color table vectors, obtained by
;          exectuing the command TVLCT, r, g, b, /get. The INDEX field will
;          have the index number of the just-loaded color table. The name
;          field will have the name of the currently loaded color table.
;
;          Note that XCOLORS can't initially tell *which* color table is
;          loaded, since it just uses whatever colors are available when it
;          is called. Thus, it stores a -1 in the INDEX field to indicate
;          this "default" value. Programs that rely on the INDEX field of
;          the ev structure should normally do nothing if the value is
;          set to -1. This value is also set to -1 if the user hits the
;          CANCEL button. (Note the NAME field will initially be "Unknown").
;
;          Typically the XCOLORS button will be defined like this:
;
;             xcolorsID = widget_button(parentID, value = 'Load New Color Table...',$
;                event_pro = 'Program_Change_Colors_Event')
;
;          The ev handler will be written something like this:
;
;             pro Program_Change_Colors_Event, ev
;
;                ; Handles color table loading events. Allows colors be to changed.
;
;             widget_control, ev.top, get_uValue = info, /no_copy
;             thisEvent = Tag_Names(ev, /Structure_Name)
;             CASE thisEvent OF
;
;                'widget_bUTTON': begin
;
;                     ; Color table tool.
;
;                   XColors, nColors = info.nColors, Bottom = info.bottom,$
;                      Group_Leader = ev.top, NotifyID = [ev.id, ev.top]
;                   ENDCASE
;
;                'XCOLORS_LOAD': begin
;
;                     ; Update the display for 24-bit displays.
;
;                   device, Get_Visual_Depth = thisDepth
;                   if thisDepth gt 8 then begin
;                   WSet, info.wid
;
;                    ...Whatever display commands are required go here. for example...
;
;                    tv, info.image
;
;                 endif
;                 ENDCASE
;
;              ENDCASE
;
;              widget_control, ev.top, set_uValue = info, /no_copy
;              end
;
;       NOTIFYOBJ: A vector of structures (or a single structure), with
;          each element of the vector defined as follows:
;
;             struct = {XCOLORS_NOTIFYOBJ, object:Obj_New(), method:''}
;
;          where the Object field is an object reference, and the Method field
;          is the name of the object method that should be called when XCOLORS
;          loads its color tables.
;
;             ainfo = {XCOLORS_NOTIFYOBJ, a, 'Draw'}
;             binfo = {XCOLORS_NOTIFYOBJ, b, 'Display'}
;             XColors, NotifyObj = [ainfo, binfo]
;
;          Note that the XColors program must be compiled before these structures
;          are used. Alternatively, you can put this program, named
;          "xcolors_notifyobj__define.pro" (*three* underscore characters in this
;          name!) in your PATH:
;
;             pro XCOLORS_NOTIFYOBJ__DEFINE
;              struct = {XCOLORS_NOTIFYOBJ, OBJECT:Obj_New(), METHOD:''}
;             end
;
;          or, you can simply define this structure as it is shown here in your code.
;
;          "Extra" keywords added to the XCOLORS call are passed along to
;          the object method, which makes this an alternative way to get information
;          to your methods. if you expect such keywords, your methods should be defined
;          with an _Extra keyword.
;
;       NOTIFYPRO: The name of a procedure to notify or call when the color
;          tables are loaded. if the DATA keyword is also defined, it will
;          be passed to this program via an DATA keyword. But note that *any*
;          keyword appropriate for the procedure can be used in the call to
;          XCOLORS. for example, here is a procedure that re-displays and image
;          in the current graphics window:
;
;             pro REFRESH_IMAGE, Image = image, _Extra = extra, WID = wid
;             if n_elements(wid) ne 0 then WSet, wid
;             TVIMAGE, image, _Extra = extra
;             end
;
;          This program can be invoked with this series of commands:
;
;             IDL> Window, /Free
;             IDL> TVImage, image, Position = [0.2, 0.2, 0.8, 0.8]
;             IDL> XColors, NotifyPro = 'Refresh_Image', Image = image, WID = !D.Window
;
;          Note that "extra" keywords added to the XCOLORS call are passed along to
;          your procedure, which makes this an alternative way to get information
;          to your procedure. if you expect such keywords, your procedure should
;          be defined with an _Extra keyword as illustrated above.
;
;       TITLE: This is the window title. It is "Load Color Tables" by
;          default. The program is registered with the name 'XCOLORS:' plus
;          the TITLE string. The "register name" is checked before the widgets
;          are defined. if a program with that name has already been registered
;          you cannot register another with that name. This means that you can
;          have several versions of XCOLORS open simultaneously as long as each
;          has a unique title or name. for example, like this:
;
;            IDL> XColors, nColors = 100, Bottom = 0, title = 'First 100 Colors'
;            IDL> XColors, nColors = 100, Bottom = 100, title = 'Second 100 Colors'
;
;       XOFFSET: This is the X offset of the program on the display. The
;          program will be placed approximately in the middle of the display
;          by default.
;
;       YOFFSET: This is the Y offset of the program on the display. The
;          program will be placed approximately in the middle of the display
;          by default.
;
; COMMON BLOCKS:
;
;       None.
;
; SIDE EFFECTS:
;
;       Colors are changed. Events are sent to widgets if the NOTIFYID
;       keyword is used. Object methods are called if the NOTIFYOBJ keyword
;       is used. This program is a non-blocking widget.
;
; RESTRICTIONS:
;
;       None.
;
; EXAMPLE:
;
;       To load a color table into 100 colors, starting at color index
;       50 and send an ev to the widget identified at info.drawID
;       in the widget heirarchy of the top-level base ev.top, type:
;
;       XCOLORS, nColors = 100, BOTTOM = 50, NOTIFYID = [info.drawID, ev.top]
;
; MODIFICATION HISTORY:
;       Written by:     David Fanning, 15 April 97. Extensive modification
;         of an older XCOLORS program with excellent suggestions for
;         improvement by Liam Gumley. Now works on 8-bit and 24-bit
;         systems. Subroutines renamed to avoid ambiguity. Cancel
;         button restores original color table.
;       23 April 1997, added color protection for the program. DWF
;       24 April 1997, fixed a window initialization bug. DWF
;       18 June 1997, fixed a bug with the color protection handler. DWF
;       18 June 1997, Turned tracking on for draw widget to fix a bug
;         in wTopBase Tracking Events for WindowsNT machines in IDL 5.0. DWF
;       20 Oct 1997, Changed GROUP keyword to GROUP_LEADER. DWF
;       19 Dec 1997, Fixed bug with TOP/BOTTOM reversals and CANCEL. DWF.
;        9 Jun 1998, Fixed bug when using BOTTOM keyword on 24-bit devices. DWF
;        9 Jun 1998, Added device, Decomposed = 0 for TrueColor visual classes. DWF
;        9 Jun 1998, Removed all IDL 4 compatibility.
;       21 Oct 1998, Fixed problem with gamma not being reset on CANCEL. DWF
;        5 Nov 1998. Added the NotifyObj keyword, so that XCOLORS would work
;         interactively with objects. DWF.
;        9 Nov 1998. Made slider reporting only at the end of the drag. if you
;         want continuous updating, set the DRAG keyword. DWF.
;        9 Nov 1998. Fixed problem with TOP and BOTTOM sliders not being reset
;         on CANCEL. DWF.
;       10 Nov 1998. Fixed fixes. Sigh... DWF.
;        5 Dec 1998. Added INDEX field to the XCOLORS_LOAD ev structure. This
;         field holds the current color table index number. DWF.
;        5 Dec 1998. Modified the way the colorbar image was created. Results in
;         greatly improved display for low number of colors. DWF.
;        6 Dec 1998. Added the ability to notify an unlimited number of objects. DWF.
;       12 Dec 1998. Removed obsolete Just_Reg keyword and improved documetation. DWF.
;       30 Dec 1998. Fixed the way the color table index was working. DWF.
;        4 Jan 1999. Added slightly modified CONGRID_FAN program to fix floating divide
;          by zero problem. DWF
;        2 May 1999. Added code to work around a Macintosh bug in IDL through version
;          5.2 that tries to redraw the graphics window after a TVLCT command. DWF.
;        5 May 1999. Restore the current window index number after drawing graphics.
;          Not supported on Macs. DWF.
;        9 Jul 1999. Fixed a couple of bugs I introduced with the 5 May changes. Sigh... DWF.
;       13 Jul 1999. Scheesh! That May 5th change was a BAD idea! Fixed more bugs. DWF.
;       31 Jul 1999. Substituted !D.Table_Size for !D.N_Colors. DWF.
;        1 Sep 1999. Got rid of the May 5th fixes and replaced with something MUCH simpler. DWF.
;       14 Feb 2000. Removed the window index field from the object notify structure. DWF.
;       14 Feb 2000. Added NOTIFYPRO, DATA, and _EXTRA keywords. DWF.
;       20 Mar 2000. Added MODAL, BLOCK, and COLORINFO keywords. DWF
;       20 Mar 2000. Fixed a slight problem with color protection events triggering
;          notification events. DWF.
;       31 Mar 2000. Fixed a problem with pointer leakage on Cancel events, and improved
;          program documentation. DWF.
;       17 Aug 2000. Fixed a problem with CANCEL that occurred only if you first
;          changed the gamma settings before loading a color table. DWF.
;       10 Sep 2000. Removed the requirement that procedures and object methods must
;          be written with an _Extra keyword. DWF.
;        5 Oct 2000. Added the File keyword to LOADCT command, as I was suppose to. DWF.
;        5 Oct 2000. Now properly freeing program pointers upon early exit from program. DWF.
;        7 Mar 2001. Fixed a problem with the BLOCK keyword. DWF.
;-
;
;###########################################################################
;
; LICENSE
;
; This software is OSI Certified Open Source Software.
; OSI Certified is a certification mark of the Open Source Initiative.
;
; Copyright © 1997-2000 Fanning Software Consulting.
;
; This software is provided "as-is", without any express or
; implied warranty. In no ev will the authors be held liable
; for any damages arising from the use of this software.
;
; Permission is granted to anyone to use this software for any
; purpose, including commercial applications, and to alter it and
; redistribute it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim you wrote the original software. if you use this software
;    in a product, an acknowledgment in the product documentation
;    would be appreciated, but is not required.
;
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
;
; 3. This notice may not be removed or altered from any source distribution.
;
; for more information on Open Source Software, visit the Open Source
; web site: http://www.opensource.org.
;
;###########################################################################
; $Id: congrid.pro,v 1.7 1998/01/15 18:41:15 scottm Exp $
;
; Copyright (c) 1988-1998, Research Systems, Inc.  All rights reserved.
;  Unauthorized reproduction prohibited.
;
;
; NAME:
;  CONGRID
;
; PURPOSE:
;       Shrink or expand the size of an array by an arbitrary amount.
;       This IDL procedure simulates the action of the VAX/VMS
;       CONGRID/CONGRIDI function.
;
;  This function is similar to "REBIN" in that it can resize a
;       one, two, or three dimensional array.   "REBIN", however,
;       requires that the new array size must be an integer multiple
;       of the original size.   CONGRID will resize an array to any
;       arbitrary size (REBIN is somewhat faster, however).
;       REBIN averages multiple points when shrinking an array,
;       while CONGRID just resamples the array.
;
; CATEGORY:
;       Array Manipulation.
;
; CALLING SEQUENCE:
;  array = CONGRID(array, x, y, z)
;
; INPUTS:
;       array:  A 1, 2, or 3 dimensional array to resize.
;               Data Type : Any type except string or structure.
;
;       x:      The new X dimension of the resized array.
;               Data Type : Int or Long (greater than or equal to 2).
;
; OPTIONAL INPUTS:
;       y:      The new Y dimension of the resized array.   if the original
;               array has only 1 dimension then y is ignored.   if the
;               original array has 2 or 3 dimensions then y MUST be present.
;
;       z:      The new Z dimension of the resized array.   if the original
;               array has only 1 or 2 dimensions then z is ignored.   if the
;               original array has 3 dimensions then z MUST be present.
;
; KEYWORD PARAMETERS:
;       INTERP: if set, causes linear interpolation to be used.
;               Otherwise, the nearest-neighbor method is used.
;
;  CUBIC:   if specified and non-zero, "Cubic convolution"
;     interpolation is used.  This is a more
;     accurate, but more time-consuming, form of interpolation.
;     CUBIC has no effect when used with 3 dimensional arrays.
;     if this parameter is negative and non-zero, it specifies the
;     value of the cubic interpolation parameter as described
;     in the INTERPOLATE function.  Valid ranges are -1 < =  Cubic < 0.
;     Positive non-zero values of CUBIC (e.g. specifying /CUBIC)
;     produce the default value of the interpolation parameter
;     which is -1.0.
;
;       MINUS_ONE:
;               if set, will prevent CONGRID from extrapolating one row or
;               column beyond the bounds of the input array.   for example,
;               if the input array has the dimensions (i, j) and the
;               output array has the dimensions (x, y), then by
;               default the array is resampled by a factor of (i/x)
;               in the X direction and (j/y) in the Y direction.
;               if MINUS_ONE is present (and IS NON-ZERO) then the array
;               will be resampled by the factors (i-1)/(x-1) and (j-1)/(y-1).
;
; OUTPUTS:
;  The returned array has the same number of dimensions as the original
;       array and is of the same data type.   The returned array will have
;       the dimensions (x), (x, y), or (x, y, z) depending on how many
;       dimensions the input array had.
;
; PROCEDURE:
;       if the input array has three dimensions, or if INTERP is set,
;       then the IDL interpolate function is used to interpolate the
;       data values.
;       if the input array has two dimensions, and INTERP is NOT set,
;       then the IDL POLY_2D function is used for nearest neighbor sampling.
;       if the input array has one dimension, and INTERP is NOT set,
;       then nearest neighbor sampling is used.
;
; EXAMPLE:
;       ; vol is a 3-D array with the dimensions (80, 100, 57)
;       ; Resize vol to be a (90, 90, 80) array
;       vol = CONGRID(vol, 90, 90, 80)
;
; MODIFICATION HISTORY:
;       DMS, Sept. 1988.
;       DMS, Added the MINUS_ONE keyword, Sept. 1992.
;  Daniel Carr. Re-wrote to handle one and three dimensional arrays
;                    using INTERPOLATE function.
;  DMS, RSI, Nov, 1993.  Added CUBIC keyword.
;       SJL, Nov, 1997.  Formatting, conform to IDL style guide.
;       DWF, Jan, 1999. Added error checking to look for divide by zero.
;

function CONGRID_FAN, arr, x, y, z, INTERP = int, MINUS_ONE = m1, CUBIC = cubic

    ON_ERROR, 2      ;return to caller if error
    s = size(arr)

    if ((s[0] eq 0) or (s[0] gt 3)) then $
      Message, 'Array must have 1, 2, or 3 dimensions.'

    ;;  Supply defaults = no interpolate, and no minus_one.
    if (n_elements(int) le 0) then int = 0 else int = KEYWORD_SET(int)
    if (n_elements(m1) le 0) then m1 = 0 else m1 = KEYWORD_SET(m1)
    if (n_elements(cubic) eq 0) then cubic = 0
    if (cubic ne 0) then int = 1 ;Cubic implies interpolate


    case s[0] of
        1: begin                ; *** ONE DIMENSIONAL ARRAY
            ; DWF modified: Check divide by zero.
            srx = float(s[1] - m1)/((x-m1) > 1e-6) * findgen(x) ;subscripts
            if (int) then $
              return, INTERPOLATE(arr, srx, CUBIC = cubic) else $
              return, arr[ROUND(srx)]
        endcase
        2: begin                ; *** TWO DIMENSIONAL ARRAY
            if (int) then begin
                srx = float(s[1] - m1) / ((x-m1) > 1e-6) * findgen(x)
                sry = float(s[2] - m1) / ((y-m1) > 1e-6) * findgen(y)
                return, INTERPOLATE(arr, srx, sry, /GRID, CUBIC = cubic)
            endif else $
              return, POLY_2D(arr,$
                              [[0,0],[(s[1]-m1)/(float(x-m1) > 1e-6),0]],$ ;Use poly_2d
                              [[0,(s[2]-m1)/(float(y-m1) > 1e-6)],[0,0]],int,x,y)

        endcase
        3: begin                ; *** THREE DIMENSIONAL ARRAY
            srx = float(s[1] - m1) / ((x-m1) > 1e-6) * findgen(x)
            sry = float(s[2] - m1) / ((y-m1) > 1e-6) * findgen(y)
            srz = float(s[3] - m1) / ((z-m1) > 1e-6) * findgen(z)
            return, interpolate(arr, srx, sry, srz, /GRID)
        endcase
    endcase

    return, arr_r
end


pro XColors_NotifyObj__Define
       ; Structure definition module for object notification.
    struct = {  XColors_NotifyObj,$  ; The structure name.
                object:Obj_New(),$  ; The object to notify.
                method:'' }           ; The name of the object method to call.
end


pro XColors_Set, info
    TVLCT, r, g, b, /get
       ; Make sure the current bottom index is less than the current top index.
    if info.currentbottom ge info.currenttop then begin
       temp = info.currentbottom
       info.currentbottom = info.currenttop
       info.currenttop = temp
    endif

    r(info.bottom:info.currentbottom) = info.bottomcolor(0)
    g(info.bottom:info.currentbottom) = info.bottomcolor(1)
    b(info.bottom:info.currentbottom) = info.bottomcolor(2)
    r(info.currenttop:info.top) = info.topcolor(0)
    g(info.currenttop:info.top) = info.topcolor(1)
    b(info.currenttop:info.top) = info.topcolor(2)

    red = info.r
    green = info.g
    blue = info.b
    number = ABS((info.currenttop-info.currentbottom) + 1)

    gamma = info.gamma
    index = Findgen(info.nColors)
    distribution = index^gamma > 10e-6
    index = Round(distribution * (info.nColors-1) / (Max(distribution) > 10e-6))

    if info.currentbottom ge info.currenttop then begin
       temp = info.currentbottom
       info.currentbottom = info.currenttop
       info.currenttop = temp
    endif

    if info.reverse eq 0 then begin
       r(info.currentbottom:info.currenttop) = Congrid_FAN(red(index), number, /Minus_One)
       g(info.currentbottom:info.currenttop) = Congrid_FAN(green(index), number, /Minus_One)
       b(info.currentbottom:info.currenttop) = Congrid_FAN(blue(index), number, /Minus_One)
    endif else begin
       r(info.currentbottom:info.currenttop) = $
          Reverse(Congrid_FAN(red(index), number, /Minus_One))
       g(info.currentbottom:info.currenttop) = $
          Reverse(Congrid_FAN(green(index), number, /Minus_One))
       b(info.currentbottom:info.currenttop) = $
          Reverse(Congrid_FAN(blue(index), number, /Minus_One))
    endelse

    TVLct, r, g, b

    WSet, info.windowindex
    tv, info.colorimage

    WSet, info.thisWindow

    (*info.colorInfoPtr).r = r
    (*info.colorInfoPtr).g = g
    (*info.colorInfoPtr).b = b
    (*info.colorInfoPtr).name = info.ctname
    (*info.colorInfoPtr).index = info.index

       ; Don't bother with notification if this is just a color protection ev.
    if info.from eq 'PROTECT' then return

       ; Are there widgets to notify?
    s = size(info.notifyID)
    if s(0) eq 1 then count = 0 else count = s(2)-1
    for j = 0,count do begin
       colorEvent = { XCOLORS_LOAD,$
                      ID:info.notifyID(0,j),$
                      TOP:info.notifyID(1,j),$
                      HANDLER:0L,$
                      R:r,$
                      G:g,$
                      B:b,$
                      gamma:info.gamma,$
                      stretch: [info.currentBottom, info.currentTop],$
                      index:info.index,$
                      name:info.ctname }
       if Widget_Info(info.notifyID(0,j), /Valid_ID) then widget_control, info.notifyID(0,j), Send_Event = colorEvent
    endfor

       ; Is there an object to notify?
    nelements = size(info.notifyobj, /n_elements)
    for j = 0,nelements-1 do begin
       if Obj_Valid((info.notifyobj)[j].object) then begin
          if n_elements(*info.xcolorsData) eq 0 then begin
             s = size(*info.extra)
             if s[s[0]+1] eq 0 then begin
                Call_Method, (info.notifyobj)[j].method, (info.notifyobj)[j].object
             endif else begin
                Call_Method, (info.notifyobj)[j].method, (info.notifyobj)[j].object,$
                   _Extra = *info.extra
             endelse
          endif else begin
            s = size(*info.extra)
            if s[s[0]+1] eq 0 then begin
                Call_Method, (info.notifyobj)[j].method, (info.notifyobj)[j].object,$
                   DATA = *info.xcolorsData
            endif else begin
                Call_Method, (info.notifyobj)[j].method, (info.notifyobj)[j].object,$
                   DATA = *info.xcolorsData, _Extra = *info.extra
            endelse
          endelse
       endif
    endfor

       ; Is there a procedure to notify?
    if info.notifyPro ne "" then begin
       if n_elements(*info.xcolorsData) eq 0 then begin
          s = size(*info.extra)
          if s[s[0]+1] eq 0 then begin
             Call_Procedure, info.notifyPro
          endif else begin
             Call_Procedure, info.notifyPro, _Extra = *info.extra
          endelse
       endif else begin
          s = size(*info.extra)
          if s[s[0]+1] eq 0 then begin
             Call_Procedure, info.notifyPro, DATA = *info.xcolorsData
          endif else begin
             Call_Procedure, info.notifyPro, DATA = *info.xcolorsData, _Extra = *info.extra
          endelse
       endelse
    endif
end



pro XCOLORS_TOP_SLIDER, ev

   ; Get the info structure from storage location.
widget_control, ev.top, get_uValue = info, /no_copy

   ; Update the current top value of the slider.
currentTop = ev.value
widget_control, info.botSlider, get_value = currentBottom
currentBottom = currentBottom + info.bottom
currentTop = currentTop + info.bottom

   ; Error handling. Is currentBottom = currentTop?

if currentBottom eq currentTop then begin
   currentBottom = (currentTop - 1) > 0
   thisValue = (currentBottom-info.bottom)
   if thisValue LT 0 then begin
      thisValue = 0
      currentBottom = info.bottom
   endif
   widget_control, info.botSlider, set_value = thisValue
endif

   ; Error handling. Is currentBottom > currentTop?

if currentBottom gt currentTop then begin

   bottom = currentTop
   top = currentBottom
   bottomcolor = info.topColor
   topcolor = info.bottomColor
   reverse = 1

endif else begin

   bottom = currentBottom
   top = currentTop
   bottomcolor = info.bottomColor
   topcolor = info.topColor
   reverse = 0

endelse

   ; Create a pseudo structure.

pseudo = {currenttop:top, currentbottom:bottom, reverse:reverse,$
   bottomcolor:bottomcolor, topcolor:topcolor, gamma:info.gamma, index:info.index,$
   top:info.top, bottom:info.bottom, nColors:info.nColors, r:info.r,$
   g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage,$
   windowindex:info.windowindex, from:'TOP', notifyObj:info.notifyObj, extra:info.extra,$
   thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData,$
   colorInfoPtr:info.colorInfoPtr, colornames:info.colornames, ctname:info.ctname,$
   needColorInfo:info.needColorInfo}

   ; Update the colors.

XColors_Set, pseudo

info.currentTop = currentTop

   ; Put the info structure back in storage location.

widget_control, ev.top, set_uValue = info, /no_copy
end ; ************************************************************************



pro XCOLORS_BOTTOM_SLIDER, ev
    widget_control, ev.top, get_uValue = info, /no_copy

          ; Update the current bottom value of the slider.
       currentBottom = ev.value + info.bottom
       widget_control, info.topSlider, get_value = currentTop
       ;currentBottom = currentBottom + info.bottom
       currentTop = currentTop + info.bottom

          ; Error handling. Is currentBottom = currentTop?
       if currentBottom eq currentTop then begin
          currentBottom = currentTop
          widget_control, info.botSlider, set_value = (currentBottom-info.bottom)
       endif

          ; Error handling. Is currentBottom > currentTop?
       if currentBottom gt currentTop then begin
          bottom = currentTop
          top = currentBottom
          bottomcolor = info.topColor
          topcolor = info.bottomColor
          reverse = 1
       endif else begin
          bottom = currentBottom
          top = currentTop
          bottomcolor = info.bottomColor
          topcolor = info.topColor
          reverse = 0
       endelse

          ; Create a pseudo structure.
       pseudo = {currenttop:top, currentbottom:bottom, reverse:reverse,$
          bottomcolor:bottomcolor, topcolor:topcolor, gamma:info.gamma, index:info.index,$
          top:info.top, bottom:info.bottom, nColors:info.nColors, r:info.r,$
          g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage,$
          windowindex:info.windowindex, from:'BOTTOM', notifyObj:info.notifyObj, extra:info.extra,$
          thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData,$
          colorInfoPtr:info.colorInfoPtr, colornames:info.colornames, ctname:info.ctname,$
          needColorInfo:info.needColorInfo}

       ; Update the colors.
       XColors_Set, pseudo
       info.currentBottom = currentBottom
    widget_control, ev.top, set_uValue = info, /no_copy
end

pro XCOLORS_GAMMA_SLIDER, ev
    widget_control, ev.top, get_uValue = info, /no_copy

          ; Get the gamma value from the slider.
       widget_control, ev.id, get_value = gamma
       gamma = 10^((gamma/50.0) - 1)

          ; Update the gamma label.
       widget_control, info.gammaID, set_value = String(gamma, Format = '(F6.3)')

          ; Make a pseudo structure.
       if info.currentBottom gt info.currentTop then $
          pseudo = {currenttop:info.currentbottom, currentbottom:info.currenttop,$
             reverse:1, bottomcolor:info.topcolor, topcolor:info.bottomcolor,$
             gamma:gamma, top:info.top, bottom:info.bottom, index:info.index,$
             nColors:info.nColors, r:info.r, g:info.g, b:info.b,$
             notifyID:info.notifyID, colorimage:info.colorimage, extra:info.extra,$
             windowindex:info.windowindex, from:'SLIDER', notifyObj:info.notifyObj,$
             thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData,$
             colorInfoPtr:info.colorInfoPtr, colornames:info.colornames, ctname:info.ctname,$
             needColorInfo:info.needColorInfo} $
       else $
          pseudo = {currenttop:info.currenttop, currentbottom:info.currentbottom,$
             reverse:0, bottomcolor:info.bottomcolor, topcolor:info.topcolor,$
             gamma:gamma, top:info.top, bottom:info.bottom, index:info.index,$
             nColors:info.nColors, r:info.r, g:info.g, b:info.b,$
             notifyID:info.notifyID, colorimage:info.colorimage, extra:info.extra,$
             windowindex:info.windowindex, from:'SLIDER', notifyObj:info.notifyObj,$
             thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData,$
             colorInfoPtr:info.colorInfoPtr, colornames:info.colornames, ctname:info.ctname,$
             needColorInfo:info.needColorInfo}

          ; Load the colors.
       XColors_Set, pseudo
       info.gamma = gamma
    widget_control, ev.top, set_uValue = info, /no_copy
end


pro XCOLORS_COLORTABLE, ev
    widget_control, ev.top, get_uValue = info, /no_copy

       loadCT, ev.index, File = info.file, /Silent, nColors = info.nColors, Bottom = info.bottom

       TVLct, r, g, b, /get
       info.r = r(info.bottom:info.top)
       info.g = g(info.bottom:info.top)
       info.b = b(info.bottom:info.top)
       info.topcolor = [r(info.top), g(info.top), b(info.top)]
       info.bottomcolor = [r(info.bottom), g(info.bottom), b(info.bottom)]

          ; Update the slider positions and values.
;     widget_control, info.botSlider, set_value = 0
;     widget_control, info.topSlider, set_value = info.nColors-1
;     widget_control, info.gammaSlider, set_value = 50
;     widget_control, info.gammaID, set_value = String(1.0, Format = '(F6.3)')
;     info.currentBottom = info.bottom
;     info.currentTop = info.top
;     info.gamma = 1.0
       info.index = ev.index
       info.ctname = info.colornames[ev.index]

          ; Create a pseudo structure.
       pseudo = {currenttop:info.currenttop, currentbottom:info.currentbottom,$
          reverse:info.reverse, windowindex:info.windowindex, index:ev.index,$
          bottomcolor:info.bottomcolor, topcolor:info.topcolor, gamma:info.gamma,$
          top:info.top, bottom:info.bottom, nColors:info.nColors, r:info.r,$
          g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage,$
          from:'LIST', notifyObj:info.notifyObj, thisWindow:info.thisWindow,$
          notifyPro:info.notifyPro, xcolorsData:info.xcolorsData, extra:info.extra,$
          colorInfoPtr:info.colorInfoPtr, colornames:info.colornames, ctname:info.ctname,$
          needColorInfo:info.needColorInfo}

          ; Update the colors.
       XColors_Set, pseudo
    widget_control, ev.top, set_uValue = info, /no_copy
end


pro XCOLORS_PROTECT_COLORS, ev
    widget_control, ev.top, get_uValue = info, /no_copy
          ; Create a pseudo structure.
       pseudo = {currenttop:info.currenttop, currentbottom:info.currentbottom,$
          reverse:info.reverse,$
          bottomcolor:info.bottomcolor, topcolor:info.topcolor, gamma:info.gamma,$
          top:info.top, bottom:info.bottom, nColors:info.nColors, r:info.r, index:info.index,$
          g:info.g, b:info.b, notifyID:info.notifyID, colorimage:info.colorimage,$
          windowindex:info.windowindex, from:'PROTECT', notifyObj:info.notifyObj, extra:info.extra,$
          thisWindow:info.thisWindow, notifyPro:info.notifyPro, xcolorsData:info.xcolorsData,$
          colorInfoPtr:info.colorInfoPtr, colornames:info.colornames, ctname:info.ctname,$
          needColorInfo:info.needColorInfo}

          ; Update the colors.
       XColors_Set, pseudo
    widget_control, ev.top, set_uValue = info, /no_copy
end


pro XCOLORS_CANCEL, ev
    widget_control, ev.top, get_uValue = info, /no_copy
       XColors_Set, info.cancelStruct
    widget_control, ev.top, set_uValue = info, /no_copy
    widget_control, ev.top, /destroy
end


pro XCOLORS_DISMISS, ev
    widget_control, ev.top, /destroy
end


pro XCOLORS_CLEANUP, wTopBase
    widget_control, wTopBase, get_uValue = info, /no_copy
    if n_elements(info) ne 0 then begin
       ptr_free, info.xcolorsData
       ptr_free, info.extra
       if info.needColorInfo eq 0 then ptr_free, info.colorInfoPtr
    endif
end


pro XCOLORS, nColors = ncolors, Bottom = bottom, Title = title, File = file,$
    colTbl = colTbl, rgbValues = rgbValues, stretch = stretch, gamma = gamma,$
    Group_Leader = group, XOffset = xoffset, YOffset = yoffset, Block = block,$
    NotifyID = notifyID, NotifyObj = notifyObj, Drag = drag, NotifyPro = notifyPro,$
    Data = xColorsData, _Extra = extra, Modal = modal, ColorInfo = colorInfoPtr

    On_Error, 1
       ; Current graphics window.
    thisWindow = !D.Window

       ; Check keyword parameters. Define defaults.
    if (n_elements(colTbl) eq 0) then colTbl = 0
    if (n_elements(gamma) eq 0) then gamma = 1.
    if (n_elements(stretch) eq 0) then stretch = [0,255]
    bottom = stretch[0]
    top = stretch[1]
    nColors = (256 < !D.Table_Size)

    if n_elements(title) eq 0 then title = 'Load Color Tables'
    if n_elements(drag) eq 0 then drag = 0
    if n_elements(file) eq 0 then file = filePath(SubDir = ['resource','colors'], 'colors1.tbl')
    if n_elements(notifyID) eq 0 then notifyID = [-1L, -1L]
    if n_elements(notifyObj) eq 0 then notifyObj = {object:Obj_New(), method:'', wid:-1}
    if size(notifyObj, /Type) ne 8 then begin
       ok = Dialog_Message(['Arguments to the NotifyObj keyword must', 'be structures. Returning...'])
       return
    end
    nelements = size(notifyObj, /n_elements)
    for j = 0,nelements-1 do begin
       tags = Tag_Names(notifyObj[j])
       check = where(tags eq 'OBJECT', count1)
       check = where(tags eq 'METHOD', count2)
       if (count1 + count2) ne 2 then begin
          ok = Dialog_Message('NotifyObj keyword has incorrect fields. Returning...')
         return
       endif
    endfor
    if n_elements(notifyPro) eq 0 then notifyPro = ""
    if n_elements(xcolorsData) eq 0 then xdata = ptr_new(/allocate_heap) else xdata = ptr_new(xcolorsData)
    if n_elements(extra) eq 0 then extra = ptr_new(/Allocate_Heap) else extra = ptr_new(extra)
    if Arg_Present(colorInfoPtr) then needcolorInfo = 1 else needcolorInfo = 0
    noblock = 1 - Keyword_Set(block)

       ; Create a pointer to the color information.
    if (n_elements(rgbValues) ne 0) then begin
       size_rgb = (size(rgbValues, /dim))[2]
       type_rgb = size(rgbValues, /type)
       rr = make_array(size_rgb, type = type_rgb) + rgbValues[0,0,*]
       gg = make_array(size_rgb, type = type_rgb) + rgbValues[0,1,*]
       bb = make_array(size_rgb, type = type_rgb) + rgbValues[0,2,*]
    endif else TVLCT, rr, gg, bb, /get
    colorInfoPtr = ptr_new({R:rr, G:gg, B:bb, Name:'Unknown', Index:-1})

       ; Get the colors that make up the current color table in the range that this program deals with.
    r = rr[0:nColors-1]
    g = gg[0:nColors-1]
    b = bb[0:nColors-1]
    topColor = [rr[nColors-1], gg[nColors-1], bb[nColors-1]]
    bottomColor = [rr[0], gg[0], bb[0]]

       ; Find the center of the display.
    device, get_screen_size = screenSize
    xCenter = FIX(screenSize(0) / 2.0)
    yCenter = FIX(screenSize(1) / 2.0)
    if n_elements(xoffset) eq 0 then xoffset = xCenter - 150
    if n_elements(yoffset) eq 0 then yoffset = yCenter - 200

       ; Only one XCOLORS with this title.
    registerName = 'XCOLORS:' + title
    if XRegistered(registerName) gt 0 then begin
       ptr_free, xdata
       ptr_free, extra
       ptr_free, colorInfoPtr
       return
    endif

       ; Create the top-level base. No resizing.
    if Keyword_Set(modal) and n_elements(group) ne 0 then wTopBase = widget_base(Column = 1, Title = title, TLB_Frame_Attr = 1, XOffSet = xoffset, YOffSet = yoffset, Base_Align_Center = 1, Modal = 1, Group_Leader = group) $
       else wTopBase = widget_base(Column = 1, Title = title, TLB_Frame_Attr = 1, XOffSet = xoffset, YOffSet = yoffset, Base_Align_Center = 1)

       ; Create a draw widget to display the current colors.
    if !D.NAME ne 'MAC' then draw = Widget_Draw(wTopBase, XSize = 256, YSize = 40, Expose_Events = 0, Retain = 0, event_pro = 'XCOLORS_PROTECT_COLORS') else draw = Widget_Draw(wTopBase, XSize = 256, YSize = 40, Retain = 1)

       ; Create sliders to control stretchs and gamma correction.
    sliderbase = widget_base(wTopBase, Column = 1, Frame = 1)
    botSlider = widget_slider(sliderbase, value = stretch[0], min = 0, max = nColors-1, XSize = 256,event_pro = 'XColors_Bottom_Slider', title = 'Stretch Bottom', Drag = drag)
    topSlider = widget_slider(sliderbase, value = stretch[1], min = 0, max = nColors-1, XSize = 256, event_pro = 'XColors_Top_Slider', title = 'Stretch Top', Drag = drag)
    gammaID = widget_label(sliderbase, value = String(gamma, format = '(F6.3)'))
    gammaSlider = widget_slider(sliderbase, value = 50. *(1 + alog10(gamma)), min = 0, max = 100,$
       Drag = drag, XSize = 256, /Suppress_Value, event_pro = 'XColors_Gamma_Slider', title = 'Gamma Correction')

       ; Get the colortable names for the list widget.
    colorNames = ''
    loadCT, Get_Names = colorNames, File = file
    colorNamesIndex = StrArr(n_elements(colorNames))
    for j = 0,n_elements(colorNames)-1 do colorNamesIndex(j) = StrTrim(j,2) + ' - ' + colorNames(j)
    filebase = widget_base(wTopBase, Column = 1, /Frame)
    listlabel = widget_label(filebase, value = 'Select Color Table...')
    list = widget_list(filebase, value = colorNamesIndex, YSize = 8, Scr_XSize = 256, event_pro = 'XColors_ColorTable')

       ; Dialog Buttons
    dialogbase = widget_base(wTopBase, Row = 1)
    cancel = widget_button(dialogbase, value = 'Cancel', event_pro = 'XColors_Cancel', uValue = 'CANCEL')
    dismiss = widget_button(dialogbase, value = 'Accept', event_pro = 'XColors_Dismiss', uValue = 'ACCEPT')
    widget_control, wTopBase, /Realize

       ; Get window index number of the draw widget.
    widget_control, draw, get_value = windowIndex

       ; Is this a 24-bit TrueColor device? if so, turn color decomposition OFF.
    thisRelease = float(!Version.Release)
    if thisRelease ge 5.1 then begin
       device, Get_Visual_Name = thisVisual
       if thisVisual eq 'TrueColor' then device, Decomposed = 0
    endif else device, Decomposed = 0

       ; Put a picture of the color table in the window.
    bar = bindgen(nColors) # replicate(1B, 10)
;   bar = bytScl(bar, top = nColors-1) + bottom
    bar = Congrid_FAN(bar, 256, 40, /interp)
    WSet, windowIndex
    tvLCT, rr, gg, bb
    tv, bar

       ; Create a cancel structure.
    cancelstruct = { currenttop:top,$
                    currentbottom:bottom,$
                      reverse:0,$
                      windowindex:windowindex,$
                      bottomcolor:bottomcolor, topcolor:topcolor, gamma:1.0,$
                      top:top, bottom:bottom, nColors:ncolors, r:r,$
                      g:g, b:b, notifyID:notifyID, index:-1,$
                      colorimage:bar, from:'CANCEL', notifyObj:notifyObj, extra:extra,$
                      thisWindow:thisWindow, notifyPro:notifyPro, xcolorsData:xData,$
                      colorInfoPtr:colorInfoPtr, colornames:colornames, ctname:"Unknown",$
                      needColorInfo:needColorInfo}

    info = {  windowIndex:windowIndex,$         ; The WID of the draw widget.
                 botSlider:botSlider,$             ; The widget ID of the bottom slider.
                 currentBottom:bottom,$            ; The current bottom slider value.
                 currentTop:top,$                  ; The current top slider value.
                 topSlider:topSlider,$             ; The widget ID of the top slider.
                 gammaSlider:gammaSlider,$         ; The widget ID of the gamma slider.
                 gammaID:gammaID,$                 ; The widget ID of the gamma label
                 nColors:ncolors,$                 ; The number of colors we are using.
                 gamma : gamma,$                       ; The current gamma value.
                 file:file,$                       ; The name of the color table file.
                 bottom : 0,$                   ; The bottom color index.
                 top : ncolors-1,$                         ; The top color index.
                 topcolor:topColor,$               ; The top color in this color table.
                 bottomcolor:bottomColor,$         ; The bottom color in this color table.
                 reverse:0,$                       ; A reverse color table flag.
                 notifyID:notifyID,$               ; Notification widget IDs.
                 notifyObj:notifyObj,$             ; An vector of structures containng info about objects to notify.
                 notifyPro:notifyPro,$             ; The name of a procedure to notify.
                 r:r,$                             ; The red color vector.
                 g:g,$                             ; The green color vector.
                 b:b,$                             ; The blue color vector.
                 extra:extra,$                     ; A pointer to extra keywords.
                 oindex: colTbl,$                       ; The original color table number.
                 index: colTbl,$                        ; The current color table number.
                 thisWindow:thisWindow,$           ; The current graphics window when this program is called.
                 xcolorsData:xdata,$               ; A pointer to the xcolorData variable passed into the program.
                 rstart:r,$                        ; The original red color vector.
                 gstart:g,$                        ; The original green color vector.
                 bstart:b,$                        ; The original blue color vector.
                 colorInfoPtr:colorInfoPtr,$       ; A pointer to the color information.
                 colornames:colornames,$           ; The names of the color tables.
                 ctname:"Unknown",$                ; The current color table name.
                 needColorInfo:needColorInfo,$     ; A flag that indicates color information is requested.
                 cancelStruct:cancelStruct,$       ; The cancel structure.
                 colorimage:bar }                   ; The color table image.

       ; Turn color protection on.
    if !D.NAME ne 'MAC' then widget_control, draw, Draw_Expose_Events = 1

       ; Store the info structure in the user value of the top-level base.
    widget_control, wTopBase, set_uValue = info, /no_copy
    widget_control, wTopBase, /Managed
    WSet, thisWindow

    XManager, registerName, wTopBase, Group = group, No_Block = noblock, cleanUp = "XColors_Cleanup"

       ; return the colorInfo information as a structure if this program was called as a modal widget.
    if (Keyword_Set(modal) and n_elements(group) ne 0 and needColorInfo) or (noblock eq 0 and needColorInfo)then begin
       stuff = *colorInfoPtr
       ptr_free, colorInfoPtr
       colorInfoPtr = stuff
    endif

       ; set current CT selection
    widget_control, list, set_list_select = colTbl
    XColors_ColorTable, {top: wTopBase, index : colTbl}
end
