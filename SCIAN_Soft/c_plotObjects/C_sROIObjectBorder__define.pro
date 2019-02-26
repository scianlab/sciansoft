;_____________________________IOISIOI____________________
; NAME:
;      C_sROIObjectBorder
;
; PURPOSE:
;       - Creates ROIObject for Object Polygons
;
; AUTHOR:
;		Dr. Steffen Härtel (2002)
;		e_mail: shaertel@physik.uni-bremen.de
;
; CALLING SEQUENCE:
;      	result = Obj_New('C_sROIObjectBorder' )
;
; METHOHDS:
;function C_sROIObjectBorder::init, Name=Name, Number=Number, *pFramePixSize = *pFramePixSize, WherePoints=WherePoints
										;	Number: Number of Object
										;	*pFramePixSize: 2D- or 3D-embedding Object-Space
										;	WherePoints: 2D- or 3D-Object-Coordinates
;_____________________________IOISIOI____________________

pro C_sROIObjectBorder::SetProperty, Number=Number, pFramePixSize = pFramePixSize, WherePoints=WherePoints
    if (n_elements(Number) ne 0) then self.Number = Number
    if (n_elements(pFramePixSize) ne 0) then begin
		if (Ptr_Valid(self.pFramePixSize)) then Ptr_Free, self.pFramePixSize
    	self.pFramePixSize = Ptr_New(*pFramePixSize)
    endif
    if (n_elements(WherePoints) ne 0) then begin
		if (Ptr_Valid(self.pWherePolygon)) then Ptr_Free, self.pWherePolygon
    	self.pWherePolygon = Ptr_New(WherePoints)
    endif
end

pro C_sROIObjectBorder::GetPProperty, Number=Number, pFramePixSize = pFramePixSize, pWherePolygon=pWherePolygon
    if keyword_set(Number) then Number = self.Number
    if keyword_set(pFramePixSize) then pFramePixSize = self.pFramePixSize
    if keyword_set(WherePoints) then pWherePolygon = self.pWherePolygon
end

pro C_sROIObjectBorder::cleanup
	; Cleanup any data stored in this class
	if (Ptr_Valid(self.pFramePixSize)) then Ptr_Free, self.pFramePixSize
	if (Ptr_Valid(self.pWherePolygon)) then Ptr_Free, self.pWherePolygon
	if (Ptr_Valid(self.pPointValues)) then Ptr_Free, self.pPointValues
	self->IDLgrROI::cleanup
end

function C_sROIObjectBorder::init, Number = Number,$					; Number of Object
													Color = Color,$							; Color of Object
													Interior = Interior,$
													pFramePixSize = pFramePixSize,$ 		; 2D- or 3D-embedding Object-Space
													pWherePolygon=pWherePolygon,$ 			; 2 D- or 3D-Polygon-Coordinates
													_extra = extra									; extra-Structure for IDLgrROI

    if not(keyword_set(color)) then color = [255,0,0]
    if keyword_set(Number) then self.Number = Number else self.Number = -1
    if keyword_set(pFramePixSize) then self.pFramePixSize = pFramePixSize

	xcoord_conv = [-1.*Floor((*self.pFramePixSize)[0]/2.)/Floor((*self.pFramePixSize)[0]), 1.0/ (*self.pFramePixSize)[0]]
	ycoord_conv = [-1.*Floor((*self.pFramePixSize)[1]/2.)/Floor((*self.pFramePixSize)[1]), 1.0/ (*self.pFramePixSize)[1]]
   	if (N_Elements(*self.pFramePixSize) EQ 2) then zcoord_conv = [0., 1.] $
   		else zcoord_conv = 	[-1.*Floor((*self.pFramePixSize)[2]/2.)/Floor((*self.pFramePixSize)[2]) , 1.0/ (*self.pFramePixSize)[2]]

    if keyword_set(_extra) then f= self->IDLgrROI::init(_extra = extra)
    if keyword_set(Interior) then f= self->IDLgrROI::init( pWherePolygon,$
	       									style = 2,$
	       									UValue = pParameterStrukt,$
	       									name = StrCompress(string(self.Number), /Remove),$
	       									color = color,$
	       									/Double,$
	       									/Interior,$
	      									xcoord_conv = xcoord_conv,$
											ycoord_conv = ycoord_conv,$
											zcoord_conv = zcoord_conv )

    if (not(keyword_set(_extra)) AND not( keyword_set(Interior))) then	f= self->IDLgrROI::init( pWherePolygon,$
	       									style = 2,$
	       									UValue = pParameterStrukt,$
	       									name = StrCompress(string(self.Number), /Remove),$
	       									color = color,$
	       									/Double,$
	      									xcoord_conv = xcoord_conv,$
											ycoord_conv = ycoord_conv,$
											zcoord_conv = zcoord_conv )

    return, 1
end

pro C_sROIObjectBorder__define
   tmp = { C_sROIObjectBorder, Number : 0,$
			      						pFramePixSize : Ptr_New(),$
			      						pWherePolygon : Ptr_New(),$
			      						pPointValues : Ptr_New(),$
			      						pParameterStrukt:  Ptr_New(),$
			      						inherits IDLgrROI $
			      						}
end
