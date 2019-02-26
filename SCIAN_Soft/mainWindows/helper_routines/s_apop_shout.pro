FUNCTION s_apop_shout, text					; text ist shOUTtext
; March'98, Steffen

BASE = WIDGET_BASE(title='Steffen_Hoppla!!!', /ROW)
b2 = WIDGET_BASE(BASE, /frame, /COLUMN)
draw = WIDGET_TEXT(b2, XSIZE= 30, YSIZE= 4>(Size(text))(1), /WRAP, VALUE = text)
WIDGET_CONTROL,  BASE, /REALIZE
WAIT, 2
WIDGET_CONTROL,  BASE, /DESTROY
Return,''

End
