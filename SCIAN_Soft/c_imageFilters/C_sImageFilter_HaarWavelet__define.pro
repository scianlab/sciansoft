;_____________________________IOISIOI____________________
; NAME:
;      C_sImageFilter_HaarWavelet
;
; PURPOSE:
;       - HaarWavelet-Filter-Class.
;
; AUTHOR:
;     loyarzo (2003)
;     e_mail: loyarzo@gmx.net
;
; CALLING SEQUENCE:
;       result = obj_new('C_sImageFilter_HaarWavelet' )
;
; METHOHDS:
;   function  ->apply, image = image                                   ; Image Data Matrix
;   pro ->set, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;   pro ->get, pParamStruct = pParamStruct         ;pParamStruct Pointer on filterStruct-Data
;_____________________________IOISIOI____________________

function C_sImageFilter_HaarWavelet::getImageFilterType
    return, 'Single_Image_Filter_Method'
end

function C_sImageFilter_HaarWavelet::apply, image = image
    if (size(image, /n_dim) ne 2) then return, image

    ; get dim image
    dimI = size (image, /dim)
    rowSize = dimI[0]
    xSize = dimI[0]
    ySize = dimI[1]

    ; get level
    level = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'Level'))[0]]

    vn = fltArr( dimI[0] , dimI[1] )
    vn[*,*] = image[*,*]
    un = fltArr( dimI[0] , dimI[1] )

    for i = 0,level-1 do begin

       xSizeRed = floor(xSize * 0.5);
       ySizeRed = floor(ySize * 0.5);
       ;un[0:xSizeRed-1, 0:ySizeRed-1] = 0
       ;print, xSizeRed
       ;print, ySizeRed

       if ( floor(xSize mod 2.) ) then xBroken = 1 else xBroken = 0
       if ( floor(ySize mod 2.) ) then yBroken = 1 else yBroken = 0

       ;print, xBroken
       ;print, yBroken
       ;print, "------"

        for x=0, xSizeRed-1 do begin
            for y=0, ySizeRed-1 do begin
                ij1 = 1. * vn[ 2 * x     , 2 * y  ]
                ij2 = 1. * vn[ 2 * x + 1 , 2 * y  ]
                ij3 = 1. * vn[ 2 * x     , 2 * y + 1 ]
                ij4 = 1. * vn[ 2 * x + 1 , 2 * y + 1 ]

                vn[ x , y ] = (ij1 + ij2 + ij3 + ij4) * 0.25
                un[ x , y ] = (ij1 + ij2 + ij3 + ij4) * 0.25
                un[ (x + xSizeRed + xBroken) , y ] = ij1 - ij2 + ij3 - ij4
                un[ x , (y + ySizeRed + yBroken) ] = ij1 + ij2 - ij3 - ij4
                un[ (x + xSizeRed + xBroken) , (y + ySizeRed + yBroken) ] =ij1 - ij2 - ij3 + ij4

            endfor
        endfor

        if (yBroken) then begin
         ;print , "yBroken"
         ;print , i
         for x=0, xSizeRed-1 do begin
          vn[ x , ySizeRed] = 0.25 *  (vn[x , y] + vn[(x+1) , y] + vn[x , (y+1)] + vn[(x+1) , (y+1)])
          un[ x , ySizeRed] = 0.25 *  (un[x , y] + un[(x+1) , y] + un[x , (y+1)] + un[(x+1) , (y+1)])
            endfor
        endif

        if (xBroken) then begin
         ;print , "xBroken"
         ;print , i
         for y=0, ySizeRed-1 do begin
          vn[xSizeRed , y] = 0.25 * (vn[x , y] + vn[(x+1) , y] + vn[x , (y+1)] + vn[(x+1) , (y+1)])
          un[xSizeRed , y] = 0.25 * (un[x , y] + un[(x+1) , y] + un[x , (y+1)] + un[(x+1) , (y+1)])
         endfor
       endif

       vn = un[0:xSizeRed-1, 0:ySizeRed-1]
       xSize = xSizeRed
       ySize = ySizeRed

    endfor

    ;begin test
    ;xSize = dimI[0]
    ;ySize = dimI[1]
    ;for x=0, xSize-1 do begin
    ;    for y=0, ySize-1 do begin
    ;        print, un[ x , y ]
    ;    endfor
    ;endfor
    ;end test


    ;show Wavelet coefficient
    whereShow = (where((*(*self.pParamStruct).pNames) eq 'Show All Image'))[0]
    if ( (*(*self.pParamStruct).pActive)[whereShow] eq 1) then begin
        ;print, "Show All Image"
        return, un
    endif

    ;Show IO with IP
    whereShow = (where((*(*self.pParamStruct).pNames) eq 'Show IP'))[0]
    if ( (*(*self.pParamStruct).pActive)[whereShow] eq 1) then begin
        ;print, "Show IP"

        ; get NumInterestPoint
        NumInterestPoint = (*(*self.pParamStruct).pValues)[(where((*(*self.pParamStruct).pNames) eq 'NumInterestPoint'))[0]]
        NumInterestPointSum = floor(NumInterestPoint / 6 )


        xSizeRed = dimI[0]
        ySizeRed = dimI[1]
        IntPoints1 = fltArr( 6 , floor( NumInterestPointSum ) )
        sumAll = fltArr( dimI[0] , dimI[1] )

        for i = 0,level-1 do begin  ;Search size of image S
           xSizeRed = floor(xSizeRed * 0.5)
           ySizeRed = floor(ySizeRed * 0.5)
        endfor

        ;BEGIN SEARCH INTEREST POINT IN LEVELS
        for i = 0,level-1 do begin

            ;begin level 0
            if ( i eq 0 ) then begin
                xSize = (xSizeRed * 2)
               ySize = (ySizeRed * 2)

                if ( floor(xSize mod 2.) ) then xBroken = 1  else xBroken = 0
               if ( floor(ySize mod 2.) ) then yBroken = 1  else yBroken = 0

               sum = fltArr( xSizeRed , ySizeRed )

               ;begin derive sum0
               for x=0, xSizeRed-1 do begin
                 for y=0, ySizeRed-1 do begin
                  sum[x , y] = 0.
                  sum[x , y] = ABS (ABS(un[(x+xSizeRed+xBroken) , y]) + ABS(un[ x , (y+ySizeRed+yBroken)]) + ABS(un[(x+xSizeRed+xBroken) , (y+ySizeRed+yBroken)]))
                 endfor
               endfor
               ;end derive sum0

               ;search IP in sum0
               for j=0, floor(NumInterestPointSum * 0.5)-1 do begin
                    maxSum = max(sum)
              where_ = where(sum eq maxSum)
                   IntPoints1[i,j] = where_[0] ;IP[0,*]
                   if ( IntPoints1[i,j] ne -1 ) then begin
                     sum[IntPoints1[i,j]] = 0
                   endif
                endfor
                for j=floor(NumInterestPointSum * 0.5), NumInterestPointSum-1 do begin
                    maxSum = min(sum)
              where_ = where(sum eq maxSum)
                   IntPoints1[i,j] = where_[0] ;IP[0,*]
                   if ( IntPoints1[i,j] ne -1 ) then begin
                     sum[IntPoints1[i,j]] = 0
                   endif
                endfor

                ;begin search IP in sum3
                    sum = un[xSizeRed:xSize-1,0:ySizeRed-1]
                    for j=0, floor(NumInterestPointSum * 0.5)-1 do begin
                        maxSum = max(sum)
                 where_ = where(sum eq maxSum)
                       IntPoints1[3,j] = where_[0] ;IP[3,*]
                       if ( IntPoints1[3,j] ne -1 ) then begin
                         sum[IntPoints1[3,j]] = 0
                       endif
                    endfor
                    for j=floor(NumInterestPointSum * 0.5), NumInterestPointSum-1 do begin
                        maxSum = min(sum)
                 where_ = where(sum eq maxSum)
                       IntPoints1[3,j] = where_[0] ;IP[3,*]
                       if ( IntPoints1[3,j] ne -1 ) then begin
                         sum[IntPoints1[3,j]] = 0
                       endif
                    endfor
                ;end search IP in sum3

                ;begin search IP in sum4
                    sum = un[xSizeRed:xSize-1,ySizeRed:ySize-1]
                    for j=0, floor(NumInterestPointSum * 0.5)-1 do begin
                        maxSum = max(sum)
                       where_ = where(sum eq maxSum)
                       IntPoints1[4,j] = where_[0] ;IP[4,*]
                       if ( IntPoints1[4,j] ne -1 ) then begin
                         sum[IntPoints1[4,j]] = 0
                       endif
                    endfor
                    for j=floor(NumInterestPointSum * 0.5), NumInterestPointSum-1 do begin
                        maxSum = min(sum)
                       where_ = where(sum eq maxSum)
                       IntPoints1[4,j] = where_[0] ;IP[4,*]
                       if ( IntPoints1[4,j] ne -1 ) then begin
                         sum[IntPoints1[4,j]] = 0
                       endif
                    endfor
                ;end search IP in sum4

                ;begin search IP in sum5
                    sum = un[0:xSizeRed-1,ySizeRed:ySize-1]
                    for j=0, floor(NumInterestPointSum * 0.5)-1 do begin
                        maxSum = max(sum)
                       where_ = where(sum eq maxSum)
                       IntPoints1[5,j] = where_[0] ;IP[5,*]
                       if ( IntPoints1[5,j] ne -1 ) then begin
                         sum[IntPoints1[5,j]] = 0
                       endif
                    endfor
                    for j=floor(NumInterestPointSum * 0.5), NumInterestPointSum-1 do begin
                        maxSum = min(sum)
                       where_ = where(sum eq maxSum)
                       IntPoints1[5,j] = where_[0] ;IP[5,*]
                       if ( IntPoints1[5,j] ne -1 ) then begin
                         sum[IntPoints1[5,j]] = 0
                       endif
                    endfor
                ;end search IP in sum5

                xSizeRed = xSize
               ySizeRed = ySize
            endif
            ;end level 0

            ;begin level 1
            if ( i eq 1 ) then begin
                xSize = (xSizeRed * 2)
               ySize = (ySizeRed * 2)

                if ( floor(xSize mod 2.) ) then xBroken = 1  else xBroken = 0
               if ( floor(ySize mod 2.) ) then yBroken = 1  else yBroken = 0

               sum = fltArr( xSizeRed , ySizeRed )

               ;begin derive sum1
               for x=0, xSizeRed-1 do begin
                 for y=0, ySizeRed-1 do begin
                  sum[x , y] = 0.
                  sum[x , y] = ABS (ABS(un[(x+xSizeRed+xBroken) , y]) + ABS(un[ x , (y+ySizeRed+yBroken)]) + ABS(un[(x+xSizeRed+xBroken) , (y+ySizeRed+yBroken)]))
                 endfor
               endfor
               ;end derive sum1

               ;search IP in sum1
               for j=0, floor(NumInterestPointSum * 0.5)-1 do begin
                    maxSum = max(sum)
              where_ = where(sum eq maxSum)
                   IntPoints1[1,j] = where_[0] ;IP[1,*]
                   if ( IntPoints1[1,j] ne -1 ) then begin
                     sum[IntPoints1[1,j]] = 0
                   endif
                endfor
                for j=floor(NumInterestPointSum * 0.5), NumInterestPointSum-1 do begin
                    maxSum = min(sum)
              where_ = where(sum eq maxSum)
                   IntPoints1[1,j] = where_[0] ;IP[1,*]
                   if ( IntPoints1[1,j] ne -1 ) then begin
                     sum[IntPoints1[1,j]] = 0
                   endif
                endfor


                ;tracking IP sum3
                 sum = un[xSizeRed:xSize-1,0:ySizeRed-1]
                    for j=0, NumInterestPointSum-1 do begin
                       ;Get position in (x,y)
                       pos = IntPoints1[3,j]
                       posx = (pos mod (floor(xSizeRed*0.5)) )*2
                       posy = ((floor(pos/(floor(xSizeRed*0.5))) )*2)

                 maxAux = sum[posx,posy]
                 IntPoints1[3,j] = (posy*xSizeRed) + (posx mod xSizeRed)

                 if ((sum[posx+1,posy]) gt maxAux) then begin
                   maxAux = sum[posx+1,posy]
                   IntPoints1[3,j] = (posy*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                 if ((sum[posx,posy+1]) gt maxAux) then begin
                   maxAux = sum[posx,posy+1]
                   IntPoints1[3,j] = ((posy+1)*xSizeRed) + (posx mod xSizeRed)
                 endif
                 if ((sum[posx+1,posy+1]) gt maxAux) then begin
                   maxAux = sum[posx+1,posy+1]
                   IntPoints1[3,j] = ((posy+1)*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                    endfor

                ;tracking IP sum4
                 sum = un[xSizeRed:xSize-1,ySizeRed:ySize-1]
                 for j=0, NumInterestPointSum-1 do begin
                       ;Get position in (x,y)
                       pos = IntPoints1[4,j]
                       posx = (pos mod (floor(xSizeRed*0.5)) )*2
                       posy = (( floor(pos/(floor(xSizeRed*0.5))) )*2)

                 maxAux = sum[posx,posy]
                 IntPoints1[4,j] = (posy*xSizeRed) + (posx mod xSizeRed)

                 if ((sum[posx+1,posy]) gt maxAux) then begin
                   maxAux = sum[posx+1,posy]
                   IntPoints1[4,j] = (posy*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                 if ((sum[posx,posy+1]) gt maxAux) then begin
                   maxAux = sum[posx,posy+1]
                   IntPoints1[4,j] = ((posy+1)*xSizeRed) + (posx mod xSizeRed)
                 endif
                 if ((sum[posx+1,posy+1]) gt maxAux) then begin
                   maxAux = sum[posx+1,posy+1]
                   IntPoints1[4,j] = ((posy+1)*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                    endfor

                ;tracking IP sum5
                    sum = un[0:xSizeRed-1,ySizeRed:ySize-1]
                 for j=0, NumInterestPointSum-1 do begin
                       ;Get position in (x,y)
                       pos = IntPoints1[5,j]
                       posx = (pos mod (floor(xSizeRed*0.5)) )*2
                       posy = (( floor(pos/(floor(xSizeRed*0.5)) ))*2)

                 maxAux = sum[posx,posy]
                 IntPoints1[5,j] = (posy*xSizeRed) + (posx mod xSizeRed)

                 if ((sum[posx+1,posy]) gt maxAux) then begin
                   maxAux = sum[posx+1,posy]
                   IntPoints1[5,j] = (posy*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                 if ((sum[posx,posy+1]) gt maxAux) then begin
                   maxAux = sum[posx,posy+1]
                   IntPoints1[5,j] = ((posy+1)*xSizeRed) + (posx mod xSizeRed)
                 endif
                 if ((sum[posx+1,posy+1]) gt maxAux) then begin
                   maxAux = sum[posx+1,posy+1]
                   IntPoints1[5,j] = ((posy+1)*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                    endfor

                xSizeRed = xSize
               ySizeRed = ySize
            endif
            ;end level 1

            ;begin level 2
            if ( i eq 2 ) then begin
                xSize = (xSizeRed * 2)
               ySize = (ySizeRed * 2)

                if ( floor(xSize mod 2.) ) then xBroken = 1  else xBroken = 0
               if ( floor(ySize mod 2.) ) then yBroken = 1  else yBroken = 0

               sum = fltArr( xSizeRed , ySizeRed )

               ;begin derive sum2
               for x=0, xSizeRed-1 do begin
                 for y=0, ySizeRed-1 do begin
                  sum[x , y] = 0.
                  sum[x , y] = ABS (ABS(un[(x+xSizeRed+xBroken) , y]) + ABS(un[ x , (y+ySizeRed+yBroken)]) + ABS(un[(x+xSizeRed+xBroken) , (y+ySizeRed+yBroken)]))
                 endfor
               endfor
               ;end derive sum2
               ;sumAll[xSizeRed:xSize-1,ySizeRed:ySize-1] = sum

               ;search IP in sum2
               for j=0, floor(NumInterestPointSum * 0.5)-1 do begin
                    maxSum = max(sum)
              where_ = where(sum eq maxSum)
                   IntPoints1[2,j] = where_[0] ;IP[2,*]
                   if ( IntPoints1[2,j] ne -1 ) then begin
                     sum[IntPoints1[2,j]] = 0
                   endif
                endfor
                for j=floor(NumInterestPointSum * 0.5), NumInterestPointSum-1 do begin
                    maxSum = min(sum)
              where_ = where(sum eq maxSum)
                   IntPoints1[2,j] = where_[0] ;IP[2,*]
                   if ( IntPoints1[2,j] ne -1 ) then begin
                     sum[IntPoints1[2,j]] = 0
                   endif
                endfor


                ;tracking IP sum2
                for j=0, NumInterestPointSum-1 do begin
                       ;Get position in (x,y)
                       pos = IntPoints1[2,j]
                       posx = floor( pos mod (floor(xSizeRed)) )*2
                       posy = ((floor(pos/(floor(xSizeRed)) ))*2)

                 maxAux = image[posx,posy]
                 IntPoints1[2,j] = (posy*xSize) + (posx mod xSize)

                 if ((image[posx+1,posy]) gt maxAux) then begin
                   maxAux = image[posx+1,posy]
                   IntPoints1[2,j] = (posy*xSize) + ((posx+1) mod xSize)
                 endif
                 if ((image[posx,posy+1]) gt maxAux) then begin
                   maxAux = image[posx,posy+1]
                   IntPoints1[2,j] = ((posy+1)*xSize) + (posx mod xSize)
                 endif
                 if ((image[posx+1,posy+1]) gt maxAux) then begin
                   maxAux = image[posx+1,posy+1]
                   IntPoints1[2,j] = ((posy+1)*xSize) + ((posx+1) mod xSize)
                 endif
                    endfor

                ;tracking IP sum3
                 sum = un[xSizeRed:xSize-1,0:ySizeRed-1]
                    for j=0, NumInterestPointSum-1 do begin
                       ;Get position in (x,y)
                       pos = IntPoints1[3,j]
                       posx = (pos mod (floor(xSizeRed*0.5)) )*2
                       posy = (( floor(pos/(floor(xSizeRed*0.5))) )*2)

                 maxAux = image[posx,posy]
                 IntPoints1[3,j] = (posy*xSizeRed) + (posx mod xSizeRed)

                 if ((sum[posx+1,posy]) gt maxAux) then begin
                   maxAux = image[posx+1,posy]
                   IntPoints1[3,j] = (posy*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                 if ((image[posx,posy+1]) gt maxAux) then begin
                   maxAux = image[posx,posy+1]
                   IntPoints1[3,j] = ((posy+1)*xSizeRed) + (posx mod xSizeRed)
                 endif
                 if ((image[posx+1,posy+1]) gt maxAux) then begin
                   maxAux = image[posx+1,posy+1]
                   IntPoints1[3,j] = ((posy+1)*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                    endfor

                ;tracking IP sum4
                 sum = un[xSizeRed:xSize-1,ySizeRed:ySize-1]
                 for j=0, NumInterestPointSum-1 do begin
                       ;Get position in (x,y)
                       pos = IntPoints1[4,j]
                       posx = (pos mod (floor(xSizeRed*0.5)) )*2
                       posy = ((floor(pos/(floor(xSizeRed*0.5))) )*2)

                 maxAux = image[posx,posy]
                 IntPoints1[4,j] = (posy*xSizeRed) + (posx mod xSizeRed)

                 if ((image[posx+1,posy]) gt maxAux) then begin
                   maxAux = image[posx+1,posy]
                   IntPoints1[4,j] = (posy*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                 if ((image[posx,posy+1]) gt maxAux) then begin
                   maxAux = image[posx,posy+1]
                   IntPoints1[4,j] = ((posy+1)*xSizeRed) + (posx mod xSizeRed)
                 endif
                 if ((image[posx+1,posy+1]) gt maxAux) then begin
                   maxAux = image[posx+1,posy+1]
                   IntPoints1[4,j] = ((posy+1)*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                    endfor

                ;tracking IP sum5
                    sum = un[0:xSizeRed-1,ySizeRed:ySize-1]
                 for j=0, NumInterestPointSum-1 do begin
                       ;Get position in (x,y)
                       pos = IntPoints1[5,j]
                       posx = (pos mod (floor(xSizeRed*0.5)) )*2
                       posy = ((floor(pos/(floor(xSizeRed*0.5))) )*2)

                 maxAux = image[posx,posy]
                 IntPoints1[5,j] = (posy*xSizeRed) + (posx mod xSizeRed)

                 if ((image[posx+1,posy]) gt maxAux) then begin
                   maxAux = image[posx+1,posy]
                   IntPoints1[5,j] = (posy*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                 if ((image[posx,posy+1]) gt maxAux) then begin
                   maxAux = image[posx,posy+1]
                   IntPoints1[5,j] = ((posy+1)*xSizeRed) + (posx mod xSizeRed)
                 endif
                 if ((image[posx+1,posy+1]) gt maxAux) then begin
                   maxAux = image[posx+1,posy+1]
                   IntPoints1[5,j] = ((posy+1)*xSizeRed) + ((posx+1) mod xSizeRed)
                 endif
                    endfor
            endif
            ;end level 2

        endfor
        ;END SEARCH INTEREST POINT IN LEVELS

        ;tracking IP to Image Original
            for i = 3,(level*2)-1 do begin
                for j=0, NumInterestPointSum-1 do begin
                   ;Get position in (x,y)
                 pos = IntPoints1[i,j]
                   posx = (pos mod xSizeRed )*2
                   posy = (floor(pos/xSizeRed) )*2

              maxAux = image[posx,posy]
              IntPoints1[i,j] = (posy*xSize) + (posx mod xSize)

              if ((image[posx+1,posy]) gt maxAux) then begin
                 maxAux = image[posx+1,posy]
                 IntPoints1[i,j] = (posy*xSize) + ((posx+1) mod xSize)
              endif
              if ((image[posx,posy+1]) gt maxAux) then begin
                 maxAux = image[posx,posy+1]
                 IntPoints1[i,j] = ((posy+1)*xSize) + (posx mod xSize)
              endif
              if ((image[posx+1,posy+1]) gt maxAux) then begin
                 maxAux = image[posx+1,posy+1]
                 IntPoints1[i,j] = ((posy+1)*xSize) + ((posx+1) mod xSize)
              endif
                endfor
            endfor


       xSizeRed = xSize
       ySizeRed = ySize

       ;print, IntPoints1
       ;for j=0, NumInterestPoint-1 do begin
       for i = 2,(level*2)-1 do begin
         for j=0, NumInterestPointSum-1 do begin
             ;Get position in (x,y)
          ;pos = IntPoints1[i,j]
          pos = IntPoints1[i,j]
             posx = (pos mod xSizeRed )
             posy = (floor(pos/xSizeRed) )

          image[posx,posy] = 254

           endfor

        endfor

        ;image(where(image ne 254))  0 = ;Only IP
       return, image
    endif
end


function C_sImageFilter_HaarWavelet::init

    filterStruct = {Name: 'C_HaarWavelet',$     ;  Filter Name.
                           pWidgetType: ptr_new(),$   ; Pointer on Filter Parameter Names.
                           pNames: ptr_new(),$       ; Pointer on Filter Parameter Names.
                           pActive: ptr_new(),$      ; Pointer on Filter Parameter Active Bool.
                           pMin: ptr_new(),$      ; Pointer on Filter Parameter Min_Values.
                           pMax: ptr_new(),$         ; Pointer on Filter Parameter Max_Values.
                           pValues:ptr_new()  $       ; Pointer on Filter Parameter Values.
                       }

       ; Parameters of C_HaarWavelet.
    filterParamWidgetType = make_array(4, /string, value = 'widget_slider')
    filterParamNames  = ['Level' , 'NumInterestPoint' , 'Show All Image' , 'Show IP' ] ;  Normalize HaarWavelet Values ->
    filterParamActive  = [1,1  ,0,1]
    filterParamMin  = [1,1  ,0,0]
    filterParamMax  = [3,1200,1,1]
    filterParamValues  = [3,600,0,0]

    filterStruct.pWidgetType = ptr_new(filterParamWidgetType, /no_copy)
    filterStruct.pNames = ptr_new(filterParamNames, /no_copy)
    filterStruct.pActive = ptr_new(filterParamActive, /no_copy)
    filterStruct.pMin = ptr_new(filterParamMin, /no_copy)
    filterStruct.pMax = ptr_new(filterParamMax, /no_copy)
    filterStruct.pValues = ptr_new(filterParamValues, /no_copy)

    self.pParamStruct = ptr_new(filterStruct, /no_copy)
    return, 1
end

pro C_sImageFilter_HaarWavelet__define
  tmp = {C_sImageFilter_HaarWavelet, pParamStruct: ptr_new(), inherits C_sImageFilter}
end
