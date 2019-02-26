;_____________________________IOISIOI____________________
; NAME:
;      C_s3DTrackingNode
;
; PURPOSE:
;       - Tracking graph-Basic-Class.
;
; AUTHOR:
;     FASL 2011
;     e_mail: We need a Scian mail :_D
;     fsantibanez@cefop.udec.cl
; CALLING SEQUENCE:
;       result = obj_new('C_s3DTrackingNode' )
;
; METHOHDS:
;_____________________________IOISIOI____________________
                  
function C_s3DTrackingNode::getIndexConexiones
  whereConexiones = where((*self.indexConexiones) ne -1.0d,/L64)
  if(whereConexiones[0] ne -1.0d) then begin
    return, (*self.indexConexiones)[whereConexiones]
  endif
  return, whereConexiones[0]
end
function C_s3DTrackingNode::getIndexNumberHerederos
  whereConexiones = where((*self.indexNumberHerederos) ne -1.0d,/L64)
  if(whereConexiones[0] ne -1.0d) then begin
    return, (*self.indexNumberHerederos)[whereConexiones]
  endif
  return, whereConexiones[0]
end
function C_s3DTrackingNode::getIndexTimeHerederos
  whereConexiones = where((*self.indexTimeHerederos) ne -1.0d,/L64)
  if(whereConexiones[0] ne -1.0d) then begin
    return, (*self.indexTimeHerederos)[whereConexiones]
  endif
  return, whereConexiones[0]
end
; Gets
function C_s3DTrackingNode::getActualColor
  return, self.color
end
function C_s3DTrackingNode::getInUse
  return, self.inUse
end
function C_s3DTrackingNode::getInView
  return, self.inView
end
function C_s3DTrackingNode::getIsRoot
  return, self.isroot
end
function C_s3DTrackingNode::getOwnNumber
  return, self.ownNumber
end
function C_s3DTrackingNode::getOwnIndex
  return, self.ownIndex
end
function C_s3DTrackingNode::getOwnTime
  return, self.ownTime
end
function C_s3DTrackingNode::getOwnCluster
  return, self.ownCluster
end
function C_s3DTrackingNode::getOwnRoot
  return, self.ownRoot
end
function C_s3DTrackingNode::getOwnParent
  return, self.ownParent
end
function C_s3DTrackingNode::getNumConexiones
  return, self.numConexiones
end

function C_s3DTrackingNode::getNumHerederos
  return, self.numHerederos
end
function C_s3DTrackingNode::getCenter
  return, self.centerXYZ
end

function C_s3DTrackingNode::getIsDivision
  return, self.isDivision
end
function C_s3DTrackingNode::getAngleDiv
  return, self.angleDiv
end
function C_s3DTrackingNode::getPuntoDiv
  return, self.puntoDiv
end
function C_s3DTrackingNode::getNormalDiv
  return, self.normalDiv
end

; Sets

pro C_s3DTrackingNode::setActualColor, vValue = vValue
  self.color  = vValue
end

pro C_s3DTrackingNode::setInUse, vValue = vValue
  self.inUse  = vValue
end
pro C_s3DTrackingNode::setInView, vValue = vValue
  self.inView  = vValue
end
pro C_s3DTrackingNode::setIsRoot, vValue = vValue
  self.isroot = vValue
end
pro C_s3DTrackingNode::setOwnIndex, vValue = vValue
  self.ownIndex = vValue
end
pro C_s3DTrackingNode::setOwnNumber, vValue = vValue
  self.ownNumber = vValue
end
pro C_s3DTrackingNode::setOwnTime, vValue = vValue
  self.ownTime = vValue
end
pro C_s3DTrackingNode::setOwnCluster, vValue = vValue
  self.ownCluster = vValue
end
pro C_s3DTrackingNode::setOwnRoot, vValue = vValue
  self.ownRoot = vValue
end
pro C_s3DTrackingNode::setOwnParent, vValue = vValue
  self.ownParent = vValue
end

pro C_s3DTrackingNode::setNumConexiones, vValue = vValue
  self.numConexiones = vValue
end
pro C_s3DTrackingNode::setCenter, vValue = vValue
  self.centerXYZ = vValue
end

pro C_s3DTrackingNode::setIsDivision, vValue = vValue
  self.isDivision = vValue
end
pro C_s3DTrackingNode::setAngleDiv, vValue = vValue
  self.angleDiv = vValue
end
pro C_s3DTrackingNode::setPuntoDiv, vValue = vValue
  self.puntoDiv = vValue
end
pro C_s3DTrackingNode::setNormalDiv, vValue = vValue
  self.normalDiv = vValue
end

; Funciones
pro C_s3DTrackingNode::AddIndexConexiones, ownIndexConexiones = ownIndexConexiones
    dummy = [ownIndexConexiones]
  if((*self.indexConexiones)[0] eq -1.0d) then begin
    self.indexConexiones = ptr_new(dummy,/no_copy)
  endif else begin
    tempWhere = where(*self.indexConexiones eq ownIndexConexiones)
    if(tempWhere[0] eq -1) then begin
       self.indexConexiones = ptr_new([*self.indexConexiones , dummy],/no_copy)
    endif
  endelse
  self.numConexiones++
end
  
pro C_s3DTrackingNode::DelIndexConexiones, ownIndexConexiones = ownIndexConexiones
  whereKeep = where((*self.indexConexiones) ne ownIndexConexiones,/L64)
  if(whereKeep[0] ne -1) then begin
    dummy = (*self.indexConexiones)[whereKeep]
    self.indexConexiones = ptr_new(dummy,/no_copy)
    self.numConexiones--
  endif else begin
    dummy = [-1.0d]
    self.indexConexiones = ptr_new(dummy,/no_copy)
    self.numConexiones = 0.0d
  endelse
end


pro C_s3DTrackingNode::AddIndexHerederos, ownIndexNumberHerederos = ownIndexNumberHerederos, ownTimeHerederos = ownTimeHerederos 
    dummy = [ownIndexNumberHerederos]
    dummyTime = [ownTimeHerederos]

  if((*self.indexNumberHerederos)[0] eq -1.0d) then begin
    self.indexNumberHerederos = ptr_new(dummy,/no_copy)
    self.indexTimeHerederos = ptr_new(dummyTime,/no_copy)
  endif else begin
    self.indexNumberHerederos = ptr_new([*self.indexNumberHerederos , dummy],/no_copy)
    self.indexTimeHerederos = ptr_new([*self.indexTimeHerederos , dummyTime],/no_copy)
  endelse
  self.numHerederos++
end
  
pro C_s3DTrackingNode::DelIndexHerederos, ownIndexConexiones = ownIndexConexiones
  whereKeep = where((*self.indexNumberHerederos) ne ownIndexConexiones,/L64)
  if(whereKeep[0] ne -1) then begin
    dummy = (*self.indexNumberHerederos)[whereKeep]
    dummyTime = (*self.indexTimeHerederos)[whereKeep]
    self.indexNumberHerederos = ptr_new(dummy,/no_copy)
    self.indexTimeHerederos = ptr_new(dummyTime,/no_copy)
    self.numHerederos--
  endif else begin
    dummy = [-1.0d]
    dummyTime = [-1.0d]
    self.indexNumberHerederos = ptr_new(dummy,/no_copy)
    self.indexTimeHerederos = ptr_new(dummyTime,/no_copy)
    self.numHerederos = 0.0d
  endelse
end

function C_s3DTrackingNode::init
    ; Parameters of C_s3DTrackingNode.
    
    dummy = [-1.0d]
    self.indexConexiones = ptr_new(dummy,/no_copy)
    dummy = [-1.0d]
    self.indexNumberHerederos = ptr_new(dummy,/no_copy)
    dummy = [-1.0d]
    self.indexTimeHerederos = ptr_new(dummy,/no_copy)

    self.isDivision  = 0b
    self.ownParent  = -1
    self.numConexiones = 0
    self.numHerederos  = 0
    self.color = [255,255,255]    
    return, 1
end

pro C_s3DTrackingNode::cleanup
  if(self.numConexiones gt 0) then ptr_free, self.indexConexiones
  if(self.numHerederos gt 0) then ptr_free, self.indexTimeHerederos
  if(self.numHerederos gt 0) then ptr_free, self.indexNumberHerederos
end

pro C_s3DTrackingNode__define
   interClass = { C_s3DTrackingNode, $
                  color : fltArr(3) , $         ;
                  inUse            : 0b, $         ;
                  inView           : 0b, $ 
                  isroot           : 0b, $         ;
   
                  isDivision       : 0b, $         ;
                  angleDiv         :0.0, $         ;
                  puntoDiv         : fltArr(3) , $         ;
                  normalDiv        : fltArr(3) , $         ;

                  ownIndex        : -1, $          ; Indice de este elemento .. relevante para relacion de nodos
                  ownNumber       : -1, $          ; numero de objeto (ROI), starts from 0
                  ownTime         : -1, $          ; tiempo
                  ownCluster      : -1, $          ; num cluster
                  ownRoot         : -1, $             ; Indice del elemento root
                  ownParent       : -1, $             ; Indice del padre
                  
                  numConexiones    : 0, $          ;
                  indexConexiones  :  ptr_new(), $          ;

                  numHerederos    : 0, $          ;
                  indexNumberHerederos  :  ptr_new(), $          ;
                  indexTimeHerederos  :  ptr_new(), $          ;
                  
                  centerXYZ        : dblarr(3)$
                }
end