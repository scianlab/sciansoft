;_____________________________IOISIOI____________________
; NAME:
;      C_s3GraphEdge
;
; PURPOSE:
;       - graph-EDGE_Basic-Class.
;
; AUTHOR:
;     FASL 2012
;     e_mail: fsantibanez@med.uchile.cl
; CALLING SEQUENCE:
;       result = obj_new('C_s3GraphEdge' )
;
; METHOHDS:
;_____________________________IOISIOI____________________
                  
; Gets
function C_s3GraphEdge::getOwnInitVertex
  return, self.ownVertexIndexInit
end
function C_s3GraphEdge::getOwnEndVertex
  return, self.ownVertexIndexEnd
end
function C_s3GraphEdge::getOwnNormal
  return, self.normalVector
end

function C_s3GraphEdge::getOwnLevel
  return, self.ownLevel
end

function C_s3GraphEdge::getWaiting
  return, self.isWaiting
end

function C_s3GraphEdge::getAngleMeanChildren
  return, self.angleMeanChildren
end
function C_s3GraphEdge::getLenghtMeanChildren
  return, self.lenghtMeanChildren
end
function C_s3GraphEdge::getAngleEdge
  return, self.angleEdge
end

function C_s3GraphEdge::getLenghtEdgeReal
  return, self.lenghtEdgeReal
end

function C_s3GraphEdge::getLenghtEdge
  return, self.lenghtEdge
end

function C_s3GraphEdge::getInitXYZ
  return, self.initXYZ
end
function C_s3GraphEdge::getEndXYZ
  return, self.endXYZ
end

function C_s3GraphEdge::getActualColor
  return, self.color
end

function C_s3GraphEdge::getIsRoot
  return, self.isroot
end

function C_s3GraphEdge::getOwnIndex
  return, self.ownIndex
end

function C_s3GraphEdge::getOwnRoot
  return, self.ownRoot
end

function C_s3GraphEdge::getOwnParent
  return, self.ownParent
end

function C_s3GraphEdge::getNumConexiones
  return, self.numConexiones
end

function C_s3GraphEdge::getIndexConexiones
  whereConexiones = where((*self.indexConexiones) ne -1.0d,/L64)
  if(whereConexiones[0] ne -1.0d) then begin
    return, (*self.indexConexiones)[whereConexiones]
  endif
  return, whereConexiones[0]
end

; Sets
pro C_s3GraphEdge::setOwnInitVertex, vValue = vValue
  self.ownVertexIndexInit = vValue
end
pro C_s3GraphEdge::setOwnEndVertex, vValue = vValue
  self.ownVertexIndexEnd = vValue
end
pro C_s3GraphEdge::setOwnNormal, vValue = vValue
  self.normalVector = vValue
end

pro C_s3GraphEdge::setOwnLevel, vValue = vValue
  self.ownLevel = vValue
end

pro C_s3GraphEdge::setAngleMeanChildren, vValue = vValue
  self.angleMeanChildren  = vValue
end

pro C_s3GraphEdge::setLenghtMeanChildren, vValue = vValue
  self.lenghtMeanChildren  = vValue
end

pro C_s3GraphEdge::setAngleEdge, vValue = vValue
  self.angleEdge  = vValue
end

pro C_s3GraphEdge::setLenghtEdge, vValue = vValue
  self.lenghtEdge  = vValue
end

pro C_s3GraphEdge::setLenghtEdgeReal, vValue = vValue
  self.lenghtEdgeReal  = vValue
end

pro C_s3GraphEdge::setWaiting, vValue = vValue
  self.isWaiting = vValue
end

pro C_s3GraphEdge::setInitXYZ, vValue = vValue
  self.initXYZ  = vValue
end

pro C_s3GraphEdge::setEndXYZ, vValue = vValue
  self.endXYZ  = vValue
end

pro C_s3GraphEdge::setActualColor, vValue = vValue
  self.color  = vValue
end

pro C_s3GraphEdge::setIsRoot, vValue = vValue
  self.isroot = vValue
end

pro C_s3GraphEdge::setOwnIndex, vValue = vValue
  self.ownIndex = vValue
end

pro C_s3GraphEdge::setOwnRoot, vValue = vValue
  self.ownRoot = vValue
end

pro C_s3GraphEdge::setOwnParent, vValue = vValue
  self.ownParent = vValue
end

pro C_s3GraphEdge::setNumConexiones, vValue = vValue
  self.numConexiones = vValue
end

; Funciones
pro C_s3GraphEdge::AddIndexConexiones, ownIndexConexiones = ownIndexConexiones
   dummy = [ownIndexConexiones]
  if((*self.indexConexiones)[0] eq -1.0d) then begin
    self.indexConexiones = ptr_new(dummy,/no_copy)
    self.numConexiones++
  endif else begin
    tempWhere = where(*self.indexConexiones eq ownIndexConexiones)
    if(tempWhere[0] eq -1) then begin
       self.indexConexiones = ptr_new([*self.indexConexiones , dummy],/no_copy)
       self.numConexiones++
    endif
  endelse
end
  
  
pro C_s3GraphEdge::DelIndexConexiones, ownIndexConexiones = ownIndexConexiones
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

function C_s3GraphEdge::init
    ; Parameters of C_s3GraphEdge.
    
    dummy = [-1.0d]
    self.indexConexiones = ptr_new(dummy,/no_copy)

    self.color = [255,255,255]    
    return, 1
end


pro C_s3GraphEdge::cleanup
  if(self.numConexiones gt 0) then ptr_free, self.indexConexiones
end


pro C_s3GraphEdge__define
   interClass = { C_s3GraphEdge, $
                  isWaiting         : 0b, $                 ;                  
                  color             : fltArr(3) , $         ;
                  isroot            : 0b, $                 ;

                  initXYZ           : dblarr(3),$
                  endXYZ            : dblarr(3),$
                                                             ; [Level_i, Mean] ... JOINT all Level_i for real means values 
                  angleMeanChildren :0.0, $                  ;  Promedio de los angulos del nivel siguiente.. en asociacion al nivel permite estimar el valor medio de angulo por nivel... asi cual esta es el nivel medio .. pero del nivel siguiente de mis descendientes..
                  lenghtMeanChildren:0.0, $                  ; Se debe calcular una vez que todos los valores del arbol han sido obtenidos.. recolectando los valores de los niveles inferiores...                  
                                                             ; cuando se aumenta el numero de conexiones de mi parent.. sumo mi angulo o mi largo .. al finalizar... los means.. deben ser ponderados.. es decir dividir el valor acumulado por el numero de conexiones...                  
  
                  angleEdge         :0.0, $                  ;  En relacion al anterior... no valido para el Root.. XD
                  lenghtEdge        :0.0, $
                  lenghtEdgeReal    :0.0, $        

                  ownVertexIndexInit : -1, $                   
                  ownVertexIndexEnd  : -1, $                   
                  normalVector       : dblarr(3),$
                                      
                  ownIndex          : -1, $                   ; Indice de este elemento .. relevante para relacion de nodos
                  ownRoot           : -1, $                   ; Indice del elemento root .. por ahora siempre deberia ser 0.. vere si sirve para algo en el contexto de uso actual
                  ownParent         : -1, $                   ; Indice del padre
                  
                  numConexiones     : 0, $                    ; Edges compartidas con el edge actual en el punto ENDXYZ... aqui se usa un arbol..          
                  indexConexiones   : ptr_new(), $            ; Indices de estos edges...

                  ownLevel          : 0 $                    ; una medida de la profundidad del arbol....    
                }
end
