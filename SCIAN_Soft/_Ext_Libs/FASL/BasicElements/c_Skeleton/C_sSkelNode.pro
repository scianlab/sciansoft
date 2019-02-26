;_____________________________IOISIOI____________________
; NAME:
;      C_sSkelNode
;
; PURPOSE:
;       - Skeltree-Basic-Class.
;
; AUTHOR:
;     FASL 2011
;     e_mail: We need a Scian mail :_D
;     fsantibanez@cefop.udec.cl
; CALLING SEQUENCE:
;       result = obj_new('C_sSkelNode' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sSkelNode::getInUse
  return, self._inUse
end
function C_sSkelNode::getOwnIndex
  return, self._ownIndex
end
function C_sSkelNode::getNumEdges
  return, self._numEdges
end
function C_sSkelNode::getIndexEdges
  return, (*self._indexEdges)
end
function C_sSkelNode::getEdgeLabels
  return, (*self._edgeLabels)
end
function C_sSkelNode::getDim
  return, self._dim
end
function C_sSkelNode::getDir
  return, self._Dir
end
function C_sSkelNode::getVertexPosition
  return, self._vertexNode
end
function C_sSkelNode::getWCentroid
  return, self._wCentroid
end
function C_sSkelNode::getNumPoints
  return, self._numPoints
end
function C_sSkelNode::getIndexOriginalPoints
  return, *(self._indexOriginalPoints)
end

pro C_sSkelNode::setInUse, value = value
  self._inUse = value
end
pro C_sSkelNode::setOwnIndex, value = value
  self._ownIndex = value
end
pro C_sSkelNode::setNumEdges, value = value
  self._numEdges = value
end
pro C_sSkelNode::setIndexEdges, value = value
  self._indexEdges = ptr_new(value)
end
pro C_sSkelNode::setEdgeLabels, value = value
  self._edgeLabels  = ptr_new(value)
  self._Dir = [0,0,0]

  ; number of distinct edgesLabels
  self._dim = 0
  vectorMNPresentes = [-1]
  for indexEdge = 0, n_elements((*(self._edgeLabels))[0,*])-1 do begin
    xMN = ((*(self._edgeLabels))[0,indexEdge] eq -1)? 2: (*(self._edgeLabels))[0,indexEdge] 
    yMN = ((*(self._edgeLabels))[1,indexEdge] eq -1)? 2: (*(self._edgeLabels))[1,indexEdge]
    zMN = ((*(self._edgeLabels))[2,indexEdge] eq -1)? 2: (*(self._edgeLabels))[2,indexEdge]        
    magicNumber = (xMN*(3^0)) + (yMN*(3^1))  + (zMN*(3^2)) 
    wMN = where(vectorMNPresentes eq magicNumber,/L64)
    if(wMN[0] eq -1) then begin
        self._Dir = self._Dir + [xMN,yMN,zMN]
        self._dim = self._dim + 1
        vectorMNPresentes = [vectorMNPresentes,magicNumber]
    endif
  endfor
end

pro C_sSkelNode::setDim, value = value
  self._dim = value
end
pro C_sSkelNode::setDir, value = value
  self._Dir = value
end
pro C_sSkelNode::setVertexPosition, value = value
  self._vertexNode = value
end
pro C_sSkelNode::setWCentroid, value = value
  self._wCentroid = value
end
pro C_sSkelNode::setNumPoints, value = value
  self._numPoints = value
end
pro C_sSkelNode::setIndexOriginalPoints, value = value
  self._indexOriginalPoints  = ptr_new(value)
end

pro C_sSkelNode::CopyParams, otherNode = otherNode
  self._numEdges = otherNode->getNumEdges()
  
  ptr_free, self._indexEdges 
  ptr_free, self._edgeLabels   
  self._indexEdges = ptr_new(otherNode->getIndexEdges())
  self._edgeLabels = ptr_new(otherNode->getEdgeLabels())
    
  self._dim        = otherNode->getDim()
  self._Dir        = otherNode->getDir()
  self._vertexNode = otherNode->getVertexPosition()
  self._wCentroid = otherNode->getWCentroid()
  self._numPoints = otherNode->getNumPoints()

  ptr_free, self._indexOriginalPoints
  self._indexOriginalPoints = ptr_new(otherNode->getIndexOriginalPoints())
end

pro C_sSkelNode::CopyEndParams, otherNode = otherNode
  self._numEdges = otherNode->getNumEdges()
  
  ptr_free, self._indexEdges 
  self._indexEdges = ptr_new(otherNode->getIndexEdges())
    
  self._dim        = otherNode->getDim()
  self._Dir        = otherNode->getDir()
  self._vertexNode = otherNode->getVertexPosition()
  self._wCentroid = otherNode->getWCentroid()
  self._numPoints = otherNode->getNumPoints()

  ptr_free, self._indexOriginalPoints
  self._indexOriginalPoints = ptr_new(otherNode->getIndexOriginalPoints())
end

pro C_sSkelNode::FixEdges, crossEdgeReference = crossEdgeReference
  newEdges = [-1]
  initStep = 1
  for i = 0, self._numEdges-1 do begin
    actualEdge = crossEdgeReference[(*self._indexEdges)[i]]
    wEdgeIndex = where( newEdges eq actualEdge, /L64)
    if(wEdgeIndex[0] eq -1) then begin
        if (initStep eq 1) then begin
          newEdges = [actualEdge]
          initStep = 0
        endif else newEdges = [newEdges, actualEdge] 
    endif
  endfor

  self._numEdges = N_ELEMENTS(newEdges) 
  
  ptr_free, self._indexEdges 
  self._indexEdges = ptr_new(newEdges, /NO_COPY)
end
pro C_sSkelNode::DisableNode
  self._inUse = 0
end

function C_sSkelNode::init
  self._indexEdges = ptr_new([-1])
  self._edgeLabels = ptr_new([-1])
  self._indexOriginalPoints = ptr_new([-1])
  return, 1
end

pro C_sSkelNode__define
   interClass = { C_sSkelNode, $ 
                  _inUse             : 0b, $         ; 
                  _ownIndex          :-1d, $          ;
                  
                  _numEdges     :-1d, $          ;
                  _indexEdges  :  ptr_new(), $          ;
                  _edgeLabels       :  ptr_new(), $          ;

                  _dim              : 0.0d, $          ;
                  _Dir              : dblarr(3), $  ; 

                  _vertexNode           : dblarr(3), $  ;
                  _wCentroid            : 1.0d, $          ;
                  _numPoints            : 0.0d, $          ;
                  _indexOriginalPoints  : ptr_new() $          ;
                }
end