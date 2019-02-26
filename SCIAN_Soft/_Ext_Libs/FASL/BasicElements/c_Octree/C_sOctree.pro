;_____________________________IOISIOI____________________
; NAME:
;      C_sOctree
;
; PURPOSE:
;       - Octree-Basic-Class.
;
; AUTHOR:
;     FASL 2011
;     e_mail: We need a Scian mail :_D
;     fsantibanez@cefop.udec.cl
; CALLING SEQUENCE:
;       result = obj_new('C_sOctree' )
;
; METHOHDS:
;_____________________________IOISIOI____________________

function C_sOctree::getChild, ownIndexChild = ownIndexChild
  return, self.children[ownIndexChild]
end

function C_sOctree::getOffSpring
  return, self.offSpring
end

function C_sOctree::getIndexPoints
  return, *(self.indexPoints)
end

function C_sOctree::getNumPoints
  return, self.numPoints
end

function C_sOctree::getLevelDeep
  return, self.levelDeep
end

function C_sOctree::getMaxSemiAxisXYZ
  return, self.maxSemiAxisXYZ
end

function C_sOctree::getSemiAxisXYZ
  return, self.semiAxisXYZ
end

function C_sOctree::getCenterNode
  return, self.centerXYZ
end

function C_sOctree::getMinXYZ
  return, self.minXYZ
end

function C_sOctree::getMaxXYZ
  return, self.maxXYZ
end

function C_sOctree::getMassCenterXYZ
  return, self.massCenterXYZ
end

pro C_sOctree::setActiveRoot
  self.root = 1b
end

pro C_sOctree::setLevelDeep, levelDeep = levelDeep
  self.levelDeep = levelDeep
end

pro C_sOctree::setChild, ownChild = ownChild, ownIndexChild = ownIndexChild
  self.children[ownIndexChild] = ownChild
end
  
pro C_sOctree::setParent, ownParent = ownParent
  self.parent = ownParent 
end

pro C_sOctree::setIsOffSpring, ownOffSpring = ownOffSpring
  ; Determine if subdivision is required
  self.offSpring = ownOffSpring
end

pro C_sOctree::setDimensions, xyzPoints = xyzPoints, indexPoints = indexPoints, minXYZ = minXYZ, maxXYZ = maxXYZ
  if (indexPoints[0] ne -1) then begin
      ; Obtain boundaries for Principal Octree
      ;self.minXYZ[0] = min(xyzPoints[0,indexPoints], max = dummy)
      ;self.maxXYZ[0] = dummy
      ;self.minXYZ[1] = min(xyzPoints[1,indexPoints], max = dummy)
      ;self.maxXYZ[1] = dummy
      ;self.minXYZ[2] = min(xyzPoints[2,indexPoints], max = dummy)
      ;self.maxXYZ[2] = dummy
      self.minXYZ     = minXYZ ; balanced version
      self.maxXYZ     = maxXYZ ; balanced version    
      
      self.semiAxisXYZ = 0.5d*abs(self.maxXYZ - self.minXYZ)
      self.maxSemiAxisXYZ = max(self.semiAxisXYZ)

      self.numPoints = n_elements(indexPoints)
    
      self.centerXYZ = 0.5d*(self.minXYZ + self.maxXYZ)
      
      self.massCenterXYZ[0] = TOTAL(xyzPoints[0,indexPoints])
      self.massCenterXYZ[1] = TOTAL(xyzPoints[1,indexPoints])
      self.massCenterXYZ[2] = TOTAL(xyzPoints[2,indexPoints])
      self.massCenterXYZ    = self.massCenterXYZ / self.numPoints
            
  endif else begin
      self.minXYZ[0] = -1.0d
      self.maxXYZ[0] = -1.0d
      self.minXYZ[1] = -1.0d
      self.maxXYZ[1] = -1.0d
      self.minXYZ[2] = -1.0d
      self.maxXYZ[2] = -1.0d
    
      self.semiAxisXYZ = 0.5d*abs(self.maxXYZ - self.minXYZ)
      self.maxSemiAxisXYZ = max(self.semiAxisXYZ)


      self.numPoints = 0.0d
      
      self.centerXYZ = 0.5d*(self.minXYZ + self.maxXYZ)
      
      self.massCenterXYZ = self.centerXYZ
  endelse
  self.indexPoints = ptr_new(indexPoints)
end

function C_sOctree::init
    ; Parameters of C_sOctree.
    self.children[0] = -1.0d
    self.children[1] = -1.0d
    self.children[2] = -1.0d
    self.children[3] = -1.0d
    self.children[4] = -1.0d
    self.children[5] = -1.0d
    self.children[6] = -1.0d
    self.children[7] = -1.0d
    
    self.indexPoints = ptr_new([-1], /no_copy)
    return, 1
end

pro C_sOctree__define
   interClass = { C_sOctree, $ 
                  root        : 0b, $         ; 
                  offSpring   : 0b, $         ;
                  levelDeep   : 0b, $         ;
                  
                  parent      : -1.0d, $         ;
                  children    : dblarr(8), $         ;  
                  
                  numPoints   : 0.0d, $          ;
                  indexPoints : ptr_new(), $          ;
                  
                  minXYZ           : dblarr(3), $  ;
                  maxXYZ           : dblarr(3), $  ;
                  centerXYZ        : dblarr(3), $  ;
                  SemiAxisXYZ      : dblarr(3), $  ;
                  maxSemiAxisXYZ   : 0.0d, $  ;
                  massCenterXYZ    : dblarr(3)  $  ;
                }
end