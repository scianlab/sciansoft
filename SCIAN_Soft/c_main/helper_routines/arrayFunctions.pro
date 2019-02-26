function makeSignedVertexLists, codes

  nVertices = n_elements(codes)
  if (nVertices lt 1) then return, ptr_new()

  pVertexLists = ptrArr(1)
  nLists   = 0
  fNewList = 0b
  newList  = [-1]

  fMergeEnds = (codes[0] ne 0) and ((codes[nVertices-1]) ne 0)
  for i = 0L, nVertices-1 do begin
    if (codes[i] eq 0) then begin
      if (fNewList eq 0) then continue else begin
        pVertexLists[nLists] = ptr_new(newList, /NO_COPY)
        pVertexLists = [pVertexLists, ptr_new()]
        nLists += 1
        fNewList = 0b
      endelse
    endif else begin
      newElement = codes[i] eq 1 ? i : -i
      if (fNewList eq 0) then begin
        fNewList = 1b
        newList = [newElement]
      endif else newList = [newList, newElement]
    endelse
c:  continue
  endfor
  if fNewList then begin
    pVertexLists[nLists] = ptr_new(newList, /NO_COPY)
    pVertexLists = [pVertexLists, ptr_new()]
    nLists += 1
    fNewList = 0b
  endif
  if (fMergeEnds and (nLists gt 1)) then begin
    listAtStart = *(pVertexLists[0])
    listAtEnd   = *(pVertexLists[nLists-1])
    ;print, 'listAtStart ', listAtStart
    ;print, 'listAtEnd ', listAtEnd
    ptr_free, pVertexLists[0], pVertexLists[nLists-1]
    pVertexLists[0] = ptr_new([listAtEnd, listAtStart])
    nLists -= 1
  endif

  return, (nLists ge 1) ? pVertexLists[0:nLists-1] : ptr_new()
end


pro makeSignedVertexLists_testSingle, codes
  out  = makeSignedVertexLists(codes)
  nOut = n_elements(out)
  if (nOut eq 1) then if ~ptr_valid(out[0]) then nOut = 0
  print, 'number of output lists: ', nOut
  if (nOut gt 0) and (ptr_valid(out[0])) then for i = 0, nOut-1 do print, 'list of codes ', i, ': ', *(out[i])
end


pro makeSignedVertexLists_test
  codes = [0, 0, 1, 1, 1, 2, 2, 2, 1, 1, 1, 0, 0]
  makeSignedVertexLists_testSingle, codes

  codes = [0, 0, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1]
  makeSignedVertexLists_testSingle, codes

  codes = [1, 0, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1]
  makeSignedVertexLists_testSingle, codes

  codes = [1, 2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1]
  makeSignedVertexLists_testSingle, codes

  codes = [1, 2, 1, 1, 1, 0, 0, 0, 1, 1, 1, 1, 1]
  makeSignedVertexLists_testSingle, codes

  codes = [1, 0, 0, 2, 1, 0, 0, 0, 1, 1, 1, 1, 1]
  makeSignedVertexLists_testSingle, codes

  makeSignedVertexLists_testSingle, replicate(0, 10)
end


; singleRunToIndices
;
; Given a pair of numbers as start/end of a sequence of n elements,
; returns the correlative sequence of number in between (including the given start/end).
;
; runStart/runEnd  Specify the run endpoints.
; nElem            Specify the number of elements for which the endpoint indices are converted (using the mod operation).
function singleRunToIndices, runStart, runEnd, nElem

  if (nElem lt 1) then return, [-1]
  if (runStart ge nElem)  then runStart = runStart mod nElem
  if (runEnd   ge nElem)  then runEnd   = runEnd mod nElem
  if (runStart eq runEnd) then return, [runStart]

  ; Calculate the run size. Cast indices to Long to avoid MaxInt quirkiness...
  nElemRun  = long(runEnd) - long(runStart) + 1 + ((runEnd gt runStart) ? 0 : nElem)
  indexType = size(runStart, /TYPE)
  indexList = indexType eq 3 ? lIndGen(nElemRun) : indGen(nElemRun)

  if (runStart gt runEnd) then $
    indexList[nElem - runStart : *] -= nElem

  return, indexList + runStart
end


pro singleRunToIndices_Test
  print, singleRunToIndices(22, 1, 27)
  print, singleRunToIndices(1, 22, 27)
  print, singleRunToIndices(1, 0, 27)
  print, singleRunToIndices(1, 1, 27)
end


function runsToIndices, runList, nElem
  if (nElem lt 1) then return, [-1]
  nRuns = n_elements(runList)/2
  outIndices = [-1]
  for i = 0u, nRuns-1 do begin
    runPos = 2*i
    outIndices = [outIndices, singleRunToIndices(runList[runPos], runList[runPos+1], nElem)]
  endfor
  return, outIndices[1:*]
end


pro runsToIndices_Test
  runs = [22, 1, 1, 22, 1, 0, 1, 1]
  print, runsToIndices(runs, 27)
end


; findIndexInRunEndsList
;
; Given a run in the form [head0, tail0, head1, tail1, ... head_n-1, tail_n-1], and a lookup index,
; returns the corresponding run head for this index, and optionally the corresponding run-index.
;
; Parameters
;  runList        The input run list.
;  index          The element index to look for in the run list.
;  startPos       Optional argument. To specify that the search must start from a run other than the first (NOT TESTED YET!).
;  fReverseSearch Indicates to return the last element of the corresponding
function findIndexInRunEndsList, runList, index, startPos = startPos, fReverseSearch = fReverseSearch, runNum = runNum
  pos      = -1
  nRuns    = n_elements(runList)/2
  startPos = floor((n_elements(startPos) eq 1 ? (startPos > 0) < (nRuns-1) : 0) /2)

  for i = startPos, nRuns-1 do begin
    if ((index ge runList[2*i]) and (index le runList[2*i+1])) then begin
      if arg_present(runNum) then runNum = i
      return, keyword_set(fReverseSearch)? runList[2*i+1] : runList[2*i]
    endif
  endfor
  if arg_present(runNum) then runNum = -1
  return, pos
end


pro findIndexInRunEndsListTest
  runTest1 = [10, 12, 13, 16, 17, 114]
  runTest2 = [11, 14, 15, 18]
  runIndexToLookFor1 = 14
  runIndexToLookFor2 = 13
  pos1 = findIndexInRunEndsList(runTest1, runIndexToLookFor1, runNum = runNum)
  print, pos1, runNum ;IDL prints 13 1
  pos1 = findIndexInRunEndsList(runTest2, runIndexToLookFor1, runNum = runNum)
  print, pos1, runNum ;IDL prints 11 0
  pos2 = findIndexInRunEndsList(runTest1, runIndexToLookFor2, /FREVERSESEARCH, runNum = runNum) 
  print, pos2, runNum ;IDL prints 16 1 
  pos2 = findIndexInRunEndsList(runTest2, runIndexToLookFor2, /FREVERSESEARCH, runNum = runNum) 
  print, pos2, runNum ;IDL prints 14 0
end


; 
; findRunEndsInArray
;
;   Given an 1D-array and a value, find a "run" of this value.
;   Here, a run is a sequence of consecutive occurrences of the given value.
;   For example, in [0, 1, 1, 1, 1, 0, 3, 4] there is a run of four 1's, two runs of single 0's, etc.
;   This function is intended to look for the occurrence of a single run.
;
; Parameters:
;   array      The input array for which the run will be searched.
;   runValue   The element value of the run.  It is assumed that the input array
;              contains only one run of this value.
;   startPos   Optional input argument speciifying a specific starting position for the search.
;   runLength  Optional output argument. Set this to a named variable to get the run length (number of elements of the run).
;
; History
;   First version: 07. 2012. JJW.
function findRunEndsInArray, array, runValue, startPos = startPos, runLength = runLength

  nElem = n_elements(array)

  if (nElem eq 1) then if array[0] eq runValue then begin
    if arg_present(runLength) then runLength = 1
    return, [0, 0]
  endif else begin
    if arg_present(runLength) then runLength = 0
    return, -1
  endelse

  startPos    = n_elements(startPos) eq 1 ? (startPos > 0) < (nElem-1) : 0
  pos         = startPos
  runStartPos = -1
  runEndPos   = -1

  ; first case (started at middle of the sequence?): look to the left and to the right
  if (array[startPos] eq runValue) then begin

    countLeft = startPos
    stopPos = (startPos eq nElem-1) ? 0 : startPos+1

    repeat begin
      pLeft      = countLeft eq 0 ? nElem-1 : countLeft-1
      fLeftReady = (array[pLeft] ne runValue) or (pLeft eq startPos)
      if ~fLeftReady then countLeft = countLeft eq 0 ? nElem-1 : countLeft-1
    endrep until fLeftReady eq 1

    countRight = startPos + 1
    repeat begin
      fRightReady = countRight ge nElem
      if ~fRightReady then if (array[countRight] ne runValue) then fRightReady = 1 else countRight += 1
    endrep until fRightReady eq 1
    runStartPos = countLeft
    runEndPos   = countRight-1

    if arg_present(runLength) then $
      runLength = runStartPos gt runEndPos ? runEndPos + (nElem - runStartPos + 1) $
                                           : runEndPos - runStartPos + 1
    return, [runStartPos, runEndPos]

  ; second case (sequence not yet found): go to the right
  endif else while (array[pos] ne runValue) do begin 

    pos   += 1
    pos mod= nElem

    if (pos eq startPos) then begin
      if arg_present(runLength) then runLength = 0
      return, -1
    endif

  endwhile

  fLeftReady  = 1
  runStartPos = pos
  countRight  = (pos + 1) mod nElem

  repeat begin
    fRightReady = countRight eq startPos
    if (array[countRight] ne runValue) $
    then fRightReady = 1 $
    else countRight = (countRight + 1) mod nElem
  endrep until fRightReady eq 1

  countRight <= nElem-1
  runEndPos   = countRight eq 0 ? nElem-1 : countRight-1

  if arg_present(runLength) then $
    runLength = runStartPos gt runEndPos ? runEndPos + (nElem - runStartPos + 1) $
                                         : runEndPos - runStartPos + 1

  return, (runStartPos gt -1) and (runEndPos gt -1) ? [runStartPos, runEndPos] : -1

end


; By now only tested with default search starting position (0)
pro findRunEndsInArrayTest
  l1 = [0, 1, 1, 1, 1, 1, 0, 0] ; OK
  l1gt = [1, 5]
  r1 = findRunEndsInArray(l1, 1, RUNLENGTH=rl1)
  print, r1, ' - run length: ', rl1
  ok1 = max((r1 - l1gt) eq 0)
  print, 'OK for test 1? ', ok1 ? 'Yes' : 'No'

  l2 = [1, 1, 1, 0, 0, 0, 1, 1] ; OK
  l2gt = [6, 2]
  r2 = findRunEndsInArray(l2, 1, RUNLENGTH=rl2)
  print, r2, ' - run length: ', rl2
  ok2 = max((r2 - l2gt) eq 0)
  print, 'OK for test 2? ', ok2 ? 'Yes' : 'No'

  l3 = [0, 0, 0, 1, 1, 1, 1, 1] ; OK
  l3gt = [3, 7]
  r3 = findRunEndsInArray(l3, 1, RUNLENGTH=rl3)
  print, r3, ' - run length: ', rl3
  ok3 = max((r3 - l3gt) eq 0)
  print, 'OK for test 3? ', ok3 ? 'Yes' : 'No'

  l4 = [1, 1, 1, 1, 1, 0, 0, 0] ; OK
  l4gt = [0, 4]
  r4 = findRunEndsInArray(l4, 1, RUNLENGTH=rl4)
  print, r4, ' - run length: ', rl4
  ok4 = max((r4 - l4gt) eq 0)
  print, 'OK for test 4? ', ok4 ? 'Yes' : 'No'

  l5 = [1, 1, 1, 1, 0, 0, 0, 1] ; OK
  l5gt = [7, 3]
  r5 = findRunEndsInArray(l5, 1, RUNLENGTH=rl5)
  print, r5, ' - run length: ', rl5
  ok5 = max((r5 - l5gt) eq 0)
  print, 'OK for test 5? ', ok5 ? 'Yes' : 'No'

  l6 = [1, 0, 0, 0, 1, 1, 1, 1] ; OK
  l6gt = [4, 0]
  r6 = findRunEndsInArray(l6, 1, RUNLENGTH=rl6)
  print, r6, ' - run length: ', rl6
  ok6 = max((r6 - l6gt) eq 0)
  print, 'OK for test 6? ', ok6 ? 'Yes' : 'No'

  l7 = [1, 0, 0, 0, 0, 0, 0, 0] ; OK
  l7gt = [0, 0]
  r7 = findRunEndsInArray(l7, 1, RUNLENGTH=rl7)
  print, r7, ' - run length: ', rl7
  ok7 = max((r7 - l7gt) eq 0)
  print, 'OK for test 7? ', ok7 ? 'Yes' : 'No'

  l8 = [0, 0, 0, 0, 0, 0, 0, 1] ; OK
  l8gt = [7, 7]
  r8 = findRunEndsInArray(l8, 1, RUNLENGTH=rl8)
  print, r8, ' - run length: ', rl8
  ok8 = max((r8 - l8gt) eq 0)
  print, 'OK for test 8? ', ok8 ? 'Yes' : 'No'

  l9 = [0, 0, 0, 0, 1, 0, 0, 0] ; OK
  l9gt = [4, 4]
  r9 = findRunEndsInArray(l9, 1, RUNLENGTH=rl9)
  print, r9, ' - run length: ', rl9
  ok9 = max((r9 - l9gt) eq 0)
  print, 'OK for test 9? ', ok9 ? 'Yes' : 'No'
  print, 'All tests OK? ', (ok1 + ok2 + ok3 + ok4 + ok5 + ok6 + ok7 + ok8 + ok9) eq 9 ? 'Yes' : 'No'
end


; makeRunList
;
; Makes run lists for 1D arrays.
;
; Keywords:
;   startPos       If set, specifies that the run must be constructed by skipping the list elements 
;                  in positions before this.
;   fCircularList  If set, indicates that the list must be treated as a circular one (i.e. the last
;                  element is followed by the first), in order to check for a run that includes the
;                  list extremes. Care should be taken by the caller method in order to process the
;                  ouutput properly.
; Example:
;   list = [0, 0, 0, 1, 2, 2, 2, 2, 2, 5, 1, 8]
;   runIndexList = [0, 3, 4, 9, 10, 11]
;   runValueList = [0, 1, 2, 5, 1, 8]
;
; Complexity
;   Let n the number of elements of the input list...
;   - Operations: O(n).
;   - Memory    : O(n) + O(number of runs in the list) + O(number of different values in the list) = O(n).
;
; History
;   First version, JJW (2013). 
;
function makeRunList, list, startingPos = startingPos, fCircularList = fCircularList

  itemCount = n_elements(list)
  if (itemCount eq 0) then return, {runIndexList: -1, runValueList: -1}

  pos  = n_elements(startingPos) gt 0 ? startingPos : 0
  pos *= 1L
  if (pos ge itemCount) then return, -1

  runIndexList = [pos]
  runValueList = list[pos]
  lastVal      = runValueList[pos]

  for i = pos+1, itemCount-1 do begin
    if (list[i] ne lastVal) then begin
      runIndexList = [runIndexList, i]
      lastVal      = list[i]
      runValueList = [runValueList, lastVal]
    endif
  endfor

  nRuns = n_elements(runValueList)
  if keyword_set(fCircularList) and (nRuns gt 1) then begin
    if (runValueList[0] eq runValueList[nRuns-1]) then begin
      runIndexListCutPos = n_elements(runIndexList)-1
      runIndexList = [runIndexList[runIndexListCutPos], runIndexList[1: runIndexListCutPos-1]]
      runValueList = runValueList[0: n_elements(runValueList)-2]
    endif
  endif

  return, {indexList: runIndexList,$
           valueList: runValueList}
end


function extractValueRunsFromArray, array, value

  if (n_elements(array) eq 0) then return, [-1]

  whValue = where(array eq value, whCount)
  if (whCount eq 0) then return, [-1]

  return, makeRunFromCorrelativeValues(whValue)

end

; run1/run2 must have the form [start_value, end_value] with start_value le end_value
function compareRunsByExtremes, run1, run2
  if run1[0] lt run2[0] then return, 1
  if run2[0] lt run1[0] then return, -1
  if run1[1] gt run2[1] then return, -1
  if run2[1] gt run1[1] then return, 1
  if run1[0] gt run2[0] then return, -1
  if run2[0] gt run1[0] then return, 1
  return, 0
end


pro sortRunList_insertionSort, runList, circularListMaxIndex = circularListMaxIndex, fRemoveDuplicatesAndContainedRuns = fRemoveDuplicatesAndContainedRuns

  nRuns = n_elements(runList) / 2
  if (nRuns le 1) then return

  for i = 1u, nRuns-1 do begin
    runPosI = 2*i
    x = runList[runPosI : runPosI+1]
    j = i - 1
    while ((j ge 0) and (compareRunsByExtremes(x, runList[2*j:2*j+1])) gt 0) do begin
      runPosJ = 2*j
      runList[runPosJ+2:runPosJ+3] = runList[runPosJ:runPosJ+1]
      j -= 1
      if (j eq -1) then break ; Looks stupidly redundant, but IDL seems to be ignoring the WHILE constraint today... ARG!
    endwhile
    runPosJ = 2*j
    runList[RunPosJ+2:runPosJ+3] = x
  endfor
  if keyword_set(fRemoveDuplicatesAndContainedRuns) then runList = removeDuplicatesAndContainedRunsFromSortedRunlist(runList, nMax = circularListMaxIndex)
end


; isRunContainedInRun
;
; run1/run2 must have the form [start_value, end_value] with start_value le end_value
;
; Return value:
; 0 if neither run is entirely contained within the other.
; 1 if the first run is entirely contained within the second.
; 2 if the second run is entirely contained within the first.
function isRunContainedInRun, run1, run2
  if (run1[0] le run2[0]) and (run1[1] ge run2[1]) then return, 2
  if (run2[0] le run1[0]) and (run2[1] ge run1[1]) then return, 1
  return, 0
end


function isRunContainedInRun, run1, run2, nMax = nMax
  fCircular1 = run1[1] le run1[0]
  fCircular2 = run2[1] le run2[0]
  fIs1in2 = 0
  case 1 of
     fCircular1 and ~fCircular2: fIs1in2 = 0
    ~fCircular1 and  fCircular2: fIs1in2 = (run1[1] le run2[1])
     fCircular1 and  fCircular2: fIs1in2 = (run1[1] le run2[1]) and (run1[0] ge run2[0])
    else: fIs1in2 = (run1[0] ge run2[0]) and (run1[1] le run2[1])
  endcase
  return, fIs1in2
end


function removeDuplicatesAndContainedRunsFromSortedRunlist, runList, nMax = nMax

  nRuns = n_elements(runList)/2
  if (nRuns le 1) then return, runList
  runListOut = runList[0:1]

  fAllRunsChecked = 0
  runNum = 1

  while ~fAllRunsChecked do begin

    runsOutLastPos = n_elements(runListOut)-1
    runCurrent     = runListOut[runsOutLastPos-1:runsOutLastPos]

    runPos     = 2 * runNum
    runToCheck = runList[runPos:runPos+1]

    ; Check if the runs can be circular in order to detect overlaps.
    fCircular1 = runCurrent[1] le runCurrent[0]
    fCircular2 = runToCheck[1] le runToCheck[0]
     ; This should not happen. Call the function with the proper argument to ensure that circular runs are allowed.
    if (n_elements(nMax) eq 0) and (fCircular1 or fCircular2) then stop
    if (fCircular1 or fCircular2) then begin
      fRun1In2 = isRunContainedInRun(runCurrent, runToCheck, nMax = nMax)
      fRun2In1 = isRunContainedInRun(runToCheck, runCurrent, nMax = nMax)
    endif else begin ; If the runs are not circular, the basic criteria are checked.
      fRun1In2 = (runToCheck[0] le runCurrent[0]) and (runToCheck[1] ge runCurrent[1])
      fRun2In1 = (runCurrent[0] le runToCheck[0]) and (runCurrent[1] ge runToCheck[1])
    endelse

    case 1 of
    fRun1In2: begin
      ;runListOut = (n_elements(runListOut) eq 2) ? runToCheck : [runListOut[0:2*(lastRunNum-1)-1], runToCheck]
      runListOut = (n_elements(runListOut) eq 2) ? runToCheck : [runListOut[0:n_elements(runListOut)-3], runToCheck]
      endcase
    fRun2In1: begin
      endcase
    else: begin
      case 1 of
        fCircular1 and ~fCircular2: begin
          fMerge12 = (runCurrent[1] ge runToCheck[0])
          if fMerge12 then runMerged = [runCurrent[0], runToCheck[1]]
          fMerge21 = runCurrent[1] ge runToCheck[0]
          if fMerge21 then runMerged = [runToCheck[0], runCurrent[1]]
        endcase
        ~fCircular1 and fCircular2: begin
          fMerge12 = (runCurrent[1] ge runToCheck[0])
          if fMerge12 then runMerged = [runCurrent[0], runToCheck[1]]
          fMerge21 = (runToCheck[1] ge runCurrent[0]) 
          if fMerge21 then runMerged = [runToCheck[0], runCurrent[1]]
        endcase
        fCircular1 and fCircular2: begin
         stop; This cannot happen as per the condition of the outer Case block, because it means that one run is contained in the other.
        endcase
        else: begin 
          fMerge12 = (runCurrent[0] le runToCheck[0]) and (runToCheck[0] le runCurrent[1])
          if fMerge12 then runMerged = [runCurrent[0], runToCheck[1]]
          fMerge21 = (runToCheck[0] le runCurrent[0]) and (runCurrent[0] le runToCheck[1])
          if fMerge21 then runMerged = [runToCheck[0], runCurrent[1]]
        endcase
      endcase
      if (fMerge12 or fMerge21) then begin
        ;runListOut = (n_elements(runListOut) eq 2) ? runMerged : [runListOut[0:2*(lastRunNum-1) - 1], runMerged]
        runListOut = (n_elements(runListOut) eq 2) ? runMerged : [runListOut[0:n_elements(runListOut)-3], runMerged]
      endif else begin
        runListOut = [runListOut, runToCheck]
      endelse
      endcase
    endcase

    runNum += 1
    if (runNum eq nRuns) then fAllRunsChecked = 1

  endwhile

  return, runListOut
end


function distanceBetweenIndicesInArray, ia, ib, maxIndexCircularList = maxIndexCircularList
  return, keyword_set(maxIndexCircularList) $
    ? ib + maxIndexCircularList - ia $
    : ib - ia
end


; TODO juanedo complete comments, indicating i) what this function do and ii) an example
; makeRunFromCorrelativeValues
;
; Input arguments:
;   orderedList        The input list of values from which the run list is computed.
;   nMax               If set, specifies the maximum index that can appear in the input list, which also means that the list is circular, i.e. values can "pass" trough 0.
;   getnMaxFromZeroPos If set, indicates the function to look for a 0-crossing of the list 
;                      values and assume that the preceding value is the maximum index value.
;   outnMax            If getnMaxFromZeroPos is set, this variable stores the found maximum index value.
;                      NOTE: if the 0-crossing is not found, no value is returned (it's easy to change this, though ;).
;   delta              Specifies an optional "jump" to omit, instead of strictly consecutive elements (which means delta = 1).
function makeRunFromCorrelativeValues, orderedlist, nMax = nMax, getnMaxFromZeroPos = getnMaxFromZeroPos, outnMax = outnMax, delta = delta

  itemCount = n_elements(orderedlist)
  if (itemCount eq 0) then return, [-1] ; Empty input list
  if (itemCount eq 1) then return, [orderedlist,orderedlist]; 1-element list
  delta = (n_elements(delta) gt 0) ? delta > 1 : 1
  run = [orderedlist[0]]
  for i = 1, itemCount-1 do begin
    if keyword_set(getnMaxFromZeroPos) then $
      if (orderedlist[i] eq 0) then begin 
        outnMax = orderedlist[i-1]
        continue
      endif
    if keyword_set(nMax) $
    then if (orderedlist[i] eq 0) and (orderedlist[i-1] ge (nMax + 1 - delta)) then continue
    if ((orderedlist[i]-orderedlist[i-1]) gt delta) $
    then run = [run, orderedlist[i-1], orderedlist[i]]
  endfor

  run = [run, orderedlist[itemCount-1]]
  ; if nMax is set, check if there is a run that goes from the tail to the head of the output run list
  nElemRun = n_elements(run)
  if keyword_set(nMax) and (nElemRun gt 2) then begin
    distExtremes = distanceBetweenIndicesInArray(run[nElemRun-1], run[0], maxIndexCircularList = nMax)
    diffStartEnd = run[0] - run[nElemRun-1]
    if (distExtremes le delta) or ((diffStartEnd ge 0) and (diffStartEnd le delta)) then begin
    ;if (run[0] eq 0) and (run[nElemRun-1] ge (nMax + 1 - delta)) then begin
      run[0] = run[nElemRun-2]
      run = run[0:nElemRun-3]
    endif
  endif

  return, run
end


pro makeRunList2test
  list1 = [0]
  list2 = [0,1]
  list3 = [0,3]
  list4 = [0,1,3]
  list5 = [1,2,3,5,9,10]
  print, makeRunFromCorrelativeValues(list1)
  print, makeRunFromCorrelativeValues(list2)
  print, makeRunFromCorrelativeValues(list3)
  print, makeRunFromCorrelativeValues(list4)
  print, makeRunFromCorrelativeValues(list5)
end    


; makeRunListWithExclusion
;
; Makes run lists for 1D arrays, excluding a single value.
; Return value:
;   The output run index list contains pairs of [startIndex, endIndex] for each run.
;   The output run value list contains the run values.
;
; startPos  If set, specifies that the run must be constructed by skipping the list elements in positions before this.
;
; Example:
;   list = [0, 0, 0, 1, 2, 2, 2, 2, 2, 5, 1, 8]
;   runIndexList = [3,3, 4,8, 9,9, 10,10, 11,11]
;   runValueList = [1, 2, 5, 1, 8]
;
function makeRunListWithExclusion, list, excludeValue, startingPos = startingPos

  itemCount = n_elements(list)
  if (itemCount eq 0) then return, {runIndexList: -1, runValueList: -1}

  pos = n_elements(startingPos) gt 0 ? startingPos : 0
  pos *= 1L
  if (pos ge itemCount) then return, -1

  if list[pos] eq excludeValue then $
    while (pos lt itemCount) do begin
      if (list[pos] eq excludeValue) $
      then pos += 1 $
      else break
    endwhile
  if (pos ge itemCount) then return, -1

  runIndexList = [pos]
  runValueList = list[pos]
  lastVal      = runValueList[0]

  for i = pos+1, itemCount-1 do begin
    if (list[i] ne lastVal) then begin
      lastVal      = list[i]
      if (lastVal ne excludeValue) then begin
        if (list[i-1] ne excludeValue) $ 
        then runIndexList = [runIndexList, i-1, i] $
        else runIndexList = [runIndexList, i]
        runValueList = [runValueList, lastVal]
      endif else begin
        lastElemIndex = n_elements(runValueList)-1
        if (list[i-1] ne excludeValue) and (lastVal ne runValueList[lastElemIndex]) then runIndexList = [runIndexList, i-1]
      endelse
    endif
  endfor

  return, {indexList: runIndexList,$
           valueList: runValueList}
end


; makeRunListTest
;
; Test function for makeRunList
; Parameters:
;   inList  optional input list to test the function. A default list is created is no inpu list is given. 
;
; History
;   First version, JJW (2013). 
;
pro makeRunListTest, inList = inList
  list = n_elements(inList) gt 0 ? inList : [0, 0, 0, 1, 2, 2, 2, 2, 2, 5, 1, 8, 0, 0, 0, 1, 2, 2, 2, 0, 2, 1, 0, 0]
  runList = makeRunList(list)
  print, 'makeRunList'
  print, '', runList.indexList
  print, '', runList.valueList

  runList = makeRunListWithExclusion(list, 0)
  print, 'makeRunListWithExclusion'
  print, '', runList.indexList
  print, '', runList.valueList

end


; locatePair
; Given a 1D-list and a pair of values, searches for the position of the first element of the pair.
;
; Return value: the position of the pair's first element, for the first occurrence of the pair in the list.
;               -1 if the pair is not found.
;
; list                   The input list. 
; val1                   The first element of the pair.
; val2                   The second element of the pair.
; pos2ndItem             If set, specifies to return the position of the second element instead of the first.
; searchForInvertedPair  If set, specifies to search also if the pair is inverted (first occurrence, even if it is located before the non-inverted occurrence).
; isInverted             Optional output variable, use only if searchForInvertedPair is set, to get a flag set to 1 if the pair is first found inverted. 
;
; Examples
;   l = [0, 0, 0, 1, 2, 2, 2, 2, 2, 5, 1, 8]
;   print, locatePair(l, 1, 2)
;   3
;   print, locatePair(l, 1, 8, /pos2ndItem)
;   11
;   print, locatePair(l, 1, 9)
;   -1
;
; Complexity
;  - Operations: O(search(val1,val2)) for the "strict" pair
;                min(O(search(val1,val2)), O(search(val2,val1))) for "the non-strict" pair search
;                O(n) with the current implementation (and depending on IDL's "where" function).
;                The input array may not be sorted, but for the case of overlap-computation resulting polygon vertices, the 
;                list is usually a bitonic sequence (suitable for bitonic sorting/searching).
;  - Space     : O(n), including additional variables of the function.
;
; History
;   First version, JJW (2013). 
;
function locatePair, list, val1, val2, startingPos = startingPos, pos2ndItem = pos2ndItem, searchForInvertedPair = searchForInvertedPair, isInverted = isInverted

  itemCount = n_elements(list)
  if (itemCount eq 0) then return, -1

  startingPos  = n_elements(startingPos) gt 0 ? startingPos : 0
  startingPos *= 1L
  if (startingPos ge itemCount) then return, -1

  where1 = where(list[startingPos:*] eq val1, whereCount)
  if ~keyword_set(searchForInvertedPair) then begin
    if where1[n_elements(where1)-1] eq itemCount-1 then whereCount -= 1
    if (whereCount eq 0) then return, -1
  endif

  if keyword_set(searchForInvertedPair) then begin

    isInverted = 0b
    for i = 0L, whereCount-1 do begin
      posBefore = where1[i] - 1
      posNext   = where1[i] + 1
      if (posBefore ge 0) then $
        if (list[posBefore] eq val2) then begin
          isInverted = 1b
          return, keyword_set(pos2ndItem) ? posBefore : where1[i]
        endif
      if (posNext lt itemCount) then $
        if (list[posNext] eq val2) then return, keyword_set(pos2ndItem) ? posNext : where1[i]
    endfor

  endif else begin

    for i = 0L, whereCount-1 do $
      if (list[where1[i]+1] eq val2) then return, keyword_set(pos2ndItem) ? where1[i]+1 : where1[i]

  endelse

  return, -1

end


; testMatch
;   Given two elements, evaluate if their elements match by comparing them.
;
; Parameters
;   item1: the first item of the comparison (formed of scalar values only).
;   item2: the second item of the comparison (formed of scalar values only).
;
; Complexity
;  - Operations: O(size(item1)) = O(size(item2)). O(1) can be assumed.
;  - Memory    : O(size(item1)) + O(size(item2)) + O(1). O(1) can be assumed.
;
; History
;   First version, JJW (2013). 
;
function testMatch, item1, item2
  f = 0b
  n1 = n_elements(item1)
  n2 = n_elements(item2)

  if (n1 eq n2) then begin
    fEQ = 1b
    for i = 0L, n1-1 do $
      if (item1[i] ne item2[i]) then begin
        fEQ = 0b
        break
      endif
    f = fEQ
  endif

  return, f
end


; indices array must contain non-duplicated (i.e. unique) values.
function extractFromRuns, runs, indices, fVerbose = fVerbose

  nElemRuns = n_elements(runs)
  nRuns     = nElemRuns / 2.0
  if (nRuns lt 1) then return, [-1]

  nIndices  = n_elements(indices)
  if (nIndices lt 1) then return, [-1]

  curRunPos = 0
  curIndPos = 0
  cutRuns = [-1]

  maxInd = max(indices, min = minInd)
  if (maxInd lt runs[0]) or (minInd gt runs[nElemRuns-1]) then return, runs
  if (maxInd eq runs[0]) then begin
    if ((maxInd lt runs[1]) and (nElemRuns eq 2)) then return, [-1]
    if keyword_set(fVerbose) then print, 'max index is lower than min run-index, returning all the input runs'
    cutRuns = runs
    cutRuns[0] = cutRuns[0]+1
    return, cutRuns
  endif
  if (minInd eq runs[nElemRuns-1]) then begin
    if ((minInd gt runs[nElemRuns-2]) and (nElemRuns eq 2)) then return, [-1]
    if keyword_set(fVerbose) then print, 'min index is greater than min run-index, returning all the input runs'
    cutRuns = runs
    cutRuns[nElemRuns-1] = cutRuns[0]-1
    return, cutRuns
  endif

  tmpRuns = runs
  fAppendTail = 0b
  repeat begin

    fAppendTail = 0b
    if keyword_set(fVerbose) then print, 'beginrep...'
    runMin = tmpRuns[curRunPos]
    runMax = tmpRuns[curRunPos+1]

    case 1 of

      (indices[curIndPos] lt runMin): begin
        curIndPos += 1
      endcase

      (indices[curIndPos] gt runMax): begin
        cutRuns = [cutRuns, runMin, runMax]
        if (n_elements(tmpRuns[curRunPos:*]) gt 2) then begin
          tmpRuns   = tmpRuns[curRunPos+2:*]
          curRunPos = 0
        endif else curRunPos += 2
      endcase

      else: begin

        case 1 of

          (indices[curIndPos] eq runMin) and (indices[curIndPos] eq runMax): begin
            if (n_elements(tmpRuns[curRunPos:*]) gt 2) then begin
              tmpRuns   = tmpRuns[curRunPos+2:*]
              curRunPos = 0
            endif else curRunPos += 2
            curIndPos += 1
          endcase

          (indices[curIndPos] eq runMax): begin
            cutRuns = [cutRuns, runMin, runMax-1]
            if (n_elements(tmpRuns[curRunPos:*]) gt 2) then begin
              tmpRuns   = tmpRuns[curRunPos+2:*]
              curRunPos = 0
            endif else curRunPos += 2
            curIndPos += 1
          endcase

          (indices[curIndPos] eq runMin): begin
            tmpRuns[curRunPos] = runMin+1
            curIndPos += 1
          endcase

          else: begin
            cutRuns = [cutRuns, runMin, indices[curIndPos]-1]
            tmpRuns[curRunPos] = indices[curIndPos]+1
            curRunPos = 0
            curIndPos += 1
          endcase
          else: if keyword_set(fVerbose) then print, 'default case, not expected... ?'
        endcase
      endcase
    endcase

    if keyword_set(fVerbose) then begin
      print, 'endrep...'
      print, 'curIndPos = ', curIndPos, ' curRunPos = ', curRunPos, ' tmpRuns = ', tmpRuns
      print, 'cutRuns = ', cutRuns
    endif
  endrep until (curIndPos gt nIndices-1) or (curRunPos ge n_elements(tmpRuns))

  if (curRunPos lt n_elements(tmpRuns)) then begin
    if (n_elements(cutRuns) eq 1) then return, tmpRuns
    return, [cutRuns[1:*], tmpRuns]
  endif

  return, (n_elements(cutRuns) gt 1) ? cutRuns[1:*] : [-1]
end


; mcerda
; indices array must contain non-duplicated (i.e. unique) values.
; out put is the extended runs minus vertices, separed by -1, e.g.
; if runs are [0,2, 5,9], and vertices [2,3,4,5,6]
; output is
; [-1 0 1 -1 7 8 9 -1]
function extractFromRuns2, runs, indices, nVertices, fVerbose = fVerbose

  nElemRuns = n_elements(runs)
  nRuns     = nElemRuns / 2.0
  if (nRuns lt 1) then return, [-1]

  nIndices = n_elements(indices)
  if (nIndices lt 1) then return, [-1]

  ;build the expanded runs
  fullList = uIndGen(nVertices)
  fullRuns = [-1]

  for i = 0L, nRuns -1 do begin
    if (runs[2*i] le runs[2*i+1]) then begin
      expRun   = SetDifference(fullList[runs[2*i]:runs[2*i+1]], indices)
      fullRuns = [fullRuns, expRun, -1]
    endif else begin
      expRun   = SetDifference([fullList[runs[2*i]:nVertices-1], fullList[0:runs[2*i+1]]], indices)
      fullRuns = [fullRuns, expRun, -1]
    endelse
  endfor

  return, fullRuns
end


; setDifference
;
; mcerda (from IDL Coyote).
;
; removes B from A
; ...or, alternatively, computes A and (not B) = elements in A but not in B.
function setDifference, a, b  

  minA = min(a, max = maxA)
  minB = min(b, max = maxB)
  if (minB gt maxA) or (maxB lt minA) then return, a ; No intersection...

  r = where((histogram(a, min = minA, max = maxA) ne 0) and $
            (histogram(b, min = minA, max = maxA) eq 0), count)
  return, (count eq 0) ? -1 : r + minA
end


; mcerda
pro extractFromRuns_test2
  aa = extractFromRuns2([0,2,5,9], [2,3,4,5,6], 10, fVerbose = 1b)
  print, 'Testing basics...'
  print, aa

  print, 'Testing cycle...'
  aa = extractFromRuns2([0,2,5,1], [2,3,4,5,6], 10, fVerbose = 1b)
  print, aa
end


pro extractFromRuns_test
  runs = [10, 14, 20, 25]
  indices = [1, 2, 3, 5, 6, 17, 18]
  res = runs
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [10, 14, 20, 25]
  indices = [1, 2, 3, 5, 6, 17, 18, 22]
  res = [10, 14, 20, 21, 23, 25]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [10, 14, 20, 25]
  indices = [1, 2, 3, 5, 6, 17, 18, 25]
  res = [10, 14, 20, 24]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [1, 2, 10, 14, 20, 25]
  indices = [1, 2, 3, 5, 6, 17, 18, 25]
  res = [10, 14, 20, 24]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [1, 2, 10, 14, 18, 18, 20, 25]
  indices = [1, 2, 3, 5, 6, 17, 18, 25]
  res = [10, 14, 20, 24]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [1, 2]
  indices = [1]
  res = [2, 2]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [1, 1]
  indices = [1]
  res = [-1]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [10, 14, 20, 25]
  indices = [1, 2, 3, 5, 6, 20, 21, 22, 23, 24]
  res = [10, 14, 25, 25]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [10, 14, 20, 25]
  indices = [14]
  res = [10, 13, 20, 25]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [10, 14, 20, 25]
  indices = [10, 11, 12, 13, 14]
  res = [20, 25]
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
  runs = [10, 14, 20, 25]
  indices = [10, 14, 20, 23]
  res = [11, 13, 21, 22, 24, 25] 
  out = extractFromRuns(runs, indices);, /FVERBOSE)
  print, out
  print, 'error code: ', (n_elements(out) ne n_elements(res)) > max(out ne res)
end


; getComplementRuns
;
; Given a list of values and a list of value-rus, substract the latter from the list
; and return the resulting runs.
function getComplementRuns, list, runsToExtract, maxIndex = maxIndex, fCircularList = fCircularList

  nRunsToExtract = n_elements(runsToExtract)/2
  if (nRunsToExtract lt 1) then return, makeRunList(list, fCircularList = fCircularList)
  if ~arg_present(maxIndex) then maxIndex = max(runsToExtract) > max(list)
  differenceList = list
  for i = 0u, nRunsToExtract-1 do begin
    runPos = 2*i
    curListToExtract = singleRunToIndices(runsToExtract[runPos], runsToExtract[runPos+1], maxIndex+1)
    differenceList   = setDifference(differenceList, curListToExtract)
  endfor
  return, makeRunFromCorrelativeValues(differenceList)
end


; mergeRunListsFromCorrelativePos
;
; Requirement: both runLists are sorted in ascending order.
function mergeRunListsFromCorrelativePos, runList1, runList2
  nRuns1 = n_elements(runList1) / 2
  nRuns2 = n_elements(runList2) / 2
  if (nRuns1 lt 1) then return, replicate(1, nRuns2)
  if (nRuns2 lt 1) then return, replicate(0, nRuns1)
  runOrder = [-1]
  countRuns1 = 0
  countRuns2 = 0
  fOK = 0
  while ~fOK do begin
    posRun1 = 2 * countRuns1
    posRun2 = 2 * countRuns2
    whichRun = compareRunsByExtremes(runList1[posRun1:posRun1+1], runList2[posRun2:posRun2+1])
    case whichRun of
      -1: begin
          runOrder = [runOrder, 1]
          countRuns2 += 1
          endcase
       0: begin
          runOrder = [runOrder, 0, 1]
          countRuns1 += 1
          countRuns2 += 1
          endcase
       1: begin
          runOrder = [runOrder, 0]
          countRuns1 += 1
          endcase
      else: stop
    endcase
    if (countRuns1 eq nRuns1) and (countRuns2 lt nRuns2) then begin
      deltaR2    = nRuns2 - countRuns2
      runOrder   = [runOrder, replicate(1, deltaR2)]
      countRuns2 = nRuns2
    endif
    if (countRuns2 eq nRuns2) and (countRuns1 lt nRuns1) then begin
      deltaR1    = nRuns1 - countRuns1
      runOrder   = [runOrder, replicate(0, deltaR1)]
      countRuns1 = nRuns1
    endif
    fOK = (countRuns1 eq nRuns1) and (countRuns2 eq nRuns2)
  endwhile

  return, runOrder[1:*]
end


; intersectionXYZpoints
;
; Given two arrays of 2D or 3D points, returns the position of the first occurrence of a common point.
;
; Return value
;   +1 if the search is succesful
;   -1 if not
;
; Parameters
;   arr1/arr2  Input arrays of points, either 2D (2xn) or 3D (3xn).
;   pos1/pos2  Set these to named variables to get the position of the common point in each input array.
;
; Complexity
;   Let n1 and n2 the number of elements of the input arrays 1 and 2, respectively...
;   - Operations: O(n1 * n2) worst case with the current implementation (arg!)
;                 Could be improved with binary search for ordered arrays (point arrays in IDL ROI objects are sorted by x, y and z coordinates).
;   - Memory    : O(n1 + n2)
function intersectionXYZpoints, arr1, arr2, pos1 = pos1, pos2 = pos2

  nDim1 = size(arr1, /DIMENSIONS)
  nDim2 = size(arr2, /DIMENSIONS)

  if (nDim1[0] ne nDim2[0]) then begin
    print, 'Input array dimensions mismatch'
    return, -1
  endif else begin
    if (n_elements(nDim1) ne 2) and (n_elements(nDim1) ne 3) $ ; Check 2xN, 3xN ox 3x1 (point) elements
    and ~(((n_elements(nDim1) eq 1) and nDim1[0] eq 3) or ((n_elements(nDim1) eq 1) and nDim1[0] eq 2)) then begin
      print, 'Input arrays must have 2xN or 3xN elements'
      return, -1
    endif
  endelse

  sz1 = size(n1, /N_DIMENSIONS)
  sz2 = size(n2, /N_DIMENSIONS)
  n1 = n_elements(arr1[0,*])
  n2 = n_elements(arr2[0,*])

  ; (slow) Sequential search
  ; If the arrays are sorted (as in the case of getXYPpoints or getproperty from IDL roi classes)
  ; a binary search (faster) could be used instead.
  for i = 0L, n1-1 do begin
    for j = 0L, n2-1 do begin
      f = testMatch(arr1[*,i], arr2[*,j])
      if (f eq 1) then begin
        if arg_present(pos1) then pos1 = i
        if arg_present(pos2) then pos2 = j
        return, 1
      endif
    endfor
  endfor

  return, 0
end


function fixRun, runList, nElem, fVerbose = fVerbose
  indicesList1 = runsToIndices(runList, nElem)
  indicesList1 = indicesList1[sort(indicesList1)]
  indicesList1 = indicesList1[uniq(indicesList1)]
  runList = makeRunFromCorrelativeValues(indicesList1)
  nRuns = n_elements(runList) / 2
  if ((nRuns gt 1) and (runList[2*nRuns-1] eq (nElem-1)) and (runList[0] eq 0)) then $
    runList = [runList[2:2*nRuns-2], runList[1]]
  if keyword_set(fVerbose) then print, runList
  return, runList
end


function removeFromArray, array, elements
  nArray = n_elements(array)
  nToRemove = n_elements(elements)
  if ((nArray eq 0) or (nToRemove eq 0)) then return, [-1]
  prunedArray = [-1]
  for i = 0u, nArray-1 do begin
    whInElements = where(elements eq array[i], whCount)
    if (whCount eq 0) then prunedArray = [prunedArray, array[i]]
  endfor

  return, n_elements(prunedArray) gt 1 ? prunedArray[1:*] : prunedArray
end


pro removeFromArrayTest
  arr1 = indGen(14)
  elem1 = [2, 5, 7]
  print, removeFromArray(arr1, elem1)
end


function mergeFromSortedPairList, pairList
  nPairs = n_elements(pairList) / 2
  if (nPairs lt 1) then return, [-1]
  
  for i = 0u, nPairs-1 do begin
    print, 'TODO'
  endfor
  return, pMergeList
end


pro mergeFromSortedPairList_Test
  pairList = [0, 1, 0, 2, 4, 5] ; should generate [0, 1, 2]; [4, 5]
  print, mergeFromSortedPairList(pairList)
  pairList = [0, 1, 0, 2, 2, 3] ; should generate [0, 1, 2, 3]
  print, mergeFromSortedPairList(pairList)
end


;arr1: values
;arr2: keys
function sortArrayPair, arr1, arr2, out1 = out1, out2 = out2, unique = unique
  sortedKeys = sort(arr2)
  keysArr = arr2[uniq(arr2, sortedKeys)]
  nKeys = n_elements(keysArr)
  out1 = [arr1[0]]
  out2 = [arr2[0]]
  for i = 0, nKeys-1 do begin
    whereKeyI = where(arr2 eq keysArr[i], countKeyI)
    subArrKeyI = arr1[whereKeyI]
    sortedArrKeyI = sort(subArrKeyI)
    tmp = arr1[whereKeyI[sortedArrKeyI]]
    if keyword_set(unique) then tmp = tmp[uniq(tmp, sort(tmp))]
    nTmp = n_elements(tmp)
    out1 = [out1, tmp]
    out2 = [out2, replicate(keysArr[i], nTmp)]
  endfor
  out1 = out1[1:*]
  out2 = out2[1:*]
  return, nKeys
end


; mergeArraysToUnique
; Given two input arrays, merge them and return a sorted array with no repeated elements.
; Set fNoSortArr1 | fNoSortArr1 to avoid pre-sorting (reduces some processing time).
function mergeArraysToUnique, arr1, arr2, fNoSortArr1 = fNoSortArr1, fNoSortArr2 = fNoSortArr2
  merge = [keyword_set(fNoSortArr1) ? arr1 : arr1[sort(arr1)], $
           keyword_set(fNoSortArr2) ? arr2 : arr2[sort(arr2)]]
  return, merge[uniq(merge, sort(merge))]
end


; removeDuplicatesFromObjArray
; Returns the indices of non-duplicated objects from a given list.
; WARNING: initial version is a bubble-sort implementation, so it will be very slow with large arrays.
function removeDuplicatesFromObjArray, objList
  nObj = n_elements(objList)
  if (nObj eq 0) then return, -1
  if (nObj eq 1) then return, [0]
  outIndexFlags = bytArr(nObj)
  for i = 0u, nObj-2 do $
    for j = i+1, nObj-1 do $
      if (objList[i] eq objList[j]) then outIndexFlags[j] = 1
  nOut = total(outIndexFlags)
  if (nOut gt 0) then begin
    uniqIndexList = [-1L]
    for i = 0, nObj-1 do $
      if outIndexFlags[i] eq 0 then uniqIndexList = [uniqIndexList, i]
    return, uniqIndexList[1:*]
  endif else return, uLonArr(nObj)
end


; isObjInObjArray
; Returns the position of the first occurence of a given object in an object list (array).
function whereObjInObjArray, obj, objList

  if ~obj_valid(obj) then return, -1

  nObj = n_elements(objList)
  if (nObj lt 1) then return, -1

  for i = 0u, nObj-1 do $
    if obj eq objList[i] then return, i

  return, -1
end


; sortCircularIndexList
;
; The input list is assumed to have only one index "jump", i.e. the list is
; a consecutive run of indices that goes through 0.
function sortCircularIndexList, list, nMax, sortedIndices = sortedIndices, throughZero = throughZero
  nList = n_elements(list)
  if (nList lt 2) then return, list
  sortedListIndices1 = uniq(list, sort(list))
  sortedList  = list[sortedListIndices1]
  zeroPos     = where(sortedList eq 0, countZero)
  nMaxPos     = where(sortedList eq nMax, countNmax)
  nSortedList = n_elements(sortedList)
  throughZero = (countZero eq 1) and (countNmax eq 1)
  if throughZero then throughZero = throughZero and (nMaxPos ne nMax)
  if ~throughZero then begin
    sortedIndices = sortedListIndices1
  endif else begin
    i = 1u
    breakIndexPos = -1L
    while (i lt nSortedList) and (breakIndexPos lt 0) do $
      if ((sortedList[i] - sortedList[i-1]) gt 1) $
      then breakIndexPos = i $
      else i += 1
    subList1        = sortedList[0:breakIndexPos-1]
    subListIndices1 = uIndGen(breakIndexPos)
    subList2        = sortedList[breakIndexPos:*]
    subListIndices2 = breakIndexPos + uIndGen(nSortedList-breakIndexPos)
    sortedIndices   = [subListIndices2, subListIndices1]
    sortedList      = sortedList[sortedIndices]
  endelse
  return, sortedList
end
