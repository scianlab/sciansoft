function s_apop_flaeche, image, setAreaAsPixelSize = setAreaAsPixelSize, revIndizes = revIndizes, nObj = nObj, fAllNeighbors = fAllNeighbors ; image: Originalbild
; s_apop_flaeche:	nummeriert zusammenhängende Flächen nach Größe (klein --> groß, 1,2...)
; Feb|98 Steffen
; image: Original Image
; s_apop_flaeche:	enumerate in ascendent order by size (small -> big, 1, 2, ...) 
; Feb|98 Steffen
	dImage = size(image, /dim)
	a = bytArr(dImage[0]+2, dImage[1]+2)
	a[1:dImage[0], 1:dImage[1]] = image

	   ; nummeriert zusammenhaengende binaere Punkte GT 0
	   ; enumerate contiguous binary points GT 0
	lImage = keyword_set(fAllNeighbors) ? (label_region(a gt 0, /uLong, /all_neighbors))[1:dImage[0], 1:dImage[1]]$
	                                    : (label_region(a gt 0, /uLong))[1:dImage[0], 1:dImage[1]]

	if (max(lImage) eq 0) then return, image

	a = histogram(lImage, min = 1, Reverse_Indices = revIndizes)	; ordnet Blobbs Flaechen ohne 0-Flaechen zu
;	sortA  = sort(a)														; order vector: a=[7,3,4] -> sortA=[1,2,0]
	sortA  = sort(a)														; sortiert Vektor: a=[7,3,4] -> sortA=[1,2,0]
	nObj = n_elements(a)
	if (n_elements(setAreaAsPixelSize) eq 0) then $
		for i = 0l,nObj-1 do lImage([revIndizes[revIndizes[i]:revIndizes[i+1]-1]]) = where(sortA eq i)+1 else $
		for i = 0l,nObj-1 do lImage([revIndizes[revIndizes[i]:revIndizes[i+1]-1]]) = a[i]
	return, lImage
end
