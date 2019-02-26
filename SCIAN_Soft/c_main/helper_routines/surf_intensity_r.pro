function surf_intensity_r, vertices = vertices, volume = volume, xyz_size_per_pixel = xyz_size_per_pixel, r_pixels = r_pixels, pts_factor = pts_factor

  print, 'surf_intensity_r: starting...'
  t1        = sysTime(1)
  size      = size(vertices, /DIMENSIONS)
  size_vol  = size(volume, /DIMENSIONS)
  intensity = make_array(size[1])
  n_pts     = (pts_factor * (2*r_pixels+1) - 1) / 2
  indices   = array_indices([2*n_pts+1, 2*n_pts+1, 2*n_pts+1], indGen(2*n_pts+1, 2*n_pts+1, 2*n_pts+1), /DIMENSIONS) - n_pts
  dist2     = indices[0,*]^2 + indices[1,*]^2 + indices[2,*]^2
  wh        = where(dist2 le n_pts*n_pts)
  sphere    = array_indices([2*n_pts+1, 2*n_pts+1, 2*n_pts+1], wh, /DIMENSIONS) - n_pts
  z_factor  = xyz_size_per_pixel[2] / xyz_size_per_pixel[0]

  for i = 0L, size[1] - 1 do begin
    x = round(vertices[0,i])
    y = round(vertices[1,i])
    z = round(vertices[2,i])
    elements = (sphere + reBin([x, y, z*z_factor] * pts_factor, 3, (size(sphere))[2])) / pts_factor
    elements[2,*] /= z_factor
    for j = 0, n_elements(elements[0,*])-1 do begin
      idX = (round(elements[0,j]) > 0) < (size_vol[0]-1); FASL/JJW change: negative/greater-than-size index not allowed
      idY = (round(elements[1,j]) > 0) < (size_vol[1]-1)
      idZ = (round(elements[2,j]) > 0) < (size_vol[2]-1)
      intensity[i] += volume[idX, idY, idZ]
    endfor
    intensity[i] /= float(j)
  endfor
  print, '...finished in ' +  string(sysTime(1)-t1) + ' secs.'
  return, intensity
end
