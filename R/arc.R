#-----
# Different types arc
# OPEN, CHORD, PIE,
# inspiration and adopted from
# https://processing.org/reference/arc_.html
#-----
arc <- function(
	x,
	y,
	width =  1,
	height =  1,
	fill = 'grey80',
	stroke = 'grey10',
	stroke.size = 1,
	mode = 'OPEN',
	s.angle = 0,
	e.angle = 50,
	type = 'OPEN'
)
{
	
	w <- scaleComb(width)$x
	h <- scaleComb(height)$y
	###
	# fill area
	fillArea <- getXY(
		x = x,
		y = y,
		r.x = w,
		r.y = h,
		start.angle = s.angle,
		end.angle = e.angle
	)
	polypath(
		x = fillArea$x,
        y = fillArea$y,
        border = NA,
        col = fill
    )
    ###
	# stroke area
	r_x_outer <- scaleComb(
		r = (width +  stroke.size * 0.1)
	)$x
	r_y_outer <- scaleComb(
		r = (height +  stroke.size * 0.1)
	)$y
	strokeArea <- getXY(
		x = x,
      	y = y,
      	r.x = r_x_outer,
      	r.y = r_y_outer,
     	start.angle = s.angle,
		end.angle = e.angle 	
	)
	polypath(
		x = c(fillArea$x, rev(strokeArea$x)),
		y = c(fillArea$y, rev(strokeArea$y)),
		border = NA,
		col = stroke
	)
}