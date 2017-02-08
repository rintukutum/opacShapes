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
	type = 'OPEN' # by default type is OPEN
				  # additional CHORD & PIE
)
{
	fillArea <- drawFillArc(
		x = x,
		y = y,
		w = width,
		h = height,
		fill = fill,
		s.angle = s.angle,
		e.angle = e.angle,
		coord = TRUE
	)
    ###
	# stroke area
	drawStrokeArc(
		x = x,
		y = y,
		w = width,
		h = height,
		stroke = stroke,
		stroke.size = stroke.size,
		s.angle = s.angle,
		e.angle = e.angle,
		fillArea = fillArea,
		type = type
	)
}