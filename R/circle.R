#---------------
# circle =  point + stroke
#---------------
circle <- function(
	x,	# x-coordinate
	y,  # y-coordinate
	fill = 'grey80', # fill color
	stroke = 'skyblue', # stroke color
	r = 1, # radius
	stroke.size = 1,
	stroke.position = 'out' # 'in' = inside, mid = 'middle', out = 'outside'

){
	point(
		x = x,
		y = y,
		r = r,
		fill = fill
	)
	# draw stroke after point
	pos.implem <- c('mid','out')
	if(any(pos.implem == stroke.position))
	{
		drawStroke(
			x = x,
			y = y,
			r = r,
			stroke = stroke,
			stroke.size = stroke.size,
			stroke.position = stroke.position
		)
	}else{
		# eror msg
		stop(paste0(
			' In stroke.position\n',
			'"in", "mid" and "out" allowed!\n'
			))
	}

}

getXY <- function(
	x,
	y,
	r.x,
	r.y
	)
{
	rad <- (0:360/180)*pi
	coord <- list(
		x = x + r.x * cos(rad),
		y = y + r.y * sin(rad)
		)
	return(coord)
}