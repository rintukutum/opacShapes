#---------------
# circle =  point + stroke
#---------------
circle <- function(
	x,	# x-coordinate
	y,  # y-coordinate
	fill, # fill color
	stroke, # stroke color
	r = 1, # radius
	stroke.position = 'out' # 'in' = inside, mid = 'middle', out = 'outside'

){
	# draw point
	point(
		x = x,
		y = y,
		r = r,
		fill = fill,
		xycor = TRUE
		)
	# draw stroke after point
	pos.implem <- c('in','mid','out')
	if(any(pos.implem == stroke.position))
	{
		if(stroke.position == 'in'){
			# compute xy coord
		}
		if(stroke.position == 'mid'){
			# compute xy coord

		}
		if(stroke.position == 'out'){
			# compute xy coord

		}
	}else{
		# eror msg
		stop(paste0(
			' In stroke.position\n'
			'"in", "mid" and "out" allowed!\n'
			))
	}
}