# devec.r
# Function to decompose vector to orthogonal components
	# use with direction and magnitude to break out into Nor and Eas (the names Nor and Eas assume we're in a geographic coordinate system, but this need not be true and those names can be discarded)
devec <- function(df, Dir = "Dir", Mag = "Mag", geo = TRUE, wind = FALSE){
	stopifnot(geo == TRUE); cat("Dir input must be in geographic coordinate system to be converted to cartesian corrdinate system")
	# if wind = FALSE, temporarily rotate the Dir by 180 so that results will show direction of flow, not direction where flow comes from.
	if(wind == FALSE){
		df[,Dir] <- df[,Dir] + 180
		df[which(df[,Dir] > 360), Dir] <- df[which(df[,Dir] > 360), Dir] -360
	}
	# get series from df
	dir.series <- df[[Dir]]
	mag.series <- df[[Mag]]
	# store old Nor/Eas vals if any
	if('Nor' %in% names(df)){df$Nor.old <- df$Nor}
	if('Eas' %in% names(df)){df$Eas.old <- df$Eas}
	# # Quadrant indices
	q1 <- which(dir.series >= 0 & dir.series < (90))
	q2 <- which(dir.series >= (270) & dir.series < 360)
	q3 <- which(dir.series >= 180 & dir.series < (270))
	q4 <- which(dir.series >= (90) & dir.series < (180))
	# Cartesian direction (0 degrees is east, increasing degress moves counterclockwise)
	df$cartesian.dir <- rep(NA, nrow(df))
	if(length(q1) > 0){df$cartesian.dir[q1] <- 90 - dir.series[q1] } # need to fix angles so that the start at East and rotate counterclockwise
	if(length(q2) > 0){df$cartesian.dir[q2] <- (360 - dir.series[q2]) + 90 } # need to fix angles so that the start at East and rotate counterclockwise
	if(length(q3) > 0){df$cartesian.dir[q3] <- (360 - dir.series[q3]) + 90 } # need to fix angles so that the start at East and rotate counterclockwise
	if(length(q4) > 0){df$cartesian.dir[q4] <- (360 - dir.series[q4] ) + 90 } # need to fix angles so that the start at East and rotate counterclockwise

	df$rad.dir <- df$cartesian.dir * pi / 180 # convert to radians
	df$Eas <- rep(NA, nrow(df)) # initialize cross-shore wind variable
	df$Nor <- rep(NA, nrow(df)) # initilaize alongshore wind variable

	# Breaking out Relative (to coastline) wind direction into alongshore and cross-shore components
	#  Compute along and cross shore wind speed
	if(length(q1) > 0){df$Eas[q1] <- -cos(df$rad.dir[q1])*mag.series[q1]} else{cat("No observations in q1. This may not be in error but check the data.")}
	if(length(q1) > 0){df$Nor[q1] <- -sin(df$rad.dir[q1])*mag.series[q1]} else{cat("No observations in q1. This may not be in error but check the data.")}
	if(length(q2) > 0){df$Eas[q2] <- sin(df$rad.dir[q2] - (pi/2)) * mag.series[q2]} else{cat("No observations in q2. This may not be in error but check the data.")}
  	if(length(q2) > 0){df$Nor[q2] <- -cos(df$rad.dir[q2] - (pi/2)) * mag.series[q2]} else{cat("No observations in q2. This may not be in error but check the data.")}
	if(length(q3) > 0){df$Eas[q3] <- cos(df$rad.dir[q3] - (pi)) * mag.series[q3]} else{cat("No observations in q3. This may not be in error but check the data.")}
  	if(length(q3) > 0){df$Nor[q3] <- sin(df$rad.dir[q3] - (pi)) * mag.series[q3]} else{cat("No observations in q3. This may not be in error but check the data.")}
	if(length(q4) > 0){df$Eas[q4] <- -sin(df$rad.dir[q4] - (3*pi/2)) * mag.series[q4]} else{cat("No observations in q4. This may not be in error but check the data.")}
 	if(length(q4) > 0){df$Nor[q4] <- cos(df$rad.dir[q4] - (3*pi/2)) * mag.series[q4]} else{cat("No observations in q4. This may not be in error but check the data.")}

 	# put the Dir values back where they were, pointing in the direction of flow for wind = FALSE
 	if(wind == FALSE){
		df[,Dir] <- df[,Dir] + 180
		df[which(df[,Dir] > 360), Dir] <- df[which(df[,Dir] > 360), Dir] -360

		df[,"cartesian.dir"] <- df[,"cartesian.dir"] + 180
		df[which(df[,"cartesian.dir"] > 360), "cartesian.dir"] <- df[which(df[,"cartesian.dir"] > 360), "cartesian.dir"] -360

	}

 	return(df)
}