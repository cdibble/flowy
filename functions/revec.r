# revec.r 
# function takes to scalar components (Amag and Xmag) and computes resultant vector in geographic coordinate system (north up, east right)
	# geo = TRUE to return Directions in geographic coordinate system (North up, East right). Currently this is the only method
revec <- function(df, Amag = "Nor", Xmag = "Eas", Dir = "Dir", Mag = "Mag", geo = TRUE, wind = FALSE, cartesian.out = FALSE){
	stopifnot(geo == TRUE); cat("Dir input must be in geographic coordinate system to be converted to cartesian corrdinate system")
	Amag.series <- df[[Amag]]
	Xmag.series <- df[[Xmag]] 
	if(Dir %in% names(df)){
		dir.series <- df[[Dir]]
		# store old values
		if(length(dir.series) > 0){df$Dir.old <- dir.series}
	}
	if(Mag %in% names(df)){
		mag.series <- df[[Mag]]
		# store old values
		if(length(mag.series) > 0){df$Mag.old <- mag.series}
	}
	# # Quadrant indices
	q1 <- which(Amag.series > 0 & Xmag.series > 0)
	q2 <- which(Amag.series > 0 & Xmag.series < 0)
	q3 <- which(Amag.series < 0 & Xmag.series < 0)
	q4 <- which(Amag.series < 0 & Xmag.series > 0)
	# initialize df$Dir if necessary
	'%nin%' <- Negate('%in%')  # function to negate the %in% operator
	if(Dir %nin% names(df)){df[[Dir]] <- rep(NA, nrow(df))}
	if(Mag %nin% names(df)){df[[Mag]] <- rep(NA, nrow(df))}
	# compute new Mag and Dir
	if(length(Amag.series) == nrow(df) & length(Xmag.series) == nrow(df)){
		df[,Mag] <- sqrt(df[,Amag]^2 + df[,Xmag]^2)
		if(length(q1) > 0){df[[Dir]][q1] <- (atan(Xmag.series[q1] / Amag.series[q1]) * 180 / pi)}else{cat("No observations in q1. This may not be in error, but check data.")}
		if(length(q2) > 0){df[[Dir]][q2] <- 360 - abs(atan(Xmag.series[q2] / Amag.series[q2]) * 180 / pi)}else{cat("No observations in q2. This may not be in error, but check data.")}
		if(length(q3) > 0){df[[Dir]][q3] <- 180 + (atan(Xmag.series[q3] / Amag.series[q3]) * 180 / pi)}else{cat("No observations in q3. This may not be in error, but check data.")}
		if(length(q4) > 0){df[[Dir]][q4] <- 90 + abs(atan(Amag.series[q4] / Xmag.series[q4]) * 180 / pi)}else{cat("No observations in q4. This may not be in error, but check data.")}
		df[[Dir]][which(df[[Dir]] > 360)] <- df[[Dir]][which(df[[Dir]] > 360)] - 360
	}

	if(wind == TRUE){
		df[[Dir]] <- df[[Dir]] + 180
		df[[Dir]][which(df[[Dir]] > 360)] <- df[[Dir]][which(df[[Dir]] > 360)] - 360
		cat("wind = TRUE means the resulting Dir values indicate the direction from which the vector is origniating, as opposed to the direction to which the vector is headed. I.E. Dir = 90 means an Easterly wind, which is actually blowing toward 270. Don't fuck it up.")
	}

	if(cartesian.out == TRUE){
		df$cartesianDir <- NA
		df$cartesianDir[which(df[[Dir]] <= 90)] <- 90 - df[[Dir]][which(df[[Dir]] <= 90)]
		df$cartesianDir[which(df[[Dir]] > 90)] <- (360 - df[[Dir]][which(df[[Dir]] > 90)]) + 90
	}
	return(df)
}


# revec <- function(df, Amag = "Nor", Xmag = "Eas", Dir = "Dir", Mag = "Mag", geo = TRUE, wind = FALSE, cartesian.out = FALSE){
# 	stopifnot(geo == TRUE); cat("Dir input must be in geographic coordinate system to be converted to cartesian corrdinate system")
# 	Amag.series <- df[, Amag]	
# 	Xmag.series <- df[, Xmag] 
# 	if(Dir %in% names(df)){
# 		dir.series <- df[, Dir]
# 		# store old values
# 		if(length(dir.series) > 0){df$Dir.old <- dir.series}
# 	}
# 	if(Mag %in% names(df)){
# 		mag.series <- df[, Mag]
# 		# store old values
# 		if(length(mag.series) > 0){df$Mag.old <- mag.series}
# 	}
# 	# # Quadrant indices
# 	q1 <- which(Amag.series > 0 & Xmag.series > 0)
# 	q2 <- which(Amag.series > 0 & Xmag.series < 0)
# 	q3 <- which(Amag.series < 0 & Xmag.series < 0)
# 	q4 <- which(Amag.series < 0 & Xmag.series > 0)
# 	# initialize df$Dir if necessary
# 	'%nin%' <- Negate('%in%')  # function to negate the %in% operator
# 	if(Dir %nin% names(df))df[[Dir]] <- rep(0, nrow(df))
# 	# compute new Mag and Dir
# 	if(length(Amag.series) == nrow(df) & length(Xmag.series) == nrow(df)){
# 		df[[Mag]] <- sqrt(df[,Amag]^2 + df[,Xmag]^2)
# 		if(length(q1) > 0){df[[Dir]][q1] <- (atan(Xmag.series[q1] / Amag.series[q1]) * 180 / pi)}else{cat("No observations in q1. This may not be in error, but check data.")}
# 		if(length(q2) > 0){df[[Dir]][q2] <- 360 - abs(atan(Xmag.series[q2] / Amag.series[q2]) * 180 / pi)}else{cat("No observations in q2. This may not be in error, but check data.")}
# 		if(length(q3) > 0){df[[Dir]][q3] <- 180 + (atan(Xmag.series[q3] / Amag.series[q3]) * 180 / pi)}else{cat("No observations in q3. This may not be in error, but check data.")}
# 		if(length(q4) > 0){df[[Dir]][q4] <- 90 + abs(atan(Amag.series[q4] / Xmag.series[q4]) * 180 / pi)}else{cat("No observations in q4. This may not be in error, but check data.")}
# 		df[[Dir]][which(df[[Dir]] > 360)] <- df[[Dir]][which(df[[Dir]] > 360)] - 360
# 	}

# 	if(wind == TRUE){
# 		df[[Dir]] <- df[[Dir]] + 180
# 		df[[Dir]][which(df[[Dir]] > 360)] <- df[[Dir]][which(df[[Dir]] > 360)] - 360
# 		cat("wind = TRUE means the resulting Dir values indicate the direction from which the vector is origniating, as opposed to the direction to which the vector is headed. I.E. Dir = 90 means an Easterly wind, which is actually blowing toward 270. Don't fuck it up.")
# 	}

# 	if(cartesian.out == TRUE){
# 		df$cartesianDir <- NA
# 		df$cartesianDir[which(df[[Dir]] <= 90)] <- 90 - df[[Dir]][which(df[[Dir]] <= 90)]
# 		df$cartesianDir[which(df[[Dir]] > 90)] <- (360 - df[[Dir]][which(df[[Dir]] > 90)]) + 90
# 	}
# 	return(df)
# }