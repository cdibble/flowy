# filter_by_mag.r
# Notes
	# 1. Built for ADCP data.
# 	Future Extensions
	# 1. Remove requirement that Bins exist in data; write for a single series or a whole series regardless of the names of the series.

	filter_by_mag <- function(dataf, threshold.mag.horiz = 1500, threshold.mag.ver = 400, series = c("Nor","Eas","Ver", "Mag"), verbose = FALSE){
		series.names <- names(dataf)[match(series, names(dataf))] # Vector of time-series names that need to be filtered
		no.bins <- max(dataf$Bin)

			if(length(unique(dataf$Bin)) == 1){ # Can write a second method for looping over bins, but will focus on single bin method first
				for(i in 1:length(series.names)){ # Loop through each time-series, default is Northing, Easting, and Vertical
					series.i <- select(dataf, eval(as.name(series.names[i]))) # current time series
					if(series.names[i] == "Mag")dir.i <- select(dataf, Dir.true) # direction, which also needs to be interpolated
					ifelse(series.names[i] %in% c("Nor", "Eas", "Mag"),
						threshold.mag <- threshold.mag.horiz, #mm/s for "Nor" and "Eas"
						threshold.mag <- threshold.mag.ver) # mm/s for Ver
					series.extreme.values <- which(abs(series.i) > threshold.mag) # vector of indices that need to be fixed
					# Below code will replace the extreme values with the mean of the nearest non-extreme value to the left and right in the series
					for(j in 1:length(series.extreme.values)){
						row.j <- series.extreme.values[j] # current row that needs to be fixed via linear interpolation
						old.val <- series.i[row.j,] # current (extreme) value of that row
						series.i[row.j,] <- mean( c( series.i[max(which(abs(series.i[1:(row.j - 1), ]) < threshold.mag)), ],
							series.i[(min(which(abs(series.i[((row.j + 1):nrow(series.i)), ]) < threshold.mag)) + row.j), ] ),
							 na.rm = TRUE)
						if(series.names[i] == "Mag"){
							dir.i[row.j, ] <- mean(c( dir.i[max(which(abs(series.i[1:(row.j - 1) , ]) < threshold.mag)), ],
								dir.i[(min( which(abs(series.i[((row.j + 1):nrow(series.i)), ]) < threshold.mag) ) + row.j), ] ), na.rm = TRUE)			
						}
						if(verbose == TRUE){
							print(paste0("Fixed Row: ", series.extreme.values[j], " in ", names(series.i)[1]))
							print(paste0("...Old Value = ", old.val))
							print(paste0("...Critical Value = ", threshold.mag))	
							print(paste0("...New Value = ", series.i[row.j,]))
						}
					}
					dataf[ ,series.names[i]] <- series.i
					if(series.names[i] == "Mag"){dataf[,"Dir.true"] <- dir.i}

				}
			}
			# method for looping through all bins; will execute if dataf has more than one Bin (i.e., is not filtered to a single bin)
			if(length(unique(dataf$Bin)) > 1){ 
				for(b in 1:length(unique(dataf$Bin))){
					dataf.temp <- dplyr::filter(dataf, Bin == b)
					for(i in 1:length(series.names)){ # Loop through each time-series, default is Northing, Easting, and Vertical
						series.i <- select(dataf.temp, eval(as.name(series.names[i]))) # current time series
						if(series.names[i] == "Mag"){dir.i <- select(dataf.temp, Dir.true)} # direction, which also needs to be interpolated
							# Set threshold magnitude, which depends on the dataf series. Northing and Easting are different form Vertical
						ifelse(series.names[i] %in% c("Nor", "Eas", "Mag"),
							threshold.mag <- threshold.mag.horiz, #mm/s for "Nor" and "Eas"
							threshold.mag <- threshold.mag.ver) # mm/s for Ver
					series.extreme.values <- which(abs(series.i) > threshold.mag) # vector of indices that need to be fixed
							# print(series.extreme.values)
							# cat("Bin", b)
							# cat(range(dir.i[,]))
					# Below code will replace the extreme values with the mean of the nearest non-extreme value to the left and right in the series
						if(length(series.extreme.values) >= 1){
							for(j in 1:length(series.extreme.values)){
								row.j <- series.extreme.values[j] # current row that needs to be fixed via linear interpolation
								old.val <- series.i[row.j,] # current (extreme) value of that row
								series.i[row.j,] <- mean(c( series.i[max(which(abs(series.i[1:(row.j - 1) , ]) < threshold.mag)), ],
									series.i[(min( which(abs(series.i[((row.j + 1):nrow(series.i)), ]) < threshold.mag) ) + row.j), ] ), na.rm = TRUE)
								if(series.names[i] == "Mag"){
									dir.i[row.j, ] <- mean(c( dir.i[max(which(abs(series.i[1:(row.j - 1) , ]) < threshold.mag)), ],
										dir.i[(min( which(abs(series.i[((row.j + 1):nrow(series.i)), ]) < threshold.mag) ) + row.j), ] ), na.rm = TRUE)
								}
								if(verbose == TRUE){
									print(paste0("Fixed Row: ", series.extreme.values[j], " in ", names(series.i)[1]))
									print(paste0("...Old Value = ", old.val))
									print(paste0("...Critical Value = ", threshold.mag))		
									print(paste0("...New Value = ", series.i[row.j,]))
								}
							}
						dataf.temp[ ,series.names[i]] <- series.i
						if(series.names[i] == "Mag"){dataf.temp[,"Dir.true"] <- dir.i}
						}
					}
					dataf[which(dataf$Bin == b), c(series.names, "Dir.true")] <- dataf.temp[ , c(series.names, "Dir.true")]
				}
			}

		return(dataf)
	}