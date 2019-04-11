# filter_by_sd.r
# Notes
	# 1. Built for ADCP data.
# 	Future Extensions
	# 1. Remove requirement that Bins exist in data; write for a single series or a whole series regardless of the names of the series.


	# Removes extreme values as identified by their position on a histogram with respect to a critical threshold, a multiple of the standard deviation of the data set.
		# Going to go through and filter out data outside of 6 standard deviations
	filter_by_sd <- function(ddata, threshold.sd = 6, series = c("Nor","Eas","Ver","Mag"), verbose = FALSE){
		# Setup
		series.names <- names(ddata)[match(series, names(ddata))] # Vector of time-series names that need to be filtered
		if('Bin' %in% names(ddata)){no.bins <- max(ddata$Bin)}
		# Method: Single Bin in input ddata
			if(length(unique(ddata$Bin)) == 1){ # single bin method 
				for(i in 1:length(series.names)){ # Loop through each time-series, default is Northing, Easting, and Vertical
					series.i <-  ddata[[series.names[i]]] #select(ddata, eval(as.name(series.names[i]))) # current time series
					if(series.names[i] == "Mag")dir.i <- ddata[["Dir.true"]]
					critical.value.upper <- mean(unlist(series.i[]), na.rm = TRUE) + threshold.sd * sd(unlist(series.i[]), na.rm = TRUE) # compute sd of current time-series, multiply by threshold.sd
					critical.value.lower <- mean(unlist(series.i[]), na.rm = TRUE) - threshold.sd * sd(unlist(series.i[]), na.rm = TRUE) # compute sd of current time-series, multiply by threshold.sd
					series.extreme.values <- which(abs(series.i) > critical.value.upper | abs(series.i) < critical.value.lower) # vector of indices that need to be fixed
					# Below code will replace the extreme values with the mean of the nearest non-extreme value to the left and right in the series
					for(j in 1:length(series.extreme.values)){
						row.j <- series.extreme.values[j] # current row that needs to be fixed via linear interpolation
						old.val <- series.i[row.j] # current (extreme) value of that row
							if(old.val > critical.value.upper){  # interpolate too high values
								series.i[row.j] <- mean(c(series.i[max(which(series.i[1:(row.j - 1)] < critical.value.upper))],
									series.i[(min( which(series.i[((row.j + 1):length(series.i))] < critical.value.upper) ) + row.j)] ), na.rm = TRUE)
							if(series.names[i] == "Mag"){							
									dir.i[row.j] <- mean(c(dir.i[max(which(series.i[1:(row.j - 1)] < critical.value.upper))],
										dir.i[(min( which(series.i[((row.j + 1):length(series.i))] < critical.value.upper) ) + row.j)] ), na.rm = TRUE)
								}}
								if(old.val < critical.value.lower){ # interpolate too low values
								series.i[row.j] <- mean(c(series.i[max(which(series.i[1:(row.j - 1)] > critical.value.lower))],
									series.i[(min( which(series.i[((row.j + 1):length(series.i))] > critical.value.lower) ) + row.j)] ), na.rm = TRUE)								
								if(series.names[i] == "Mag"){
									dir.i[row.j] <- mean(c(dir.i[max(which(series.i[1:(row.j - 1)] > critical.value.lower))],
										dir.i[(min( which(series.i[((row.j + 1):length(series.i))] > critical.value.lower) ) + row.j)] ), na.rm = TRUE)															
								}
								}
						if(verbose == TRUE){
							print(paste0("Fixed Row: ", series.extreme.values[j], " in ", names(series.i)[1]))
							print(paste0("...Old Value = ", old.val))
							print(paste0("...Critical Value = ", critical.value.lower, " ", critical.value.upper))	
							print(paste0("...New Value = ", series.i[row.j]))
						}
					}
					ddata[ ,series.names[i]] <- series.i
					if(series.names[i] == "Mag"){ddata[,"Dir.true"] <- dir.i}

				}
			}
		# Method: Multiple Bins in input ddata
			if(length(unique(ddata$Bin)) > 1){ 
				for(b in 1:length(unique(ddata$Bin))){
					ddata.temp <- dplyr::filter(ddata, Bin == b)
					for(i in 1:length(series.names)){ # Loop through each time-series, default is Northing, Easting, and Vertical
						series.i <- ddata.temp[[series.names[i]]] #select(ddata.temp, eval(as.name(series.names[i]))) # current time series
						if(series.names[i] == "Mag")dir.i <- ddata.temp[["Dir.true"]] # direction, which also needs to be interpolated
						# Note: could take critical values from total series (e.g., "Nor"). Currently, taking critical value from series filtered to current Bin.
						critical.value.upper <- mean(unlist(series.i[]), na.rm = TRUE) + threshold.sd * sd(unlist(series.i[]), na.rm = TRUE) # compute sd of current time-series, multiply by threshold.sd
						critical.value.lower <- mean(unlist(series.i[]), na.rm = TRUE) - threshold.sd * sd(unlist(series.i[]), na.rm = TRUE) # compute sd of current time-series, multiply by threshold.sd
						series.extreme.values <- which(series.i > critical.value.upper | series.i < critical.value.lower) # vector of indices that need to be fixed
				# Below code will replace the extreme values with the mean of the nearest non-extreme value to the left and right in the series
						if(length(series.extreme.values) >= 1){
							for(j in 1:length(series.extreme.values)){
								row.j <- series.extreme.values[j] # current row that needs to be fixed via linear interpolation
								old.val <- series.i[row.j] # current (extreme) value of that row
								if(old.val > critical.value.upper){ # interpolate too high values
								series.i[row.j] <- mean(c(
									series.i[max(which(series.i[1:(row.j - 1)] < critical.value.upper))],
									series.i[(min( which(series.i[((row.j + 1):length(series.i))] < critical.value.upper) ) + row.j)] ), na.rm = TRUE)
								if(series.names[i] == "Mag"){
									dir.i[row.j] <- mean(c(dir.i[max(which(series.i[1:(row.j - 1)] < critical.value.upper))],
										dir.i[(min( which(series.i[((row.j + 1):length(series.i))] < critical.value.upper) ) + row.j)] ), na.rm = TRUE)
								}}
								if(old.val < critical.value.lower){ # interpolate too low values
								series.i[row.j] <- mean(c(series.i[max(which(series.i[1:(row.j - 1)] > critical.value.lower))],
									series.i[(min( which(series.i[((row.j + 1):length(series.i))] > critical.value.lower) ) + row.j)] ), na.rm = TRUE)								
								if(series.names[i] == "Mag"){
									dir.i[row.j] <- mean(c(dir.i[max(which(series.i[1:(row.j - 1)] > critical.value.lower))],
										dir.i[(min( which(series.i[((row.j + 1):length(series.i))] > critical.value.lower) ) + row.j)] ), na.rm = TRUE)								
								}}
								if(verbose == TRUE){
									print(paste0("Fixed Row: ", series.extreme.values[j], " in ", names(series.i)[1], " in Bin: ", b))
									print(paste0("...Old Value = ", old.val))
									print(paste0("...Critical Value = ", critical.value.lower, " ", critical.value.upper))		
									print(paste0("...New Value = ", series.i[row.j]))
								}
							}
						ddata.temp[ ,series.names[i]] <- series.i
						if(series.names[i] == "Mag"){ddata.temp[,"Dir.true"] <- dir.i}
					}
				}
					ddata[which(ddata$Bin == b), c(series.names, "Dir.true")] <- ddata.temp[ , c(series.names, "Dir.true")]
				}
			}

		return(ddata)
	}
