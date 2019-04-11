garbage_functions.r

# These functions are mostly works in progress
	# Either Broken or Untested.

cat.df <- function(..., append.name = FALSE){
	# data.list <- as.list(substitute(list(...)))[-1L]
	tm <- proc.time()
	data.list <- list(...)[[1]]
	df.out <- get(data.list[[1]])[0,]
	no.dfs <- length(data.list)
	for(i in 1:no.dfs){
		df <- get(data.list[[i]])
		if(append.name == TRUE) df <- mutate(df, name = as.character(data.list[[i]]))
		df.out <- rbind(df.out, df)
	}
	time <- proc.time() - tm
	print(time)
	return(df.out)
	
	}
cat.df2 <- function(...){
	tm <- proc.time()
	data.list <- list(...)[[1]]
	x.list <- lapply(data.list, get)
	df.out <- do.call(rbind, x.list)
	time <- proc.time() - tm 
	print(time)
	return(df.out)
}