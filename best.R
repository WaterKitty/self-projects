best <- function(state, outcome){

	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	my_data <- as.data.frame(cbind(data[, 2],
						                    data[, 7],
						                    data[, 11],
						                    data[, 17],
						                    data[, 23]),
				            stringAsFactors = FALSE)

	colnames(my_data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia") 
	
	if(!state %in% my_data[, "state"]){
		stop("Invalid state.")
	}
	else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
		stop("Invalid outcome.")
	}
	else {
		see <- which(my_data[, "state"] == state)
		set <- my_data[see, ]
		oi <- as.numeric(set[, eval(outcome)])
		min_value <- min(oi, na.rm = TRUE)
		result <- set[, "hospital"][which (oi == min_value)]
		output <- result[order(result)]
	}
	return(output)

}	
