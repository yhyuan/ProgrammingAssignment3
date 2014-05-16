rankall <- function(outcome, num = "best") {
	## Read outcome data
	dt <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")

	if (!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))) {
		stop("invalid outcome")
	} else {
		if (tolower(outcome) == "heart attack") {
			dt.subset <- dt[, c(2, 7, 11)]
		}
		if (tolower(outcome) == "heart failure") {
			dt.subset <- dt[, c(2, 7, 17)]
		}
		if (tolower(outcome) == "pneumonia") {
			dt.subset <- dt[, c(2, 7, 23)]
		}
		dt.subset[, 3] <- as.numeric(dt.subset[, 3])
		colnames(dt.subset)[3] <- "Rate"
		state <- sort(unique(dt.subset$State))
		hospital <- rep(NA, length(state))
		for (i in 1:length(state)) {
			statename = state[i]
			dt.subset2 <- dt.subset[dt.subset$State == statename, ]
			dt.subset2 <- dt.subset2[order(dt.subset2[,3], dt.subset2[,1],decreasing=FALSE),]
			dt.subset2$Rank <- c(1:nrow(dt.subset2))
			Rank <- num
			if (num == "best") {
				Rank <- 1
			}
			if (num == "worst") {
				Rank <- max(dt.subset2[!is.na(dt.subset2$Rate), ]$Rank)
			}		
			dt.subset2 <- dt.subset2[dt.subset2$Rank == Rank, ]
			if(nrow(dt.subset2) == 0) {
				hospital[i] = NA
			} else {
				hospital[i] = dt.subset2$Hospital.Name[1]
			}
		}
		#cbind(hospital, state)
		data.frame(hospital, state)
	}
}