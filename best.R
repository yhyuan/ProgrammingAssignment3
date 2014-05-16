best <- function(state, outcome) {
	## Read outcome data
	dt <- read.csv("ProgrammingAssignment3/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")

	if (!(state %in% unique(dt$State))) {
		#print (paste("best(", state, ', ', outcome, ') : invalid state', sep = ""))
		stop("invalid state")
	} else	if (!(tolower(outcome) %in% c("heart attack", "heart failure", "pneumonia"))) {
		#print (paste('best("', state, '", "', outcome, '") : invalid outcome', sep = ""))
		stop("invalid outcome")
	} else {
		dt.subset <- dt[dt$State == state, ]
		if (tolower(outcome) == "heart attack") {
			dt.subset <- dt.subset[, c(2, 7, 11)]
		}
		if (tolower(outcome) == "heart failure") {
			dt.subset <- dt.subset[, c(2, 7, 17)]
		}
		if (tolower(outcome) == "pneumonia") {
			dt.subset <- dt.subset[, c(2, 7, 23)]
		}
		dt.subset[, 3] <- as.numeric(dt.subset[, 3])
		colnames(dt.subset)[3] <- "Rate"
		dt.subset$Rank <- rank(dt.subset$Rate)
		dt.subset <- dt.subset[dt.subset$Rank < 2, ]
		dt.subset$Hospital.Name
	}
}