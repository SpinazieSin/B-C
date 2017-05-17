
# Thomas Groot 10658017

#####################
## Prospect theory ##
#####################


utility <- function(alpha, beta, x, delta) {
	if (x >= 0) {
		u = x^alpha
	} else {
		u = -delta*((-x)^beta) 
	}
	return(u)
}

subjective_probability <- function()

expected_utility <- function(probability, consequence, alpha, beta, delta){
	u = c(1:length(consequence))
	for (i in 1:length(consequence)){
		u[i] = utility(alpha, beta, consequence[i], delta)
	}
	return(sum(probability*consequence))
}

#data = read.delim("data_LC5.txt")
x = 0
help(ifelse)
ifelse (x < 1, x = 0, x = 1)
