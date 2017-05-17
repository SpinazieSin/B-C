
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

expected_value <- function(probability, consequence){
	return(sum(probability*consequence))
}

expected_utility <- function(probability, consequence, alpha, beta, delta){
	u = c(1:length(consequence))
	for (i in 1:length(consequence)){
		u[i] = utility(alpha, beta, consequence[i], delta)
	}
	return(sum(probability*consequence))
}
