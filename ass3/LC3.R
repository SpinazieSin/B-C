
TD_ZERO <- function(alpha, gamma, episodes){
	V = c(0,0,0,0,0,0,0)
	for (episode in 1:episodes){
		state = 3
		while (1){
			coinflip = runif(1, 0, 1)
			if (coinflip > 0.5 && state > 1) {
				V[state] = v_value(V[state], V[state-1], 0, alpha, gamma)
				state = state-1
			}
			else {
				if (state+1==7) {
					reward = 1
				}
				else {
					reward = 0
				}
				V[state] = v_value(V[state], V[state+1], reward, alpha, gamma)
				state = state+1
			}
			if (state == 7 || state == 1){
				break
			}
		}
	}
	return(V)
}

small_TD_ZERO <- function(alpha, gamma, V){
	state = 3
	while (1){
		coinflip = runif(1, 0, 1)
		if (coinflip > 0.5 && state > 1) {
			V[state] = v_value(V[state], V[state-1], 0, alpha, gamma)
			state = state-1
		}
		else {
			if (state+1==7) {
				reward = 1
			}
			else {
				reward = 0
			}
			V[state] = v_value(V[state], V[state+1], reward, alpha, gamma)
			state = state+1
		}
		if (state == 7 || state == 1){
			break
		}
	}
	return(V)
}

v_value <- function(state, next_state, next_reward, alpha, gamma) {
	return(state + alpha * (next_reward + gamma* next_state - state))
}

loop_TD_ZERO <- function(alpha, gamma, episodes){
	result = c(1:episodes)
	standard = c(0, 1/6, 2/6, 3/6, 4/6, 5/6, 0)
	V = c(0,0,0,0,0,0,0)
	for (run in 1:episodes){
		V = small_TD_ZERO(alpha, gamma, V)
		result[run] = sum((standard - V)^2)
	}
	return(result)
}

# result = loop_TD_ZERO(0.1, 1, 100)
# result = loop_TD_ZERO(0.7, 1, 100)
# result = loop_TD_ZERO(0.1, 0.9, 100)
result = loop_TD_ZERO(0.1, 0.5, 100)

plot(result, type="l")
 
# V(s_t=s) = E[r_{t+1}+γ*V(s_{t+1})|s_t=s]
# V(s_t) = V(s_t)+α*[r_{t+1}+γ*V(s_{t+1})–V(s_t)]
# Q(s_t,a_t) = Q(s_t,a_t)+α*δ
# δ	= r_{t+1} + γ*maxQ(s_{t+1},a)−Q(s_t,a_t)