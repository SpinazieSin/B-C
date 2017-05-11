#####################################
# Q_LEARNING part of the assignment #
#####################################

# Return new Q_value
UPDATE_Q <- function(value, reward, alpha){
  return(value+alpha*DELTA(reward,value))
}

# Calculate error of Q_value
DELTA <-function(reward, value){
  return(reward-value)
}

# Q_LEARNING function
Q_LEARN <- function(alpha, epsilon, trials){
  set.seed(123)
  Q = c(0, 0, 0, 0)
  K = c(20, 30, 50, 70)
  for (trial in 1:trials){
    random_choice <- runif(1, 0.0, 1.0)
    # Random choice (explore)
    if (epsilon >= random_choice){
      action = sample(1:4, 1)
    # Choose maximum value gain (exploit)
    } else {
      action = which.max(Q)
    }
    # Update Q value for action
    reward = K[action] + runif(1, -4.0, 4.0)
    Q[action] = UPDATE_Q(Q[action], reward, alpha)
  }
  return(Q)
}

# Calls Q_LEARN multiple times to get rid of errors
MULTIPLE_Q_LEARN <- function(episodes, alpha, epsilon, trials){
  Q_result = matrix(nrow=500, ncol=4)
  for (episode in 1:episodes){
    Q_values = Q_LEARN(alpha, epsilon, trials)
    Q_result[episode, 1] = Q_values[1]
    Q_result[episode, 2] = Q_values[2]
    Q_result[episode, 3] = Q_values[3]
    Q_result[episode, 4] = Q_values[4]
  }
  return(Q_result)
}

# Run this for a simple way to test/compare everything for multiple values for alpha
MANY_EPISODE_LEARN <- function() {
  # alpha = 0.1, epsilon = 0.1
  res1 = MULTIPLE_Q_LEARN(500, 0.1, 0.1, 200)
  mean_res1 = colSums(res1, na.rm = FALSE, dims = 1)/500
  print('alpha = 0.1, epsilon = 0.1')
  print(mean_res1)
  
  # alpha = 0.5, epsilon = 0.5
  res2 = MULTIPLE_Q_LEARN(500, 0.5, 0.1, 200)
  mean_res2 = colSums(res2, na.rm = FALSE, dims = 1)/500
  print('alpha = 0.5, epsilon = 0.1')
  print(mean_res2)
}

###################################
# Exploration - exploitation part #
###################################

Q_LEARN_decay <- function(alpha, epsilon, trials){
  set.seed(123)
  Q = c(0, 0, 0, 0)
  K = c(20, 30, 50, 70)
  for (trial in 1:trials){
    epsilon = epsilon - (epsilon/500)
    random_choice <- runif(1, 0.0, 1.0)
    # Random choice (explore)
    if (epsilon >= random_choice){
      action = sample(1:4, 1)
      # Choose maximum value gain (exploit)
    } else {
      action = which.max(Q)
    }
    # Update Q value for action
    reward = K[action] + runif(1, -4.0, 4.0)
    Q[action] = UPDATE_Q(Q[action], reward, alpha)
  }
  return(Q)
}

# Calls Q_LEARN_decay multiple times to get rid of errors
MULTIPLE_Q_LEARN_decay <- function(episodes, alpha, epsilon, trials){
  Q_result = matrix(nrow=500, ncol=4)
  for (episode in 1:episodes){
    Q_values = Q_LEARN_decay(alpha, epsilon, trials)
    Q_result[episode, 1] = Q_values[1]
    Q_result[episode, 2] = Q_values[2]
    Q_result[episode, 3] = Q_values[3]
    Q_result[episode, 4] = Q_values[4]
  }
  return(Q_result)
}

# Call Q_LEARN decay with multiple values for Epsilon
MANY_EPISODE_LEARN_decay <- function() {
  # alpha = 0.1, epsilon = 0.1
  res1 = MULTIPLE_Q_LEARN_decay(500, 0.3, 0.05, 200)
  mean_res1 = colSums(res1, na.rm = FALSE, dims = 1)/500
  print('alpha = 0.3, epsilon = 0.05')
  print(mean_res1)
  
  # alpha = 0.5, epsilon = 0.5
  res2 = MULTIPLE_Q_LEARN_decay(500, 0.3, 0.2, 200)
  mean_res2 = colSums(res2, na.rm = FALSE, dims = 1)/500
  print('alpha = 0.3, epsilon = 0.2')
  print(mean_res2)
  
  # alpha = 0.5, epsilon = 0.5
  res3 = MULTIPLE_Q_LEARN_decay(500, 0.3, 0.6, 200)
  mean_res3 = colSums(res2, na.rm = FALSE, dims = 1)/500
  print('alpha = 0.3, epsilon = 0.6')
  print(mean_res3)
}

###########
# SOFTMAX #
###########

#Q_LEARN_SMAX
