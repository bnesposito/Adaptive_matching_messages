library(tidyverse)
library(rstan) # For MCMC
library(lpSolve) # For finding the optimal matching
library(patchwork) # For arranging plots

# compile model on start-up to avoid repeated re-compilation
# store it, so it can be re-used next time, if the stan file has not changed
rstan_options(auto_write = TRUE)
betabinomial_logit_stan_model = stan_model("additive_effects_betabinomial_logit.stan")



# take data frame with factors U and V and form a matrix of dummies 
# for all interactions of U, V
predictor_matrix = function(data) {
  model.matrix(~U:V-1, data)
}

# use stan to sample from posterior for the binomial logistic random effects regression model
coefficient_posterior = function(data, n_Y= 13*4) {
  data_list = list(
    N = as.integer(nrow(data)),
    dim_U = length(levels(factor(data$U))),
    dim_V = length(levels(factor(data$V))),
    n_Y = n_Y, #maximum value that Y can take
    X = predictor_matrix(data),
    Y = data$Y
  )
  
  sampling(
    betabinomial_logit_stan_model,
    data = data_list,
    show_messages = F, # suppress output
    refresh = 0, # suppress output
    seed = 12345,
    cores = parallel::detectCores(), # use all the available cores
    control = list(adapt_delta = 0.85) # to reduce "divergent transitions" in mcmc
  )
}    
  

# create predicted outcomes for all possible matches of elements of the factors  U and V
# create data frame with all combinations of U and V, and i,j as indices
predictions_all_combinations = function(beta, U, V) { 
    all_combinations = merge(U %>% mutate(i=row_number()), 
                             V %>% mutate(j=row_number()))
    # add predictions from logit model
    all_combinations %>%
        mutate(yhat = plogis(
          predictor_matrix(all_combinations) %*% beta
        ))
}

# The following plots predicted values for the cross-combinations 
# of setting exactly one of the variables in U and in V equal to 1
plot_prediction_matrix <- function(beta, k1, k2, title = "Predicted outcomes") {
  U = tibble(U = factor(1:k1))
  V = tibble(V = factor(1:k2))
  predictions_all = predictions_all_combinations(beta, U, V)
  
  ggplot(predictions_all, aes(x = i, y = j, z = yhat)) +
    geom_tile(aes(fill = yhat))  +
    scale_fill_gradient(low = "white",
                        high = "dodgerblue4",
                        limits = c(0, 1)) +
    coord_fixed() +
    labs(x="U", y="V",
         title = title) + 
        # caption="This plot shows predicted outcomes\n for each combination of levels of U and V.") +
    scale_x_continuous(breaks = 0:k1) + scale_y_continuous(breaks = 0:k2) +
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.position="none") #+
    #guides(fill = FALSE)
}

# matching is a tibble with columns i and j, each row corresponding to a matched pair
evaluate_matching = function(matching,
                             predictions_all) {
    # select rows of all predictions correspnding to the rows in matching
    left_join(matching, predictions_all, by = c("i", "j")) %>% 
        summarise(average = mean(yhat)) %>%  # return average predicted outcome
        as.numeric()
}



# find a matching that maximizes predicted average outcomes, using integer programming
optimal_matching_lpSolve = function(predictions_all, m, n) {
  # sort first by j, within j by i, then extract yhat
  predictions_all = predictions_all %>%
    arrange(j, i)
  obj = predictions_all$yhat / m
  
  ##################################
  #Set up the constraint matrix
  #Sizes submatrix
  size <- rep(1, m) ##family size - 1 corresponds to LP
  size_mat <- kronecker(diag(n), 
                        matrix(size, nrow=1))
  # match refugees only once submatrix
  match_once = kronecker(matrix(rep(1,n), nrow=1),
                         diag(m))
  # combine the constraints in one matrix
  constr_mat <- rbind(size_mat, match_once)
  # right hand side of the constraints - to be replaced?
  cap <- c(rep(1,m), rep(1, n))
  
  solve = lp(
    direction="max",
    objective.in = obj,
    const.mat = constr_mat,
    const.dir = rep("<=", (n + m)),
    const.rhs = cap,
    all.bin = TRUE
  )
  
  list(matching=predictions_all[as.logical(solve$solution),] %>% 
         arrange(i,j),
       predicted_average=solve$objval)
}



# putting it all together
thompson_matching = function(prior_data, U, V, 
                             constraints=NULL) {

    posterior = coefficient_posterior(prior_data)
    
    # need to extract one draw from posterior here
    betadraws = (posterior %>% 
        rstan::extract(permuted = TRUE))[["beta"]] #extract the simulation draws
    # extract the first draw for each coefficient (draws are permuted)
    # alpha is intercept, beta the other components
    betadraw = betadraws[1,]
    
    predictions_all = predictions_all_combinations(betadraw, U, V) 

    optimal_matching = 
      optimal_matching_lpSolve(predictions_all, nrow(U), nrow(V))
     
    # estimated coefficients - not used in algorithm, but for reporting results
    optimal_matching$beta_hat= colMeans(betadraws)
  
    optimal_matching
}
