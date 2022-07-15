library(brms)


# Prior 1 to 3 --> more informative to less informative
prior1 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,150)", class = "b"),  
            prior("normal(0,200)", class = "sd"),    
            prior("normal(0,500)", class = "sigma"))

prior2 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,100)", class = "b"),  
            prior("normal(0,200)", class = "sd"),
            prior("normal(0,500)", class = "sigma"))

prior3 <- c(prior("normal(300,1000)", class = "Intercept"),
            prior("normal(0,100)", class = "b"),  
            prior("normal(0,150)", class = "sd"),
            prior("normal(0,300)", class = "sigma"))


get_brms_parameters <- function(prior_type){
  curr_prior <- ifelse(prior_type == 'prior1', prior1,
                  ifelse(prior_type == 'prior2', prior2, prior3))
  
  parms <- list(prior = curr_prior,
                ncores = 4,
                niters = 12000,
                seed = 117,
                warmup = 6000,
                adapt_delta = 0.8)
  
  return(parms)
}



