library(brms)


# Prior 1 to 3 --> more informative to less informative
prior1 <- c(prior("normal(5.7,1.5)", class = "Intercept"),
            prior("normal(0,1)", class = "b"),  
            prior("normal(0,1.5)", class = "sd"),    
            prior("normal(0,2)", class = "sigma"))


get_brms_parameters <- function(){
  parms <- list(prior = prior1,
                ncores = 4,
                niters = 12000,
                seed = 117,
                warmup = 6000,
                adapt_delta = 0.8)
  
  return(parms)
}



