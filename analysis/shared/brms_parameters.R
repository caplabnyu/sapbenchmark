library(brms)


# Prior 1 to 3 --> less informative to more informative

get_brms_parameters <- function(prior_type){
  
  if(prior_type == 'prior1'){
    curr_prior = c(prior("normal(300,1000)", class = "Intercept"),
                   prior("normal(0,150)", class = "b"),  
                   prior("normal(0,200)", class = "sd"),    
                   prior("normal(0,500)", class = "sigma"))
  }else if(prior_type == 'prior2'){
    curr_prior = c(prior("normal(300,1000)", class = "Intercept"),
                   prior("normal(0,100)", class = "b"),  
                   prior("normal(0,200)", class = "sd"),
                   prior("normal(0,500)", class = "sigma"))
  }else if(prior_type == 'prior3'){
    curr_prior =c(prior("normal(300,1000)", class = "Intercept"),
                  prior("normal(0,100)", class = "b"),  
                  prior("normal(0,150)", class = "sd"),
                  prior("normal(0,300)", class = "sigma"))
  }else{
    print('ENTER A VALID PRIOR')
  }
  
  parms <- list(prior = curr_prior,
                ncores = 4,
                niters = 12000,
                seed = 117,
                warmup = 6000,
                adapt_delta = 0.8)
  
  return(parms)
}



