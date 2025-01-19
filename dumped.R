## dumped functions
# Jan 2025

# read and process Excel file (version that just does two columns)
my_read_excel_old = function(input_file){
  # read in the file and assign dummy names (two groups only so far)
  raw = read_excel(path = input_file, sheet=1, col_names = FALSE, skip=2)
  names(raw) = paste('v', 1:ncol(raw), sep='')
  # find percent and continuous
  row_cont = which(str_detect(string=raw$v1, pattern='Continuous variables'))[1]
  row_percent = which(str_detect(string=raw$v1, pattern='Numbers or percents'))[1]
  # extract continuous summary stats
  continuous = raw[(row_cont+2):(row_percent-1),] %>%
    mutate(v2 = as.numeric(v2),
           v3 = as.numeric(v3),
           v4 = as.numeric(v4),
           v5 = as.numeric(v5),
           v6 = as.numeric(v6),
           v7 = as.numeric(v7)) %>%
    filter(!is.na(v2),
           !is.na(v3),
           !is.na(v4),
           !is.na(v5),
           !is.na(v6),
           !is.na(v7)) %>%
    rename('n1' = 'v2',
           'm1' = 'v3',
           'sd1' = 'v4',
           'n2' = 'v5',
           'm2' = 'v6',
           'sd2' = 'v7')
  # extract percent summary stats
  percents = raw[(row_percent+2):nrow(raw),] %>%
    mutate(v2 = as.numeric(v2),
           v3 = as.numeric(v3),
           v4 = as.numeric(v4),
           v5 = as.numeric(v5)) %>%
    filter(!is.na(v2),
           !is.na(v3),
           !is.na(v4),
           !is.na(v5)) %>%
    rename('n1' = 'v2',
           'N1' = 'v3',
           'n2' = 'v4',
           'N2' = 'v5')
  
  # null if no rows
  if(nrow(continuous)==0){continuous = NULL}
  if(nrow(percents)==0){percents = NULL}
  
  # return
  data = list()
  data$continuous = continuous
  data$percents = percents
  return(data)
}

# approximation of two-sample t-test for binomial data (see D'Agostino 1998)
t.test2.binomial <- function(a, b, c, d, return_what = 't')
{
  # fix for zero counts
  if(a==0){a=0.5}
  if(b==0){b=0.5}
  #
  m = a + c
  n = b + d
  p1 = a/m
  p2 = b/n
  var1 = p1*(1-p1) # variance
  var2 = p2*(1-p2)
  pooled_var = ((m-1)*var1 + (n-1)*var2) / (m+n-2) # pooled variance
  denom = sqrt((1/m) + (1/n))
  mdiff = p1 - p2 # mean difference
  se = sqrt(pooled_var) * denom
  t = mdiff / se
  df <- m+n-2
  p <- 2*pt(-abs(t), df)
  if(return_what=='difference'){return(mdiff)}
  if(return_what=='se'){return(se)}
  if(return_what=='t'){return(t)}
  if(return_what=='p'){return(p)}
}


## function to run Bayesian model (winbugs version)
run_bayes_test_winbugs = function(in_data,
                                  MCMC = 1000,
                                  thin = 3,
                                  n.chains = 2,
                                  debug = FALSE
){
  # prepare the data for Winbugs
  N = nrow(in_data) # number of statistics
  N_studies = length(unique(in_data$study)) # number of studies
  bdata = list(N = N, 
               mdiff = in_data$mdiff,
               N_studies = N_studies, 
               df = in_data$size - 1, # degrees of freedom
               study = in_data$study, 
               inv.sem2 = 1 / in_data$sem2) # inverse-variance
  
  ## initial values
  # precision
  mu.var = matrix(data=NA, ncol=2, nrow=N_studies) # start with NA
  mu.var[,2] = 0.1 # small positive
  #
  inits = list(mu.var = mu.var, 
               var.flag = rep(0, N_studies))  # start all with no flag for mean or variance
  inits = rep(list(inits), n.chains) # repeat per chains
  
  parms = c('var.flag','mu.var')
  bugs = bugs(data=bdata, inits=inits, parameters=parms, model.file='bugs_model.txt', DIC=FALSE,
              n.chains=n.chains, n.iter=MCMC*thin*2, n.thin=thin, bugs.seed=1234, debug=debug,
              bugs.directory="c:/Program Files/WinBUGS14")
  
  # summary stats
  stats = bugs$summary[, c(1,3,7)]
  stats = data.frame(stats)
  names(stats) = c('mean','lower','upper')
  stats$var = row.names(stats)
  # pick out results
  p.flag = filter(stats, var=='var.flag[1]') %>% pull(mean)
  mult =  filter(stats, var=='mu.var[1,2]')  %>%
    mutate(mean = exp(mean),
           lower = exp(lower),
           upper = exp(upper))
  
  #
  to.return = list()
  to.return$p.flag = p.flag
  to.return$mult = mult
  return(to.return)
} # end of function


## function to run Bayesian model (nimble version)
run_bayes_test = function(in_data,
                          MCMC = 1000,
                          p_prior = 0.5,
                          thin = 3
){
  
  # prepare the data for nimble (version with single study)
  N = nrow(in_data) # number of statistics
  constants = list(N = N, p_prior = p_prior)
  #
  data = list(mdiff = in_data$mdiff,
              inv.sem2 = 1 / in_data$sem2, # inverse-variance
              df = in_data$size - 1) # degrees of freedom
  
  ## initial values
  mu.var = rep(NA, 2)
  mu.var[2] = 0.1 # small positive
  #
  inits = list(mu.var = mu.var, 
               var.flag = 0)  # start all with no flag for mean or variance
  
  # model
  baselineCode <- nimbleCode({
    # Model
    for(i in 1:N){
      mdiff[i] ~ dt(0, tau[i], df[i])
      tau[i] <- inv.sem2[i] * inv.var # precision
    }
    
    # Priors
    # spike-slab for inverse-variance
    log(inv.var) <- mu.var[pick]
    pick <- var.flag + 1
    var.flag ~ dbern(p_prior) # 
    mu.var[1] <- 0 # spike at zero (no change in precision)
    mu.var[2] ~ dnorm(0, 0.1) # "slab"
  })
  
  # make chains
  nimbleMCMC_samples <- nimbleMCMC(code = baselineCode, 
                                   constants = constants, 
                                   data = data, 
                                   inits = inits,
                                   setSeed = 816,
                                   thin = thin,
                                   niter = MCMC*thin*2,
                                   nburnin = MCMC*thin)
  
  # summary stats
  means = colMeans(nimbleMCMC_samples)
  lower = apply(nimbleMCMC_samples, 2, FUN=quantile, probs=0.05)
  upper = apply(nimbleMCMC_samples, 2, FUN=quantile, probs=0.95)
  stats = data.frame(var = colnames(nimbleMCMC_samples), mean = means, lower = lower, upper=upper)
  row.names(stats) = NULL
  
  # pick out results
  p.flag = filter(stats, var=='var.flag') %>% pull(mean)
  mult =  filter(stats, var=='mu.var[2]')  %>%
    mutate(mean = exp(mean),
           lower = exp(lower),
           upper = exp(upper))
  
  #
  to.return = list()
  to.return$p.flag = p.flag
  to.return$mult = mult
  return(to.return)
} # end of function




## to replace missing with zero, used by 3_compare_algorithm_hand.Rmd
replace_zero = function(x){replace_na(x, '0')}
replace_zero_num = function(x){replace_na(x, 0)}
# simple function used by 4_model.R
is.this = function(x, i){sum(x==i)/length(x)}

