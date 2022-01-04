# 99_functions.R
# functions for shiny app to test baseline tables
# January 2022

# read and process Excel file
my_read_excel = function(input_file){
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
  
  # return
  data = list()
  data$continuous = continuous
  data$percents = percents
  return(data)
}

## t-test from summary stats, from https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
# m1, m2: the sample means
# s1, s2: the sample standard deviations
# n1, n2: the sample sizes
# equal.variance: whether or not to assume equal variance. Default is FALSE. 
t.test2 <- function(mean, sd, n, equal.variance=TRUE, return_what = 'difference')
{
  m1 = mean[1]
  m2 = mean[2]
  s1 = sd[1]
  s2 = sd[2]
  n1 = n[1]
  n2 = n[2]
  if( equal.variance==FALSE ) 
  {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ( (s1^2/n1 + s2^2/n2)^2 )/( (s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1) )
  } else
  {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt( (1/n1 + 1/n2) * ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2) ) 
    df <- n1+n2-2
  }      
  mdiff = m1 - m2
  t <- mdiff/se 
  p <- 2*pt(-abs(t), df)
  if(return_what=='difference'){return(mdiff)}
  if(return_what=='se'){return(se)}
  if(return_what=='t'){return(t)}
  if(return_what=='p'){return(p)}
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

# create t-statistics for continuous data
t.stats.continuous = function(indata){
  
  #
  indata = mutate(indata, row = 1:n()) 
  l1 = select(indata, row, m1, sd1, n1) %>%
    mutate(column = 1) %>%
    rename('mean' = 'm1',
           'sd' = 'sd1',
           'n' = 'n1')
  l2 = select(indata, row, m2, sd2, n2) %>%
    mutate(column = 2) %>%
    rename('mean' = 'm2',
           'sd' = 'sd2',
           'n' = 'n2')
  analysis_ready = bind_rows(l1, l2)
  
  #
  tstats = group_by(analysis_ready, row) %>%
    summarise(size = sum(n),
              mdiff = t.test2(mean=mean, sd=sd, n=n, return_what = 'difference'),
              sem = t.test2(mean=mean, sd=sd, n=n, return_what = 'se'),
              t = t.test2(mean=mean, sd=sd, n=n, return_what = 't'),
              p = t.test2(mean=mean, sd=sd, n=n, return_what = 'p'),
              sem2 = sem^2) %>% # squared
    filter(!is.na(mdiff),
           !is.na(sem2))
  #
  return(tstats)
}

# create t-statistics for percentage data
t.stats.percents = function(indata){
  
  #
  analysis_ready = mutate(indata, 
                  row = 1:n(),
         a = n1, # successes
         b = n2,
         c = N1 - n1, # failures
         d = N2 - n2) %>%
    filter(a>=0, b>=0, c>=0, d>=0)  
  
  #
  tstats = group_by(analysis_ready, row) %>%
    summarise(size = a + b + c + d, # total sample size
              mdiff = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'difference'),
              sem = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'se'),
              t = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 't'),
              p = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'p'),
              sem2 = sem^2) %>% # squared
    filter(!is.na(mdiff),
           !is.na(sem2),
           sem2 > 0)
  #
  return(tstats)
}


## function to run Bayesian model
run_bayes_test = function(in_data,
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


## Make simulated data that copies the input data
make_sim = function(indata){
  
  simp = simc = NULL
  # simulate continuous
  if(is.null(indata$continuous) == FALSE){
    simc = mutate(indata$continuous, row=1:n()) %>%
      group_by(row) %>%
      mutate(m = (m1+m2)/2,
             s = (sd1+sd2)/2,
             n = n1 + n2,
             sem = s / sqrt(n),
             m1 = rnorm(n=1, mean=m, sd=sem),
             m2 = rnorm(n=1, mean=m, sd=sem),
             sd1 = s,
             sd2 = s) %>%
      ungroup() %>%
      select(v1, n1, m1, sd1, n2, m2, sd2)
  }
  
  # simulate percentages
  if(is.null(indata$percents) == FALSE){
    simp = mutate(indata$percents, row=1:n()) %>%
      group_by(row) %>%
      mutate(p = (n1+n2) / (N1 + N2),
             n1 = rbinom(n = 1, p=p, size=N1),
             n2 = rbinom(n = 1, p=p, size=N2)) %>%
      ungroup() %>%
      select(v1, n1, N1, n2, N2)
  }

  sim_data = list()
  sim_data$continuous = simc
  sim_data$percents = simp
  return(sim_data)
}