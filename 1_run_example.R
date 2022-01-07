# 1_run_example.R
# run the thermal clothing trial example
# January 2022
TeachingDemos::char2seed('swindon')
source('global.R')

# 1) get the data
#
data = my_read_excel("example_data/excel_example_retracted.xlsx") # under-dispersed
filename = 'example_under.jpg'
#
data = my_read_excel("example_data/excel_example.xlsx")
filename = 'example_fine.jpg'
#
data = my_read_excel("example_data/excel_example_PMC6230406.xlsx")
filename = 'example_over.jpg'
#data = my_read_excel("example_data/excel_example_PMC4163224.xlsx")

# 2) get t-statistics for both statistics types
tstats.c = tstats.p = NULL
if(is.null(data$continuous) == FALSE){
  tstats.c = t.stats.continuous(indata = data$continuous)
}
if(is.null(data$percents) == FALSE){
  tstats.p = t.stats.percents(indata = data$percents)
}
# 
tstats = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
  mutate(study = 1) # dummy study number

# 3) make simulated data
n.sims = 20
for (k in 1:n.sims){
  tstats.sim = make_sim(data)
  
  # get t-statistics for both statistics types
  tstats.c = tstats.p = NULL
  if(is.null(data$continuous) == FALSE){tstats.c = t.stats.continuous(indata = tstats.sim$continuous)} 
  if(is.null(data$percents) == FALSE){tstats.p = t.stats.percents(indata = tstats.sim$percents)}
  tstats.sim = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
    mutate(study = k+1) # dummy study number
  tstats = bind_rows(tstats, tstats.sim) # add to overall data
}

# 4) run Bayesian model
results = run_bayes_test(in_data = tstats)
save(results, file='example_results.RData') #

# 5) draw the CDF
## first create a median
# create all CDFs
average = filter(tstats, study > 1) %>% # just simulations
  group_by(study) %>%
  mutate(cdf = ecdf(t)(t)) %>% # CDF per study
  ungroup() 
# now calculate median CDF
cdf_median = group_by(average, study) %>%
  arrange(study, t) %>%
  mutate(r = rank(t, ties.method = 'first')) %>%
  group_by(r) %>%
  summarise(mid = median(t)) %>%
  ungroup() %>%
  mutate(e = r/n(),
         study = 1) # had to provide study number
# add first point of the CDF
cdf_first = filter(cdf_median, r==1) %>%
  mutate(e = 0, r=0)
cdf_median = bind_rows(cdf_first, cdf_median)

# move trial line to last
tstats = mutate(tstats, study = ifelse(study==1, 999, study))
colours = grey(runif(n = n.sims + 2, min=0.5, max=0.9)) # grey colours for simulations
colours[1] = 'dodgerblue' # colour for median
colours[n.sims + 2] = 'indianred1' # colour for trial
sizes = rep(1, n.sims + 2)
sizes[c(1,n.sims + 2)] = 2 # median and trial are larger
# plot
tplot = ggplot(data=tstats, aes(x=t, size=factor(study), colour=factor(study))) +
  theme_bw()+
  scale_size_manual(values = sizes)+
  scale_color_manual(values = colours)+
  stat_ecdf()+
  geom_step(data=cdf_median, aes(x=mid, y=e))+ # simulation average CDF
  xlab('t-statistic')+
  ylab('Cumulative density')+
  theme(legend.position = 'none',
        panel.grid.minor = element_blank())

# export examples
jpeg(filename, width=5, height=4, units='in', res=400, quality=100)
print(tplot)
dev.off()

