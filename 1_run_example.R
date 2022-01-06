# 1_run_example.R
# run the thermal clothing trial example
# January 2022
TeachingDemos::char2seed('swindon')
source('global.R')

# 1) get the data
data = my_read_excel("excel_example.xlsx")
#data = my_read_excel("excel_example_PMC4163224.xlsx")

# 2) get t-statistics for both statistics types
tstats.c = t.stats.continuous(indata = data$continuous)
tstats.p = t.stats.percents(indata = data$percents)
# 
tstats = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
  mutate(study = 1) # dummy study number

# 3) make simulated data
n.sims = 20
for (k in 1:n.sims){
  tstats.sim = make_sim(data)
  
  # get t-statistics for both statistics types
  tstats.c = t.stats.continuous(indata = tstats.sim$continuous) 
  tstats.p = t.stats.percents(indata = tstats.sim$percents)
  tstats.sim = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
    mutate(study = k+1) # dummy study number
  tstats = bind_rows(tstats, tstats.sim) # add to overall data
}

# 4) run Bayesian model
results = run_bayes_test(in_data = tstats)
save(results, file='example_results.RData')

# 5) draw the CDF
# move trial line to last
tstats = mutate(tstats, study = ifelse(study==1, 999, study))
colours = grey(runif(n = n.sims + 1, min=0.5, max=0.9)) # grey colours for simulations
colours[n.sims + 1] = 'indianred1' # 
sizes = rep(1, n.sims + 1)
sizes[n.sims + 1] = 2
tplot = ggplot(data=tstats, aes(x=t, size=factor(study), colour=factor(study))) +
  theme_bw()+
  scale_size_manual(values = sizes)+
  scale_color_manual(values = colours)+
  stat_ecdf()+
  xlab('t-statistic')+
  ylab('Cumulative density')+
  theme(legend.position = 'none',
        panel.grid.minor = element_blank())
# export
jpeg('CDF.jpg', width=5, height=4, units='in', res=400, quality=100)
print(tplot)
dev.off()

