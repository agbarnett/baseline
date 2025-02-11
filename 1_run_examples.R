# 1_run_examples.R
# run the thermal clothing trial and other examples
# January 2022, updated Feb 2025
source('global.R')

# 1) get the data, depending on the type
type = 'under'
type = 'over'
type = 'okay'

#
if(type == 'under_not_used'){
  data = my_read_excel("example_data/excel_example_PMC7988981.xlsx") # 
  filename = 'example_under.jpg'
  outfile = 'example_results_under.RData'
}

#
if(type == 'under'){
  data = my_read_excel("example_data/excel_example_retracted.xlsx") # under-dispersed, PMID: 9366938 -- used in ui.R 
  filename = 'example_under.jpg'
  outfile = 'example_results_under.RData'
}
#
if(type == 'okay'){
  data = my_read_excel("example_data/excel_example_PMID25851385.xlsx")
  filename = 'example_fine.jpg'
  outfile = 'example_results_fine.RData'
}

#
if(type == 'over'){
  data = my_read_excel("example_data/excel_example_PMC6230406.xlsx")
  filename = 'example_over.jpg'
  outfile = 'example_results_over.RData'
}
#
#data = my_read_excel("example_data/excel_example_PMC7065070.xlsx") # correlated

# 2) get t-statistics for both statistics types
tstats = make_stats_for_bayes_model(data$data) %>%
  mutate(study = 1) # dummy study number

# 3) run Bayesian model
results = run_bayes_test(in_data = tstats)
#save(results, file=outfile) # for paper supplement

# 4) make simulated data
n.sims = 20
all_stats = tstats # start with study
for (k in 1:n.sims){
  tstats.sim = make_sim(tstats) # make a single simulation
  
  # get t-statistics for both statistics types
  tstats.c = tstats.p = NULL
  if(any(tstats == 'continuous')){tstats.c = t.stats.continuous(indata = tstats.sim$continuous)} 
  if(any(tstats == 'percent')){tstats.p = t.stats.percents(indata = tstats.sim$percents)}
  tstats.sim = bind_rows(tstats.c, tstats.p, .id = 'statistic') %>%
    mutate(study = k+1) # dummy study number
  all_stats = bind_rows(all_stats, tstats.sim) # add to overall data
}

# 5) draw the CDF
## first create a mean
# create all CDFs
all_cdfs = filter(all_stats, study > 1) %>% # just simulations
  group_by(study) %>%
  mutate(cdf = ecdf(t)(t)) %>% # CDF per study
  ungroup() %>%
  select(study, t, cdf)
# now calculate mean CDF
cdf_mean = group_by(all_cdfs, cdf) %>%
  summarise(t = mean(t)) %>%
  ungroup() %>%
  mutate(study = 1) # had to provide study number, can be anything
# add first point of the CDF (where it hits the y-axis)
cdf_first = cdf_mean[1,] %>% 
  mutate(cdf = 0)
cdf_mean = bind_rows(cdf_first, cdf_mean)

# move trial line to last
all_stats = mutate(all_stats, study = ifelse(study==1, 999, study))
colours = grey(runif(n = n.sims + 1, min=0.5, max=0.9)) # grey colours for simulations
colours[n.sims + 1] = 'indianred1' # colour for trial
sizes = rep(0.5, n.sims + 1)
sizes[n.sims + 1] = 1 # trial line width is larger
# plot
tplot = ggplot(data=all_stats, aes(x=t, linewidth=factor(study,ordered = TRUE), colour=factor(study,ordered = TRUE))) +
  theme_bw()+
  scale_linewidth_manual(values = sizes)+
  scale_color_manual(values = colours)+
  stat_ecdf()+
  geom_step(data=cdf_mean, aes(x=t, y=cdf), linewidth=1,col='dodgerblue')+ # simulation average CDF
  xlab('t-statistic')+
  ylab('Cumulative density')+
  theme(legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="lightcyan")) # to avoid clash with grey of simulations

# export examples
jpeg(filename, width=5, height=4, units='in', res=400, quality=100)
print(tplot)
dev.off()

