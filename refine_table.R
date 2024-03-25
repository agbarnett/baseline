# refine_table.R
# called by get_pmcid_function.R
# the following is from 2_process_extracted_data.R; Added July 2023
# refining the table
# July 2023

## pre) exclude tables with 8 or more columns, as very likely to be subgroups, e.g., PMC2515903
col_count = summarise(results$table, max = max(column)) %>%
  filter(max >= 8)
# remove from data
if(nrow(col_count) > 0){
  results$reason = 'Likely subgroups in table'
  results$table = NULL
}

## 0) very small sample size
check = filter(results$table,
               sample_size <= 2) 
# remove from data
if(nrow(check) > 0){
  results$reason = 'Small sample size of 2 or under'
  results$table = NULL
  exit
}
remove(check)

## a) non-integer sample size
check = mutate(results$table,
               diff = as.integer(sample_size) != sample_size) %>%
  filter(diff == TRUE) 
# remove from data
if(nrow(check) > 0){
  results$reason = 'Non-integer sample size'
  results$table = NULL
  exit
}
remove(check)

## b) if numbers but dp2 > 0 then change to percent
check = filter(results$table,
               statistic == 'numbers',
               dp2 != 0) %>%
  group_by(row) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  filter(n > 0) %>% # any on the row
  select(row, n) %>%
  mutate(n = 99) %>% # change all n to arbitrary number
  unique()
# now change in data
results$table = left_join(results$table, check, by=c('row')) %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
remove(check)

## c) look for n/N (%) wrongly marked as continuous (must go before median switch)
check = filter(results$table, 
               statistic == 'continuous') %>% # must be continuous
  group_by(row) %>%
  mutate(p = round(100*(stat1/stat2)), # stat1 = n, stat2=N
         match = abs(p - stat3)) %>% # stat3 = %
  summarise(total = sum(match), 
            n=n()) %>% # will be missing for some, that's okay as we want all rows
  ungroup() %>%
  filter(total < 0.5) %>% # allow for little rounding error
  select(row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic), # update statistic for those that are a percent
    stat2 = ifelse(is.na(n) == FALSE, stat3, stat2)) %>% # shift % to second statistic
  select(-n) # no longer needed
remove(check)

## d) if statistic is 'continuous', but there are three stats, and stat1 is in stat2 to stat3 then change to 'median', e.g. PMC7362422
# removed "is.na(results$table$stat4) &", because PMC6537409 ...
# but this doesn't help with PMC7469713
# Examples where it does not work: "7149340" "7469713", these have age (SD), range; however we can avoid this by specifying stat4 empty
# but also examples where it does, e.g., PMC3502261
check = filter(results$table, 
               is.na(stat4), # see notes above, must be missing so just three stats
               statistic=='continuous') %>% # must be continuous
  group_by(row) %>%
  mutate(s1 = stat1 >= stat2 & !is.na(stat3),
         s2 = stat1 <= stat3 & !is.na(stat3),
         both = (s1 + s2)/2) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(both), n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # must apply to all rows
  select(row, n) %>%
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'median', statistic)) %>% # update statistic
  select(-n) # no longer needed
remove(check)

## e) if statistic is 'median', but stat3 is empty then change to continuous, e.g, PMC7315526 ...
check = filter(results$table, 
               statistic=='median') %>% # must be continuous
  group_by(row) %>%
  mutate(missing = is.na(stat3)) %>% # 
  summarise(total = sum(missing), n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # must apply to all rows
  select(row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(statistic = ifelse(is.na(n) == FALSE, 'continuous', statistic)) %>% # update statistic for those that are a median
  select(-n) # no longer needed
#changed_to_percent = check # used below
remove(check)

## f) if statistic is 'continuous', but stat2 is missing and dp1 is 0 then change to percent
# does change some means, for example, last row of PMC6966838
check = filter(results$table, 
               statistic=='continuous') %>% # must be continuous
  group_by(row) %>%
  mutate(d = as.numeric(dp1==0),
         m = as.numeric(is.na(stat2)),
         both = d + m) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(both), n=n()) %>%
  ungroup() %>%
  filter(total == 2*n) %>% # must apply to all rows
  select(row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
changed_to_percent = check # used below
remove(check)

## g) if statistic is `percent` with just one stat that has decimal places then assume it is a % and transform to n (%)
# works for rows 16 to 18 of PMC6966838
check = filter(results$table, 
               statistic == 'percent') %>% # 
  group_by(row) %>%
  mutate(m = as.numeric(is.na(stat2)),
         d = as.numeric(dp1 > 0)) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(m) + sum(d), n=n()) %>%
  ungroup() %>%
  filter(total == 2*n) %>% # must be every row
  select(row, n) %>%
  mutate(n = 99) %>% # change n to arbitrary number
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(stat1 = ifelse(is.na(n) == FALSE, round((stat1/100)*sample_size), stat1), # change % to n
         dp1 = ifelse(is.na(n) == FALSE, 0, dp1)) %>% # and update d.p.
  select(-n) # no longer needed
remove(check)

## h1a) check if format is % n instead of n %, and switch stats if it is, e.g., PMC5088368
check = filter(results$table, 
               !is.na(stat2), # cannot be missing
               statistic %in% c('number','percent')) %>% # 
  mutate(d = dp2 == 0, # numerator must be integer
         equal = stat1==stat2, # check that n % are the same
         pcheck = 100*(stat2/sample_size),
         pdiff = abs(stat1 - pcheck)) %>%
  group_by(row) %>%
  summarise(n = n(),
            sume = sum(equal),
            sumd = sum(d),
            total_diff = sum(pdiff)) %>%
  filter(total_diff < 1/n,
         sume < n, 
         sumd == n) %>% # must all be zero dp
  ungroup() %>%
  select(row) %>%
  mutate(n = 99) # dummy flag
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(temp = stat1, # temporary
         stat1 = ifelse(is.na(n) == FALSE, stat2, stat1), # change first stat
         stat2 = ifelse(is.na(n) == FALSE, temp, stat2), # change second stat
         stat3 = ifelse(is.na(n) == FALSE, NA, stat3), # blank third stat
         dp1 = ifelse(is.na(n) == FALSE, 0, dp1), # change first dp
         statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # and update d.p.
  select(-n, -temp) # no longer needed
remove(check)

## h1b) repeat previous for continuous, e.g., PMC7399917
check = filter(results$table, 
               !is.na(stat2), # cannot be missing
               is.na(stat3), # must be missing for continuous (different to above)
               statistic %in% c('continuous','median','ci')) %>% # 
  mutate(d = dp2 == 0, # numerator must be integer
         equal = stat1==stat2, # check that n % are the same (excude if they are)
         pcheck = 100*(stat2/sample_size),
         pdiff = abs(stat1 - pcheck)) %>%
  group_by(row) %>%
  summarise(n = n(),
            sume = sum(equal),
            sumd = sum(d),
            total_diff = sum(pdiff)) %>%
  filter(total_diff < 1/n,
         sume < n, 
         sumd == n) %>% # must all be zero dp
  ungroup() %>%
  select(row) %>%
  mutate(n = 99) # dummy flag
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(temp = stat1, # temporary
         stat1 = ifelse(is.na(n) == FALSE, stat2, stat1), # change first stat
         stat2 = ifelse(is.na(n) == FALSE, temp, stat2), # change second stat
         dp1 = ifelse(is.na(n) == FALSE, 0, dp1), # change first dp
         statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # and update d.p.
  select(-n, -temp) # no longer needed
remove(check)

## h2) exclude numbers/numerators in percents/numbers that are not integers
check = filter(results$table, 
               statistic %in% c('number','percent')) %>% # 
  group_by(row) %>%
  mutate(s1 = dp1 > 0) %>% # will be missing for some, that's okay as we want all rows
  summarise(total = sum(s1)) %>%
  ungroup() %>%
  filter(total > 0) %>% # can apply to any number in the row
  select(row, total) %>%
  mutate(total = 99) %>% # change to arbitrary number
  unique()
# now remove in data
results$table = left_join(results$table, check, by='row') %>%
  filter(is.na(total)) %>% # exclude all results from check
  select(-total)
remove(check)

## k) test that stat1/sample size is approx equal to stat/100 for 'percent'
# often picks up errors in papers, e.g., https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7106867/, 'B-ALL' percents
# or picks up where denominators change, e.g. PMC6966838
check = filter(results$table, statistic == 'percent') %>%
  mutate(p = 100*(stat1 / sample_size),
         diff = abs(stat2 - p)) %>%
  filter(diff > 1) %>% # could be more generous here? maybe 1.5?
  select(row) %>% # exclude all rows
  unique()
# now exclude ...
results$table = anti_join(results$table, check, by='row')

## l) if continuous, median or min max but stat1/N == stat2 then switch to percent
check = filter(results$table, 
               statistic %in% c('min_max','median','continuous')) %>% # must be non-percent
  mutate(p = 100*(stat1 / sample_size),
         diff = abs(stat2 - p), # percent difference, number on % scale
         match = diff <= 1.5 & dp1 == 0) %>% # some round error, allow 1.5%
  group_by(row) %>% # do at the row level
  summarise(total = sum(match), n=n()) %>% # will be missing for some, that's okay as we want all rows
  ungroup() %>%
  filter(total == n) %>% # must apply to all rows
  select(row) %>%
  mutate(n = 99) %>% # change all n to arbitrary number
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
remove(check)

## m) remove percent if stat1 > n (r > n)
impossible_percent = filter(results$table, statistic=='percent', stat1 > sample_size)
# if was changed to percent because of single stat then change back 
impossible_percent_back = left_join(impossible_percent, changed_to_percent, by='row') %>%
  select(row, n) %>%
  mutate(n = 99) %>% # change to arbitrary number
  unique()
if(nrow(impossible_percent_back) > 0){
  results$table = left_join(results$table, impossible_percent_back, by='row') %>%
    mutate(
      statistic = ifelse(is.na(n) == FALSE, 'continuous', statistic)) %>% # change back to continuous
    select(-n) # no longer needed
}
impossible_percent_exclude = anti_join(impossible_percent, changed_to_percent, by='row') %>%
  select(row) %>%
  unique()
# if not then exclude ...
if(nrow(impossible_percent_exclude) > 0){
  results$table = anti_join(results$table, impossible_percent_exclude, by='row')
}

## n) if statistic is percent but actual percent is outside 0 to 100
check = filter(results$table, 
               statistic=='percent') %>% # 
  filter(stat2 < 0 | stat2> 100)
nrow(check) # has been zero, so no need to change

## o) look for negative numerators for percents
check = filter(results$table, statistic=='percent', stat1 < 0) %>%
  select('row','column') %>%
  mutate(n = 1) # flag
# now remove in data
results$table = left_join(results$table, check, by=c('row','column')) %>%
  filter(is.na(n)) %>% # exclude all results from check
  select(-n)
remove(check)

## p) if continuous but stat2 (SD) is zero, e.g., PMC8096923, then add small constant
check = filter(results$table, 
               statistic=='continuous', # must be continuous
               stat2 ==0) %>%
  select(row, column) %>%
  mutate(n = 1) # change flag
# now change in data
results$table = left_join(results$table, check, by=c('row','column')) %>%
  mutate(
    stat2 = ifelse(is.na(n) == FALSE, (stat1+0.0001)*0.01, stat2)) %>% # update zero SD to 1% of mean, add small constant to mean in case mean is zero
  select(-n) # no longer needed
remove(check)

#### confidence intervals
## q1) if CI, but stat3 (upper interval) is less than stat2 (lower interval) then delete
check = filter(results$table, 
               statistic=='ci', # must be ci
               !is.na(stat2),
               !is.na(stat3),
               stat3 < stat2) %>%
  select(row, column) %>%
  mutate(n = 1) %>% # arbitrary non-zero flag
  unique()
# now remove in data
results$table = left_join(results$table, check, by=c('row','column')) %>%
  filter(is.na(n)) %>% # exclude all results from check
  select(-n)
remove(check)

## q2) if CI, but no stat3 or stat1 not in stat2 to stat3 then change to continuous
check = filter(results$table, 
               statistic=='ci', # must be ci
               is.na(stat3) | stat1 < stat2 | stat1 > stat3) %>%
  select(row) %>%
  mutate(n = 1) %>% # change flag
  select(row, n) %>%
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'continuous', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
remove(check)

## q3) calculate SD for confidence intervals
keep = filter(results$table, statistic != 'ci') # does not need changing
redone = filter(results$table, statistic == 'ci') %>%
  mutate(z = qnorm(0.975), # assume 95% CI
         stat2 = sqrt(sample_size)*(stat3 - stat2)/(2*z),
         stat3 = NA) %>%
  select(-z)
# put back together
results$table = bind_rows(redone, keep)

## r) if original statistic is a percent, but stat1 in stat2 to stat3 then change to median, e.g., PMC3163659; must be all rows
check = filter(results$table, 
               statistic == 'percent') %>% # must be percent
  mutate(s1 = stat1 >= stat2 & stat1 <= stat3 & !is.na(stat3)) %>%
  group_by(row) %>%
  summarise(total = sum(s1), n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # Must be all rows
  select(row, n) %>%
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'median', statistic)) %>% # update statistic for those that are a percent
  select(-n) # no longer needed
remove(check)

## z) if continuous but no stat2 and stat1 is < sample_size and stat1>0 and dp1=0 then switch to percent
check = filter(results$table, 
               statistic %in% c('median','continuous')) %>% # 
  mutate(s1 = dp1==0 & stat1 <= sample_size & stat1 >= 0 & is.na(stat2)) %>%
  group_by(row) %>%
  summarise(total = sum(s1), 
            n=n()) %>%
  ungroup() %>%
  filter(total == n) %>% # Must be all rows
  select(row, n) %>%
  unique()
# now change in data
results$table = left_join(results$table, check, by='row') %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic 
  select(-n) # no longer needed
remove(check)

## w) if continuous but stat2 is negative then remove
check = filter(results$table, 
               !is.na(stat2),
               stat2 <0,
               statistic %in% c('continuous')) %>% # 
  select(row) %>%
  unique() %>%
  mutate(n = 99) # dummy flag
# remove from data
results$table = anti_join(results$table, check, by='row')
# now remove in data 
results$table = left_join(results$table, check, by='row') %>%
  mutate(
    statistic = ifelse(is.na(n) == FALSE, 'percent', statistic)) %>% # update statistic 
  select(-n) # no longer needed
remove(check)

## b2) if numbers but stat1 is actually %
# error with column here
check = filter(results$table,
               statistic == 'numbers',
               dp1 > 0) %>% # not an integer
  mutate(stat_replace = round(sample_size * stat1/100)) %>% # re-calculate
  select(row, column, stat_replace) %>%
  unique()
# now change in data (merged by row and column)
results$table = left_join(results$table, check, by=c('row','column')) %>%
  mutate(
    dp1 = ifelse(is.na(stat_replace) == FALSE, 0, dp1),
    stat1 = ifelse(is.na(stat_replace) == FALSE, stat_replace, stat1)) %>% # update statistic
  select(-stat_replace) # no longer needed
remove(check)

## i) exclude if there is no second statistic for continuous; percents can live with n (just stat1); e.g., PMC6435872
# not done by row but for any cell
index = is.na(results$table$stat2) & (results$table$statistic %in% c('continuous','median','min_max'))
if(sum(index) > 0){
  results$table = results$table[!index,]
}

## excluded if just one result per row (nothing to compare with) (run this last)
n_per_row = group_by(results$table, row) %>%
  tally() %>%
  ungroup() %>%
  filter(n==1) 
# now exclude ...
if(nrow(n_per_row) > 0){
  results$table = anti_join(results$table, n_per_row, by='row')
}


