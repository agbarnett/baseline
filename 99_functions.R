# 99_functions.R
# functions for shiny app to test baseline tables
# January 2025

# from https://stackoverflow.com/questions/24129124/how-to-determine-if-a-character-vector-is-a-valid-numeric-or-integer-vector, used below
can.be.numeric <- function(x) {
  stopifnot(is.atomic(x) || is.list(x)) # check if x is a vector
  numNAs <- sum(is.na(x))
  numNAs_new <- suppressWarnings(sum(is.na(as.numeric(x))))
  if(is.na(x)){numNAs_new = FALSE} # False if number is missing
  return(numNAs_new == numNAs)
}

# read and process Excel file (version for multiple columns)
# must keep row names
my_read_excel = function(input_file){
  warning = NULL
  
  # read in the file and assign dummy names (two groups only so far)
  raw = read_excel(path = input_file, sheet=1, col_names = FALSE, skip=2)
  names(raw)[1:13] = paste('v', 1:13, sep='') # just first thirteen columns (up to four groups)
  # calculate the number of groups
  index = which(raw$v1 == 'Name') # find name columns
  g1 = sum(raw[index[1],] == 'Mean', na.rm = TRUE)
  g2 = sum(raw[index[2],] == 'N', na.rm = TRUE)
  if(g1 != g2){warning = paste(warning, 'Number of groups for continuous and categorical data do not match.\n', sep='')}
  n_groups = g1
  
  # find percent and continuous
  row_cont = which(str_detect(string=raw$v1, pattern='Continuous variables'))[1]
  row_percent = which(str_detect(string=raw$v1, pattern='Numbers or percents'))[1]
  # check there's some continuous and percent data
  index = which(raw$v3=='Mean')[1]
  any.cont = can.be.numeric(raw$v3[index+1]) # is the next cell after the Mean a number?
  index = which(raw$v3=='N')[1]
  any.percent = can.be.numeric(raw$v3[index+1]) # is the next cell after the Mean a number?
  
  # extract continuous summary stats
  continuous = percents = NULL
  max.percent = 0
  if(any.cont == TRUE){
    continuous = raw[(row_cont+2):(row_percent-1),] %>% # extract rows
      mutate_at(vars(!matches("^v1$")), as.numeric) %>% # change every variable but first column to a number 
      filter_all(all_vars(!is.na(.)))  # all data must be available
    # names based on number of groups
    cont_names = paste(rep(c('n','m','sd'),n_groups), rep(1:n_groups,each=3), sep='')
    names(continuous) = c('v1', cont_names)
    continuous = mutate(continuous, row = 1:n()) # to keep ordering from sheet
  }
  
  # extract percent summary stats
  if(any.percent == TRUE){
    p_names = paste('v',2:((n_groups*2)+1), sep='') # need this because continuous variables above include more columns
    percents = raw[(row_percent+2):nrow(raw),] %>%
      mutate_at(vars(!matches("^v1$")), as.numeric) %>% # change every variable but first column to a number 
      filter_at(vars(all_of(p_names)), all_vars(!is.na(.))) # all data must be available
    # names based on number of groups
    cat_names = paste(rep(c('n','N'),n_groups), rep(1:n_groups,each=2), sep='')
    names(percents) = c('v1', cat_names)
    percents = select(percents, matches('^v|^n|^N')) # remove blank columns
    percents = mutate(percents, row = 1:n()) # to keep ordering from sheet
  }
  
  # convert percents to long format
  percents_long = NULL
  if(!is.null(percents)){
    col1 = select(percents, 'v1', 'row', starts_with('n', ignore.case = FALSE)) %>%
      pivot_longer(-c('v1','row'), values_to = 'stat1') %>%
      mutate(column = as.numeric(str_remove(name,'n'))) %>%
      select(-name)
    if(any(col1$stat1 - round(col1$stat1) !=0)){warning = paste(warning, 'Non-integer numerators.\n', sep='')}
    col2 = select(percents, 'v1', 'row', starts_with('N', ignore.case = FALSE)) %>%
      pivot_longer(-c('v1','row'), values_to = 'sample_size') %>%
      mutate(column = as.numeric(str_remove(name,'N'))) %>%
      select(-name)
    percents_long = full_join(col1, col2, by=c('v1','row','column')) %>%
      mutate(stat2 = 100*stat1/sample_size,
             statistic = 'percent') 
    max.percent = max(percents_long$row) # highest row number in percentages
  }
  
  # convert continuous to long format
  continuous_long = NULL
  if(!is.null(continuous)){
    ccol1 = select(continuous, 'v1', 'row', starts_with('n', ignore.case = FALSE)) %>%
      pivot_longer(-c('v1','row'), values_to = 'sample_size') %>%
      mutate(column = as.numeric(str_remove(name,'n'))) %>%
      select(-name)
    ccol2 = select(continuous, 'v1', 'row', starts_with('m', ignore.case = FALSE)) %>%
      pivot_longer(-c('v1','row'), values_to = 'stat1') %>%
      mutate(column = as.numeric(str_remove(name,'m'))) %>%
      select(-name)
    ccol3 = select(continuous, 'v1', 'row', starts_with('sd', ignore.case = FALSE)) %>%
      pivot_longer(-c('v1','row'), values_to = 'stat2') %>%
      mutate(column = as.numeric(str_remove(name,'sd'))) %>%
      select(-name)
    continuous_long = full_join(ccol1, full_join(ccol2, ccol3, by=c('v1','row','column')), by=c('v1','row','column')) %>%
      mutate(row = row + max.percent, # add to make sure rows do not clash with percents
             statistic = 'continuous')
    # warnings
    if(any(ccol3$stat2<=0)){warning = paste(warning, 'Negative or zero standard deviations.\n', sep='')}
  }
  
  # concatenate
  long = bind_rows(percents_long, continuous_long) %>%
    mutate(pmcid = 99) # dummy for later function
  # warnings
  if(any(long$sample_size - round(long$sample_size) !=0)){warning = cat(warning, 'Non-integer sample sizes.\n')}
  
  # return
  data = list()
  data$data = long
  data$reason = warning # called reason to match other structure
  return(data)
}


## Make simulated data that copies the input data (summary statistics) but follows the null hypothesis of good randomisation
make_sim = function(indata){
  
  simp = simc = NULL
  # simulate continuous
  if(any(indata$statistic == 'continuous')){
    simc = filter(indata, statistic == 'continuous') %>%
      #mutate(row=1:n()) %>%
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
      select(row, n1, m1, sd1, n2, m2, sd2) # removed v1
  }
  
  # simulate percentages
  if(any(indata$statistic == 'percent')){
    simp = filter(indata, statistic == 'percent') %>%
      #mutate(row=1:n()) %>%
      group_by(row) %>%
      mutate(p = (n1+n2) / (N1 + N2),
             n1 = rbinom(n = 1, p=p, size=N1),
             n2 = rbinom(n = 1, p=p, size=N2)) %>%
      ungroup() %>%
      select(row, row, n1, N1, n2, N2)
  }
  
  sim_data = list()
  sim_data$continuous = simc
  sim_data$percents = simp
  return(sim_data)
}


## t-test from summary stats, from https://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha
# for two groups
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



## create t-statistics for continuous summary stats (assumes 2 groups)
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
  analysis_ready = bind_rows(l1, l2) %>%
    mutate(sd = ifelse(sd==0, 0.001, sd)) # avoid zero SD
  
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




## extract baseline tables, simplified version, removed pvalues_in_table
baseline_table = function(webpage, # paper tables in xml format 
                          pmcid ,
                          table_number = 1, 
                          footnote,
                          weight = 2) # weight feeds into statistics detect function
{
  
  # maximum number of tables - probably no longer needed
  max_table = length(webpage %>% html_elements("table-wrap"))
  if(table_number > max_table){ # table likely in appendix or graphic
    to.return = list()
    to.return$reason = 'Table in appendix or table is graphic'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  # extract and tidy footnote
  table_footnote = webpage %>% 
    xml_nodes("table-wrap") %>% 
    xml_nodes('tfoot') %>% 
    xml_text()
  footnote = paste(footnote, table_footnote)
  footnote = str_squish(str_replace_all(string=tolower(footnote), pattern="[^0-9|a-z|=|<|>|%| ]", ' '))  # remove all special characters to make matching below easier
  
  # extract baseline table 
  table1 <- webpage %>%
    xml_nodes("table-wrap") %>%
    .[table_number]  # table number for baseline table
  # check for graphics
  graphic = str_detect(as.character(table1), pattern='<inline-graphic')
  if(graphic == TRUE){ # 
    to.return = list()
    to.return$reason = 'Table is graphic'
    to.return$table = NULL
    return(to.return) # stop here
  }
  # remove footnote within table
  xml_find_all(table1, ".//tfoot") %>% xml_remove()    
  #
  table1 =  xml_nodes(table1, "table") %>% # get first table in wrap
    html_table(header = TRUE, fill = TRUE) # include table headers
  table1 = table1[[1]] # to data frame
  
  # if null table then stop here
  if(nrow(table1) == 0){ # table likely a graphic
    to.return = list()
    to.return$reason = 'Table is graphic'
    to.return$table = NULL
    return(to.return) # stop here
  }
  if(nrow(table1) == 1){ # Badly formatted, all results in one row
    to.return = list()
    to.return$reason = 'Complex format'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  rename_tab = function(x){setNames(x, LETTERS[1:ncol(x)]); return(x)}
  
  ## remove blank rows
  to_test = table1 # work on simplified version
  to_test = setNames(to_test, LETTERS[1:ncol(to_test)])
  to_test = mutate_all(to_test, removeunicode) %>% 
    mutate_all(str_squish)
  index = which(rowSums(to_test=='' | is.na(to_test)) == ncol(to_test)) # empty or missing
  if(length(index)>0){
    table1 = table1[-index,]
  }
  ## remove rows that are almost identical to above
  table1 = remove_duplicate_rows(table1)
  
  ## remove odd characters and change to lower case - helps with searching below
  cnames = tolower(names(table1))
  cnames = gsub(cnames, pattern='(?=\\(\\d{1,}\\))', replacement='n=', perl=TRUE) # replace '(numbers)` with '(n=numbers)'
  cnames = str_squish(str_replace_all(cnames, pattern = '[^=|a-z|0-9|%| ]', replacement=' ')) # just keep letters, numbers, equals
  names(table1) = cnames
  cnames[is.na(cnames)] = ''
  if(any(cnames == '')){ # cant have null table numbers
    index = names(table1) == ''
    index[is.na(index)] = TRUE
    names(table1)[index] = paste('c',1:sum(index),sep = '')
  }
  
  ## scan for follow-up data
  any_follow_header = any(str_detect(string=cnames, pattern=fu_pattern))
  follow_row = str_detect(string=table1[1,], pattern=fu_pattern)
  follow_row[is.na(follow_row)] = FALSE
  if(any_follow_header == TRUE | any(follow_row) == TRUE){
    to.return = list() 
    to.return$reason = 'Follow-up results in baseline table'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  # any duplicate table names? 
  cnames = names(table1)
  # if both `variable` then just change one
  if(any(tolower(cnames)=='variable')){
    index = which(tolower(cnames)=='variable')[1]
    cnames[index] = 'VARIABLES'
    names(table1) = cnames # replace
  }
  # 
  if(any(duplicated(cnames)) == TRUE){
    # if multiple columns are 'variable' then likely to be a terrible layout
    check = sum(tolower(cnames) == 'variable')
    if(check > 0){
      to.return = list()
      to.return$reason = 'Difficult layout'
      to.return$table = NULL
      return(to.return) # bail out here, will be impossible to unpick
    }
    # avoid repeat names by adding letters to start (just for duplicated)
    dindex = duplicated(names(table1))
    dcount = sum(dindex)
    these_letters = LETTERS[1:dcount]
    names(table1)[dindex] =  paste(these_letters, names(table1)[dindex], sep = '') 
  }
  
  ## transpose badly formatted tables here (where results are in rows instead of columns)
  # if more columns than rows then assume transposed ...
  rows_columns = nrow(table1) < ncol(table1)
  # ... and if no 'age' or 'gender' in column labels (using first two columns)
  common_labels1 = any(str_detect(pattern='age|height|weight|gender|sex', tolower(table1[,1]))) # first column
  common_labels1[is.na(common_labels1)] = FALSE
  common_labels2 = any(str_detect(pattern='age|height|weight|gender|sex', tolower(table1[,2]))) # second column
  common_labels2[is.na(common_labels2)] = FALSE
  common_labels = any(common_labels1, common_labels2)
  # ... and check that `n=[0-9]` not in column header, as this indicates a column header
  numbers_in_header = any(str_detect(pattern='n\\s?=\\s?[0-9]', names(table1)))
  # or if common labels are in the table column headings, e.g., PMC5998772
  common_labels_headings = any(str_detect(pattern='\\bage\\b|\\bheight\\b|\\bweight\\b|\\bgender\\b|sex\\b', tolower(names(table1))[-1])) # do not look in left-most column, as this can often be age group
  to_transpose =  (rows_columns == TRUE & common_labels==FALSE & numbers_in_header==FALSE) | (common_labels_headings == TRUE & nrow(table1) < 10) # added nrow here because of tables without headers, PMC5731727
  if(to_transpose == TRUE){ 
    # only if there's no age or gender in column labels, e.g., PMC7230691
    any_age_sex = str_detect(string=as.character(table1[,1]), pattern='sex|age')
    if(any(any_age_sex)==FALSE){
      table1 = t(table1)
      h = table1[1,]
      no_name = h==''
      no_name[is.na(no_name)] = FALSE
      if(any(no_name)){ # if names missing then replace with letters
        h[no_name] = LETTERS[1:sum(no_name)]
      }
      table1 = data.frame(table1[-1,]) # remove top row
      names(table1) = h
      # add labels as first column
      table1 = bind_cols(row.names(table1), table1)
      names(table1)[1] = 'label'
    }
  }
  # neaten first column with labels
  table1 = mutate_at(table1, str_squish, .vars=1) # first column
  
  ## if sample size in first two rows then add to header, using sample words matching only
  # or if any rows are the total
  to_check = tolower(data.frame(table1)[,1])
  sample1 = str_detect(string=to_check, pattern=paste(sample_patterns, collapse='|'))
  sample1[is.na(sample1)] = FALSE
  sample2 = str_detect(string=to_check, pattern=paste(sample_patterns, collapse='|'))
  sample2[is.na(sample2)] = FALSE
  sample3 = which(str_detect(string=to_check, pattern=paste(sample_words, collapse = '|')))[1]
  match_prop1 = sum(sample1[2:length(sample1)]) / (length(sample1) - 1)
  match_prop2 = sum(sample2[2:length(sample2)]) / (length(sample2) - 1)
  if(is.na(sample3) == FALSE | sample1[1] == TRUE | sample2[1] == TRUE  | match_prop1 > 0.5 | match_prop2 > 0.5){ # if left-most label or if enough column labels
    if(is.na(sample3) == FALSE){ # if there's a row
      n_row = sample3
    }
    if(is.na(sample3) == TRUE){
      n_row = 2 # start on second row ...
      if(sample1[1]==TRUE | match_prop1 > 0.5){n_row = 1} # ... switch to first
    }
    # create labels to add to header
    make_labels_from = table1[n_row, ]
    index = str_detect(string=make_labels_from, pattern='%|percent')
    index[is.na(index)] = FALSE
    if(any(index) == TRUE){
      make_labels_from[index] = '' # do not add any percents as n's
    }
    labels = paste('n=', make_labels_from, sep='')
    labels[labels=='n='] = '' # remove empty labels
    # remove percentages, see PMC7164253
    this_percent = paste(c('[a-z]?%','[a-z]?percent','[a-z]?percentage'), collapse='|')
    index = str_detect(string=names(table1), pattern = this_percent)
    if(any(index)){
      labels[index] = ''
    }
    #
    names(table1) = paste(names(table1), labels)
    # remove first or second row
    table1 = table1[-n_row,] 
  }
  
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  # combine first two to three columns if they are both mostly text (double label column)
  previous = ncol(table1)
  table1 = combine_cols(table1) # first two columns ...
  if(ncol(table1)< previous){
    table1 = combine_cols(table1) # .. and try third column
  }
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  ## if "numbers (numbers - numbers)" in cells then flag median; do before plus/minus below
  #table1 = mutate_all(table1, flag_median_function) # caused occasional errors so changed to below ...
  table1 = mutate(table1, across(everything(), flag_median_function))
  
  ## combine columns if there are columns of numbers, followed by columns in brackets
  columns = as.matrix(table1)[,-1] # without first column
  bcounts = colSums(apply(columns, FUN=str_count, MARGIN=2, pattern='\\('),na.rm = TRUE)
  proportions = bcounts / nrow(columns)
  n.cols = ncol(columns)
  no_brackets = proportions < 0.05 | bcounts<=1 # changed March 2023 due to 
  with_brackets = proportions > 0.75
  index = which(no_brackets[1:(n.cols-1)] + with_brackets[2:n.cols] == 2) # any columns of non-brackets followed by brackets
  if(length(index) > 0){ # combine
    index = as.numeric(index) + 1 + 1 # plus one because of missing first column; plus another to move to right column
    tnames = names(table1)
    for (i in rev(index)){ # work backwards (from right to left)
      newname = paste(tnames[i-1], tnames[i])
      table1 = unite(table1, newname, (i-1):i, sep=' ') 
      names(table1)[names(table1) == 'newname'] = newname # rename column
    }
  }
  
  ## if both plus/minus and `n (%)` formats (but without percent in text) then add % to help stats detector ...
  # ... or if percent mentioned in footnote
  any_plus_minus = any(str_detect(as.character(table1), pattern=paste(plus_minus, collapse='|')))
  any_count_in_footnote = any(str_detect(footnote, pattern=percent_specific))
  if(any_plus_minus == TRUE | any_count_in_footnote ==TRUE){ # some plus/minus or percent used ...
    any_percent = any(str_detect(as.character(table1), pattern='%'))
    if(any_percent == FALSE){ # ... with no percents
      # to fix to avoid median :
      any_num_bracket = any(str_detect(as.character(table1), pattern='[0-9] \\([0-9]'))
      if(any_num_bracket == TRUE){ # then add % to these cells
        # table1 = mutate_all(table1, add_percent_function)# caused occasional errors so changed to below ...
        table1 = mutate(table1, across(everything(), add_percent_function))
      }
    }
  }
  
  # clean up table text and column headers:
  colnames(table1) = tolower(removeunicode(colnames(table1))) # remove unicode from headers too
  table1 = mutate_all(table1, tolower) %>%
    mutate_all(add_space_function) %>% # add space before and after brackets to stop numbers getting compressed
    mutate_all(removeunicode) %>% # 
    mutate_all(string_remove_function)
  
  ## drop any columns that are the total
  # also drop any columns that are labelled 'men' or 'women' as these are subgroups not randomised groups
  text_to_check = tolower(names(table1))
  text_to_check = str_squish(str_remove_all(text_to_check, 'n=|n =|[0-9]')) # remove sample size
  to_drop_header1 = str_detect(string=text_to_check, total_words) # see 0_patterns.R for words
  to_drop_header2 = str_detect(string=text_to_check, total_words_alt) # because of letters added to column headers to avoid name clashes
  to_drop_header1[is.na(to_drop_header1)] = FALSE
  to_drop_header2[is.na(to_drop_header2)] = FALSE
  to_drop_header = as.logical(pmax(to_drop_header1, to_drop_header2))
  # also check first row - no need for total_words_alt
  text_to_check = tolower(table1[1,])
  text_to_check = str_squish(str_remove_all(text_to_check, 'n=|n =|[0-9]')) # remove sample size
  text_to_check[is.na(text_to_check)] ='' # replace missing
  to_drop_first = str_detect(text_to_check, total_words) # see 0_patterns.R for words
  to_drop_first[is.na(to_drop_first)] = FALSE
  to_drop = as.logical(pmax(to_drop_header, to_drop_first))
  to_drop[1] = FALSE # never drop first column
  if(any(to_drop) == TRUE){
    col_names = names(table1)[to_drop]
    table1 = select(table1, -all_of(col_names))
  }
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  ## drop any columns that are only text (e.g., list of tests used)
  number_counts = table1 %>% mutate(across(everything(), ~str_count(string=.x, pattern='[0-9]'))) %>%
    colSums()
  to_drop = which(number_counts == 0)
  to_drop = to_drop[to_drop !=1] # never the first column
  # check if the column is p-value, e.g., PMC7574843
  to_keep = which(str_detect(names(table1), pattern=pval_pattern))
  if(length(to_keep)>0){to_drop = to_drop[to_drop != to_keep]}
  if(length(to_drop)>0){table1 = table1[, -to_drop]}
  
  ## exclude papers that are using a pre-post comparison as p-values may be valid
  test_cols1 = str_detect(string = names(table1), pattern=prepost_pattern) # check names
  test_cols2 = str_detect(string = table1[1,], pattern=prepost_pattern) # check first row
  test_cols = as.logical(pmax(test_cols1, test_cols2))
  test_cols[is.na(test_cols)] = FALSE
  if(any(test_cols) == TRUE){
    to.return = list() 
    to.return$reason = 'Pre-post comparison'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  ## remove columns that are test statistics,  
  test_cols1 = str_detect(string = names(table1), pattern=test_pattern) # check names
  test_cols2 = str_detect(string = table1[1,], pattern=test_pattern) # check first row
  test_cols = as.logical(pmax(test_cols1, test_cols2))
  test_cols[is.na(test_cols)] = FALSE
  test_cols[1] = FALSE # never remove the first column
  stats_column = NULL
  if(any(test_cols)){
    col_names = names(table1)[test_cols]
    stats_column = select(table1, all_of(col_names)) # keep for later, in case p-values are in here
    table1 = select(table1, -all_of(col_names))
  }
  
  ## remove columns that are just the range, e.g. PMC8073435  ...
  plus = paste(c(min_max_pattern_whole, '^quintile', '^reference.range$','^mean.difference$'), collapse='|') # ... and add quintile, PMC6761647, reference range PMC7281967, mean difference PMC3953023
  test_cols1 = str_detect(string = tolower(names(table1)), pattern=plus) # check names
  test_cols2 = str_detect(string = tolower(table1[1,]), pattern=plus) # check first row
  test_cols = as.logical(pmax(test_cols1, test_cols2))
  test_cols[is.na(test_cols)] = FALSE
  if(any(test_cols)){
    col_names = names(table1)[test_cols]
    table1 = select(table1, -all_of(col_names))
  }
  ## stop if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  ## combine statistics in neighbouring columns
  table1 = combine_columns(table1, stat1=c('mean','m'), stat2='sd') # assume single `m` for mean
  if(ncol(table1) <= 2){return(stop_one_column())} # stop if 1 column
  table1 = combine_columns(table1, stat1=c('mean','m'), stat2='95\\%\\s?c.?i.?') # assume single `m` for mean
  if(ncol(table1) <= 2){return(stop_one_column())} # stop if 1 column
  table1 = combine_columns(table1, stat1=c('median'), stat2=c('iqr','inter-quartile range','range')) #
  if(ncol(table1) <= 2){return(stop_one_column())} # stop if 1 column
  table1 = combine_columns(table1, stat1=c('n','number'), stat2=c('\\%','percent','percentage'))
  if(ncol(table1) <= 2){return(stop_one_column())}# stop if 1 column
  table1 = combine_columns(table1, stat1=c('\\%','percent'), stat2=c('n','number'), reverse=TRUE)
  if(ncol(table1) <= 2){return(stop_one_column())}# stop if 1 column
  table1 = combine_columns(table1, stat1=c('\\%','percent'), stat2=c('9.\\%CI','9.% CI','9. % CI'), reverse=FALSE)
  if(ncol(table1) <= 2){return(stop_one_column())}# stop if 1 column
  
  ## combine neighbouring rows that have `mean` and `sd`, or median and range
  table1 = combine_rows(table1, stat1=c('mean'), stat2=c('sd','standard deviation','95\\%\\s?c.?i.?')) 
  table1 = combine_rows(table1, stat1=c('median'), stat2=c('iqr','range')) 
  
  ## remove columns that are purely sample size (and put text in header) 
  test_cols1 = str_detect(string = str_squish(names(table1)), pattern=c('^n$|^n/n$|^[a-z]n$')) # check names, last pattern is because i add random letter to avoid duplicate names
  test_cols2 = str_detect(string = str_squish(table1[1,]), pattern=c('^n$|^n/n$')) # check first row
  test_cols1[is.na(test_cols1)] = FALSE
  test_cols2[is.na(test_cols2)] = FALSE
  test_cols = as.logical(pmax(test_cols1, test_cols2)) # combine rows
  if(any(test_cols) & length(test_cols)>2){ # if 2 or fewer columns then will be removed below
    # check the columns are just numbers
    M = as.matrix(table1[,test_cols]) # need to transform into matrix
    M[is.na(M)] = ''
    nums = matrix(data = rep(1:ncol(M), nrow(M)), ncol = ncol(M), nrow = nrow(M), byrow = TRUE) # make matrix of column numbers
    vector_nums = as.vector(as.matrix(nums))
    check_nums = str_detect(pattern='^[0-9]+$|^[0-9]+/[0-9]+$', M) # search for just numbers in cell
    denominators = colSums(M!='') - colSums(M=='n') # exclude empty cells from denominator (also avoid counting 'n')
    prop = table(vector_nums[check_nums]) / denominators
    n_counts = prop > 0.85 # rows are more than 85% numbers
    n_counts[is.na(n_counts)] = FALSE
    if(any(n_counts) == TRUE){ # only proceed if 85% met
      ## put numbers in header for sample size detection, only if there's not any `n=` in header already
      if(!any(str_detect(pattern='n=|n =',names(table1)))){
        # get average sample size
        numbers = matrix(as.numeric(M), ncol = ncol(M)) # ignore NA warnings
        nmean = round(colMeans(numbers, na.rm=TRUE))
        # find first row with numbers
        n_text= paste('n=', nmean, sep='') # make detectable text
        index = which(test_cols)+1 # assume it is next column along
        index = index[index<=ncol(table1)]
        names(table1)[index] = paste(names(table1)[index], n_text)
      }
      # remove columns from table
      table1 = table1[, -which(test_cols)]
    }
  }
  
  ## stop (again) if just one column
  if(ncol(table1) <= 2){return(stop_one_column())}
  
  ## remove rows with ratios, e.g., 7954267, can't process this statistic
  # added other tricky statistics with unusual formatting
  # also remove total rows
  stats_to_remove = c('ratio','snellen equivalent','^overall\\s?$','^total\\s?$','geometric mean')
  to_remove = paste('\\b', paste(stats_to_remove, collapse='\\b|\\b'), '\\b', sep='')
  ratio = str_detect(string=as.vector(data.frame(table1)[,1]), pattern=to_remove)
  ratio[is.na(ratio)] = FALSE
  ratio_ok = str_detect(string=as.vector(data.frame(table1)[,1]), pattern='waist.to.hip.ratio|waist.to.height.ratio') # these are okay
  ratio_ok[is.na(ratio_ok)] = FALSE
  ratio[ratio_ok] = FALSE # remove 'good' ratios from this exclusion
  if(any(ratio) == TRUE){
    table1 = table1[!ratio, ]
  }
  
  ## remove repeated labels across rows
  ncol = ncol(table1)
  repeats = mutate(data.frame(table1), row = 1:n()) %>%
    reshape::melt(id.vars = 'row') %>%
    group_by(row) %>%
    select(-variable) %>%
    unique() %>%
    tally() %>%
    filter(n ==1 ) %>% # all rows the same
    pull(row)
  if(length(repeats) > 0){
    table1[repeats, 2:ncol] = '' # blank repeat cells
  }
  
  ## what row do numbers/stats start on?
  numbers_start = numbers_start_function(table1)
  
  ## Additional check for labels that are "N"
  any_n = table1[,1]=='n'
  any_n[is.na(any_n)] = FALSE
  if(any(any_n)){
    index = which(table1[,1]=='n')
    #numbers_start = index + 1 # turned off did not work for PMC7005671
    table1[index,] = paste('n=', table1[index,], sep='') # add `n=` for later detection
  }
  # now add header
  table1 = mutate(table1, header = FALSE)
  if(length(numbers_start) > 0){ table1$header[1:nrow(table1) < numbers_start] = TRUE }
  # create versions without header and only header
  no_header = filter(table1, header == FALSE) %>% # remove header row(s)
    select(-header) # remove header column
  table_header = filter(table1, header == TRUE) %>%
    select(-header)
  
  ## remove rows that are sample size updates
  nums = matrix(data = rep(1:nrow(no_header), ncol(no_header)), ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  vector_nums = as.vector(as.matrix(nums))
  table_text = str_squish(as.vector(as.matrix(no_header)))
  any_n = str_detect(table_text, pattern='n = [0-9]|n=[0-9]|n =[0-9]|n= [0-9]')
  rows_with_n = vector_nums[any_n]
  tab = table(rows_with_n)
  index = which(tab == ncol(no_header) - 1) # must be all columns
  rows_with_n = as.numeric(names(index))
  no_header = no_header[1:nrow(no_header) %in% rows_with_n == FALSE, ] # remove rows from table
  
  ## find rows in the table that are just text as these are likely header rows (so just keep rows with some numbers)
  nums = matrix(data = rep(1:nrow(no_header), ncol(no_header)), ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  vector_nums = as.vector(as.matrix(nums))
  table_text = str_squish(as.vector(as.matrix(no_header)))
  table_text[is.na(table_text)] = ''
  # proportion of numbers divided by characters (just exclude spaces)
  prop_numbers = str_count(table_text, pattern = '[0-9]') / str_count(table_text, pattern = '[^\\s]')
  prop_numbers[is.na(prop_numbers)] = 0
  matrix_prop = matrix(prop_numbers, ncol=ncol(no_header)) > 0.4 # more than 40% numbers
  rows_with_numbers = which(rowSums(matrix_prop) > 0)
  
  ##### detect use of statistics for each row ####
  ## split table if there are multiple header rows (usually a second row mid-way down table)
  no_header = mutate(no_header, rrr = 1:n())
  table_header = mutate(table_header, dummy=NA) # add dummy for rrr
  #
  diffs = diff(rows_with_numbers)
  n_splits = sum(diffs > 1)
  if(n_splits == 0){ # if no header rows
    stats_detect = statistics_detect(intable=no_header, header = table_header, footnote=footnote, caption=caption, weight = weight)
  }
  if(n_splits > 0){ # if some header rows
    # first split
    stats_detect_multiple = list()
    index = which(diffs>1)[1] # first split location
    this_split = no_header[1:index,]
    stats_detect_multiple[[1]] = statistics_detect(intable=this_split, header = table_header, footnote=footnote, caption=caption, weight = weight)
    
    # then further splits
    for (row_split in 1:n_splits){
      index = which(diffs>1)[row_split] # split location
      if(row_split == n_splits){ 
        rstop = nrow(no_header)
      }
      if(row_split < n_splits){ 
        index_next = which(diffs>1)[row_split+1] # next split location
        rstop = index_next + row_split + (diffs[diffs>1][row_split] - 2)
      }
      rstart = index + row_split + (diffs[diffs>1][row_split] - 1) # last term needed for double-breaks
      this_split = no_header[rstart:rstop, ] # select rows of table
      ## use nearest header
      # if first row (rstart) looks like header then use that instead
      number_count = str_count(no_header[rstart,-1], '[0-9]') # exclude far-left column (with text)
      header_row_index = ifelse(sum(number_count) ==0, rstart, rstart-1) # use first row or previous depending on whether there are numbers in the row
      new_header = no_header[header_row_index, ] # from table with breaks, get sub-heading as it can contain statistics
      names(new_header) = str_remove_all(names(new_header), pattern='n.=|n=') # n was getting confused with percent
      stats_detect_multiple[[row_split+1]] = statistics_detect(intable=this_split, header = new_header, footnote=footnote, caption=caption, weight = weight)
      #stats_detect_multiple[[row_split+1]]$table$rrr = stats_detect_multiple[[row_split+1]]$table$rrr + max(stats_detect_multiple[[row_split]]$table$row) # adjust row numbers
    } 
    # now create overall list
    stats_detect = list()
    stats_detect$table = bind_rows(lapply(stats_detect_multiple, '[[', 2))
    # carry forward stats from previous row if there's no other type
    stats_detect$table = tidyr::fill(stats_detect$table, statistic, .direction = 'down')
    any_remove = unique(unlist(lapply(stats_detect_multiple, '[[', 1)))
    stats_detect$columns_to_remove = any_remove
    # print(this_split) # for testing
  }
  
  # remove p-value columns and record p-values in table ...
  actual_pvalues = NULL # 
  if(any(!is.na(stats_detect$columns_to_remove))){ # at least one not missing
    # find p-values column
    to_remove = stats_detect$columns_to_remove
    to_remove = to_remove[!is.na(to_remove)]
    # flag that there are p-values and keep them
    actual_pvalues = data.frame(cbind(no_header[,'rrr'], no_header[,to_remove])) # add row number
    colnames(actual_pvalues) = c('rrr','pvalue')
    actual_pvalues$rrr = as.numeric(actual_pvalues$rrr)
    actual_pvalues = mutate(actual_pvalues, pvalue = str_remove_all(string=pvalue, pattern=' |[a-z]|[A-Z]')) # remove letters and spaces
    # now remove from table
    no_header = no_header[, -to_remove]
    #table_header = table_header[, -to_remove] # keep because of potential mis-alignment, used by sample_sizes_est function below
  }
  
  # exclude if just one column (again), but this time with p-values
  if(ncol(no_header) <= 2){return(stop_one_column(presult=NULL, pvalues=TRUE))}
  
  stats_detect = stats_detect$table # ... then change list to table of cells
  
  ## now remove header rows from table
  # recalculate rows_with_numbers because of p-value
  table_text = str_squish(as.vector(as.matrix(no_header)))
  table_text[is.na(table_text)] = ''
  prop_numbers = str_count(table_text, pattern = '[0-9]') / str_count(table_text, pattern = '[^\\s]')
  prop_numbers[is.na(prop_numbers)] = 0
  matrix_prop = matrix(prop_numbers, ncol=ncol(no_header)) > 0.4 # more than 40% numbers
  rows_with_numbers = which(rowSums(matrix_prop) > 0)
  no_header = no_header[rows_with_numbers,] # just keep rows with numbers ...
  # ... repeat for p-values
  if(is.null(actual_pvalues) ==  FALSE){
    actual_pvalues = actual_pvalues[rows_with_numbers,]
  }
  
  #if(nrow(no_header) != nrow(stats_detect)){cat('Warning, stats and rows out for ',pmcid,'.\n', sep='')}
  
  # convert table to long format (statistics per row and column)
  no_header = no_header[, -1] # remove first column which is normally always not statistics
  row_nums = matrix(data = rep(no_header$rrr, ncol(no_header)), ncol = ncol(no_header), nrow = length(unique(no_header$rrr))) # make matrix of numbers
  col_nums = matrix(data = rep(1:ncol(no_header), nrow(no_header)), byrow = TRUE, ncol = ncol(no_header), nrow = nrow(no_header)) # make matrix of numbers
  row_nums = as.vector(as.matrix(row_nums))
  col_nums = as.vector(as.matrix(col_nums))
  table_text = as.vector(as.matrix(no_header))
  table = suppressMessages(bind_cols(row_nums, col_nums, table_text))
  names(table) = c('rrr','column','text')
  table = full_join(table, stats_detect, by = 'rrr') # add stats detected variables, merged by row number (rrr)
  table = mutate(table, 
                 text = str_replace_all(text, pattern = '[^-|0-9|.| ]', replacement = ' '), # just keep numbers, decimals, negatives and spaces (order matters, hyphen must be first)
                 text = str_squish(string = text)) %>% # remove any double spaces added, and spaces at start/end
    filter(!is.na(statistic)) %>% # only proceed if we know the statistic
    separate(text, c('stat1','stat2','stat3','stat4'), sep = ' ', fill = 'right') %>% # extract four statistics
    # calculate decimal places
    mutate(dp1 = decimalplaces(stat1),
           dp2 = decimalplaces(stat2)) %>%
    # then convert statistics to numbers
    mutate(
      rrr = as.numeric(as.factor(rrr)), # re-number rows
      stat1 = suppressWarnings(as.numeric(stat1)), # 
      stat2 = suppressWarnings(as.numeric(stat2)),
      stat3 = suppressWarnings(as.numeric(stat3)),
      stat4 = suppressWarnings(as.numeric(stat4))) %>%
    filter(!is.na(stat1)) %>% # knock out missing cells
    rename('row' = 'rrr') # use more standard name
  
  # apply same merge to p-values
  if(is.null(actual_pvalues)==FALSE){
    actual_pvalues = full_join(actual_pvalues, stats_detect, by = 'rrr') %>% # add stats detected variables, merged by row number (rrr)
      mutate(rrr = as.numeric(as.factor(rrr))) %>% # re-number rows
      rename('row' = 'rrr') #
  }
  
  if(nrow(table) == 0){
    to.return = list()
    to.return$reason = 'Could not detect statistics'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  ## get sample size from top row(s) or cells, then add to table. 
  # keep header from before removing p-value columns because of potential for mis-alignment
  mrow = min(4, nrow(table1))
  sample_sizes = sample_sizes_est(table_header = table_header[,-1], processed_table = table, first_rows = table1[1:mrow,]) # header without first column 
  nrow_sample = 99
  if(is.null(sample_sizes)==TRUE){nrow_sample = 0}
  if(is.null(sample_sizes)==FALSE){if(nrow(sample_sizes) <= 1){nrow_sample = nrow(sample_sizes)}}
  if(nrow_sample <= 1){
    to.return = list()
    to.return$reason = ifelse(nrow_sample == 0, 'No sample size', "Just one sample size")
    to.return$table = NULL
    return(to.return) # stop here
  }
  if(nrow(sample_sizes)==1){
    to.return = list()
    to.return$reason = 'Just one column in table'
    to.return$table = NULL
    return(to.return) # stop here
  }
  
  ## add sample sizes to table 
  # allow for mis-alignment, e.g., PMC7085367
  # shift sample size columns if number of columns in table match sample sizes
  cols_table = unique(table$column)
  if(any(sample_sizes$column %in% cols_table != TRUE)){
    cat(pmcid, '\n')
    cat('Columns in table:', cols_table, '\n')
    cat('Columns in sample size:', sample_sizes$column, '\n')
    sample_sizes$column = as.numeric(as.factor(sample_sizes$column)) # re-order from 1, 2, ...
  }
  # merge
  table = full_join(table, sample_sizes, by = 'column') %>% # add sample sizes from above
    filter(!is.na(sample_size)) # knock out missing sample size
  
  # if top row of table is sample sizes then remove top row
  n_same = nrow(filter(table, row==1, stat1==sample_size))
  if(n_same == max(table$column)){ # if all statistics are the sample size
    table = filter(table, row > 1) # chop top row
  }
  
  # arrange table
  table = mutate(table, 
                 row = as.numeric(as.factor(row)),
                 column = as.numeric(as.factor(column)))
  table = arrange(table, row, column) %>%
    filter(!is.na(row),
           !is.na(column))
  
  # check for repeat columns, happened with PMC7270845, also PMC7298630
  # need to progress by column if there are multiple duplicates - just run multiple times for now!
  table = find_duplicate_columns(table)
  table = find_duplicate_columns(table)
  table = find_duplicate_columns(table)
  table = find_duplicate_columns(table)
  
  ## check for columns that can be combined, e.g., https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8036030/ which has results spread over columns
  tab = with(table, table(row, column)) # table of zeros and ones
  upper_column = max(table$column) - 1 # number of columns to loop through
  re_number = FALSE
  for (loop in seq(1, upper_column, 2)){ # is there a perfect pattern in neighbouring columns? (done in pairs)
    pattern = sum(tab[,loop] == (1-tab[,loop+1])) # count of mirror image
    if(pattern / max(table$row) > 0.9){ # more than 90% mirror image
      table = mutate(table, column = ifelse(column == loop+1, loop, column)) # decrease column number by 1
      re_number = TRUE
    }
  }
  if(re_number==TRUE){table = mutate(table, column = as.numeric(as.factor(column)))} # re-number to avoid gaps in column numbers
  
  # return final results
  to.return = list()
  to.return$reason = NULL # no reason to exclude
  to.return$table = table
  return(to.return)
  
}

## function to detect what statistics are used in each row of the table
# assume 1st column contains labels
# weight: >1 then mentions column headers count for more than mentions in rows; <1 then vice versa

## text cleaning and detecting functions
# set up as a functions because used in mutate_all below
string_remove_function = function(x){str_remove_all(string = x, pattern = '[^a-z|0-9| |.|·|%|–|-|-|±|/|:| = |!!!]')} # need function for mutate_all, note multiple difference hyphens; triple !!! is my flag for median
add_space_function = function(x){str_replace_all(string = x, pattern = '\\(|\\)|,', replacement = ' \\(')} # use opening bracket for both replacements
removeunicode = function(x){gsub(pattern = "[^[:print:]]", " ", x)} # print = any printable character, see https://www.petefreitag.com/cheatsheets/regex/character-classes/
# next function replaces any number that is "n (%)", do not want to replace "x (y-z)"
add_percent_function = function(x){
  ## add to do all combinations of numbers with decimal places because of my poor regex skills!
  # integers
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9])\\)', replacement='%\\)') # number, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  # 1 decimal place
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9].[0-9])\\)', replacement='%\\)') # number, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9].[0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9][0-9].[0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  # 2 decimal places
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9].[0-9][0-9])\\)', replacement='%\\)') # number, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9].[0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  x = str_replace_all(string=x, pattern='(?<=[0-9]\\s\\([0-9][0-9][0-9].[0-9][0-9])\\)', replacement='%\\)') # numbers, space, open bracket, number, close bracket
  return(x)
} # 
# flag the median for three numbers with brackets
# p{Pd} = any hyphen or dash; see https://www.regular-expressions.info/unicode.html
flag_median_function = function(x){
  # pattern is: number, optional space, bracket, number, optional number, optional dp, optional number, optional space, separator (using or), optional space, number, optional number, optional decimal point, optional number, close bracket
  # dashes look the same, but are different
  pattern = '[0-9]\\s?\\([0-9][0-9]?\\.?[0-9]?\\s?(to|\\p{Pd}|,)\\s?[0-9][0-9]?\\.?[0-9]?[0-9]?[0-9]?[0-9]?[0-9]?\\)'
  #  pattern = '[0-9]\\s?\\([0-9][0-9]?\\.?[0-9]?\\s?(to|-|–|–|–|–|–|–|,)\\s?[0-9][0-9]?\\.?[0-9]?\\)'
  index = str_detect(string=x, pattern=pattern) # 
  index[is.na(index)] = FALSE
  if(any(index) == TRUE){
    x[index] = str_replace_all(x[index], '\\(', replacement=' !!! ') # add non-ascii flag
  }
  return(x)
}

## Mode (used by sample size)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

## function to extract sample size from column headers
extract_n = function(intext){
  N = length(intext)
  ns = rep(NA, N)
  # tidy up text:
  intext[is.na(intext)] = '' # replace NAs
  intext = tolower(intext) # convert to lower text
  intext = str_replace_all(string = intext, pattern = '[^a-z|0-9|=| ]', replacement = ' ') # keep only these (remove lots of characters), replace with spaces because of things like "n:"
  intext = str_squish(intext)
  pattern = 'n\\s?=?\\s?[0-9][0-9]?[0-9]?[0-9]?[0-9]?[0-9]?[0-9]?' # find `n=number` with optional spaces
  # allow for multiple results and take last, e.g., PMC7028836
  locations = str_locate_all(string=intext, pattern=pattern)
  last = lapply(locations, tail, 1) # take last result in each list
  df = NULL
  for (l in 1:N){ # had to loop because some results have no rows
    frame = data.frame(start=last[[l]][1], end=last[[l]][2])
    df = bind_rows(df, frame)
  }
  # now extract using substring
  if(nrow(df)>0){
    for (k in 1:nrow(df)){
      ns[k] = as.numeric(str_replace(str_sub(intext[k], df$start[k], df$end[k]), pattern='n\\s?=?\\s?', replacement=''))
    }
  }
  # alternative where cells are just numbers
  if(any(!is.na(ns)) == FALSE){ # if all missing
    if(str_detect(intext[1], pattern = ' n$')){ # if column heading ends in `n`
      ns = suppressWarnings(as.numeric(intext)) # turn off warning
    }
  }
  # add column number
  f = data.frame(column = 1:N, sample_size = ns) %>%
    filter(!is.na(sample_size))
  return(f)
}

## function to estimate sample sizes per group
sample_sizes_est = function(table_header, # table header with column headings
                            processed_table, # statistics per row
                            first_rows) # first rows of table, can have totals
{
  
  # which row of the header has the n's
  n_row = 0
  if(nrow(table_header) > 0){
    n_row = rep(0, nrow(table_header))
    for (r in 1:nrow(table_header)){
      this_row = str_squish(as.character(table_header[r, ]))
      n_row[r] = sum(str_detect(string = this_row, pattern = sample_patterns), na.rm = TRUE) # see 1_pattern.R
    }
    if(sum(n_row) > 0){ # if there's at least one row with 
      select_row = table_header[n_row == max(n_row),][1,] # select first row if there are multiple n rows
      sample_sizes = extract_n(select_row)
    }
  }
  
  if(sum(n_row) == 0){ # use column headers if n's are not in table cells
    sample_sizes = extract_n(names(table_header))
  }
  
  # if this first step (above) has worked then end and return results
  if(nrow(sample_sizes) > 0){
    return(sample_sizes)
  }
  
  # if still nothing try first available row of stats ...
  first_rows = select(first_rows, -header) # 
  # knock out mostly empty rows (reshape into long)
  first_rows_nums = first_rows[,-1] # drop label column
  names(first_rows_nums) = as.character(1:ncol(first_rows_nums)) # generic column name
  first_rows_nums$row = 1:nrow(first_rows_nums)
  any_n = reshape::melt(id.vars = 'row', data.frame(first_rows_nums), variable_name = "column") %>%
    mutate(value = str_squish(value),
           n_detect = str_detect(string=value, pattern = sample_patterns)) %>%
    group_by(row) %>%
    summarise(sum = sum(n_detect)) %>%
    filter(sum >= 2) %>%
    arrange(sum) %>% # just in case there are multiple take most
    slice(1) %>%
    pull(row)
  if(length(any_n)>0){  
    nums = extract_n(select(first_rows_nums, -row)[any_n,])
    return(nums)
  }
  
  # ... then try row labels
  to_detect = str_squish(data.frame(first_rows)[,1])
  n_in_first_rows = str_detect(to_detect, pattern=sample_patterns) # just look at column label
  n_in_first_rows[is.na(n_in_first_rows)] = FALSE
  if(any(n_in_first_rows)){
    top = which(n_in_first_rows)[1] # take highest row number if there are multiple
    to_extract = data.frame(t(first_rows[top,-1])) 
    names(to_extract) = 'value'
    nums = mutate(to_extract, 
                  column=1:n()) %>%
      separate(value, c('stat1','stat2','stat3','stat4'), sep = ' |/', fill = 'right') %>%
      filter(!is.na(stat1)) %>%
      mutate(stat1 = suppressWarnings(as.numeric(stat1))) %>%
      rename('sample_size' = 'stat1') %>%
      select(column, sample_size) %>%
      filter(!is.na(sample_size))
    row.names(nums) = NULL
    if(nrow(nums) > 0 ){  
      return(nums)
    }
  }
  
  # if no sample sizes from column headers guess from: 1) numbers, 2) percents in table cells
  if(any(processed_table$statistic == 'numbers')){
    numbers = filter(processed_table, statistic == 'numbers',
                     !is.na(stat1),
                     !is.na(stat2),
                     is.na(stat3)) %>%
      mutate(n_est =  stat1 + stat2) %>% # estimate sample size from n1 + n2
      filter(!is.na(n_est)) %>% # remove missing
      group_by(column) %>%
      summarise(sample_size = Mode(n_est),
                iqr = IQR(n_est),
                sd = sd(n_est)) %>% # get mode as an estimate of sample size
      ungroup() %>%
      filter(iqr < 1) %>% # only allow a small difference in sample sizes
      select(column, sample_size)
    if(nrow(numbers) > 0){return(numbers)} # use if there are any results
  }
  
  ## try reconstructing sample size from percents 
  # are there rows with the same estimated N?
  est_N = filter(processed_table, statistic == 'percent',
                 dp1 == 0, # must be a number
                 !is.na(stat1),
                 !is.na(stat2),
                 is.na(stat3)) %>% # stat3 must be missing
    mutate(n_est = stat1*(100/stat2), # estimate sample size from n and % (assume statistics are presented as n then %)
           n_est_round = round(n_est)) %>%
    filter(!is.na(n_est)) # exclude missing
  # if any infinite then do not use
  do_not_use = FALSE
  if(any(is.infinite(est_N$n_est)) == TRUE){
    do_not_use = TRUE
  }
  # find median and Standard deviation in sample size
  check_stats = group_by(est_N, column) %>%
    summarise(sd = sd(n_est_round),
              iqr = IQR(n_est_round),
              median = round(median(n_est_round))) %>%
    filter(iqr == 0) %>% # only if there's a small variance in the sample size
    select(column, median) %>%
    rename('sample_size' = 'median')
  if(nrow(check_stats) > 0 & do_not_use == FALSE ){ 
    return(check_stats)
  }
  
  ## try reconstructing n from numbers (last try)
  # are there rows with the same estimated N?
  est_N = filter(processed_table, statistic == 'numbers',
                 dp1 == 0, # must be a number
                 !is.na(stat1),
                 !is.na(stat2)) %>%
    mutate(n_est = stat1 + stat2)
  # if any infinite then do not use
  do_not_use = FALSE
  if(any(is.infinite(est_N$n_est)) == TRUE){
    do_not_use = TRUE
  }
  find_same = group_by(est_N, row) %>%
    mutate(N = n()) %>% # count number of non-missing statistics
    group_by(row, n_est, N) %>%
    tally() %>%
    ungroup() %>%
    filter(n == N) %>% # all cells give the same N
    pull(row)
  nums = filter(est_N, row==find_same[1]) %>%
    select(column, n_est) %>%
    rename('sample_size' = 'n_est')
  if(nrow(nums) > 0 & do_not_use == FALSE){ 
    return(nums)
  }
  
} # end of function

## function used to examine table cells
# last column has row number for merging
make_cells = function(indata){
  ncol = ncol(indata)
  names(indata)[1:(ncol-1)] = as.character(1:(ncol-1)) # generic column name, but not for last row which is row for merging `rrr`
  table_cells = reshape::melt(id.vars = 'rrr', data.frame(indata), variable_name = "column") %>% 
    mutate(column = as.numeric(column),
           percents = str_detect(string = value, pattern = '%'), # see 0_pattern.R 
           continuous = str_count(string = value, pattern = paste(c(plus_minus_utf, plus_minus), collapse = '|')), # changed from grepl, may need to change back
           numbers = str_detect(string = value, pattern = '/'),
           min_max = str_detect(string = value, pattern = 'min|max'),
           pvals = str_detect(string = value, pattern = 'p.value|p =|p='),
           ci = str_detect(string = value, pattern = '[0-9]\\s?to\\s?-?[0-9]|[0-9]\\s?,\\s?-?[0-9]'),
           median = str_detect(string = value, pattern = paste(median_numbers, collapse = '|')))
  return(table_cells)
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
  #
  m = a + c
  n = b + d
  p1 = a/m
  p2 = b/n
  # fix to avoid zero variance (May 2022)
  if(p1==0){p1 = 0.5/m}
  if(p2==0){p2 = 0.5/n}
  if(p1==1){p1 = (m-0.5)/m}
  if(p2==1){p2 = (n-0.5)/n}
  # calculate variance
  var1 = p1*(1-p1) # variance
  var2 = p2*(1-p2)
  #
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

### get the table data ready for the Bayesian model
# using indata in long format
make_stats_for_bayes_model = function(indata){
  
  # create all possible comparisons of two columns (g1 vs g2, g1 vs g3, etc)
  max_columns = group_by(indata, pmcid) %>%
    summarise(max_columns = max(column)) %>%
    ungroup()
  indata = left_join(indata, max_columns, by='pmcid')
  # those with two columns
  two_max = filter(indata, max_columns==2)
  # those with more than two columns
  over_two_max = filter(indata, max_columns > 2)
  new_data = NULL
  if(nrow(over_two_max) > 0){
    pmcids_to_loop = unique(over_two_max$pmcid)
    for(this_pmcid in pmcids_to_loop){
      this_data = filter(over_two_max, pmcid == this_pmcid) # select one study
      combs = combn(1:this_data$max_columns[1], 2) # all pairs of columns
      for (i in 1:ncol(combs)){ # loop through combinations
        extra_columns = filter(this_data, column %in% combs[,i]) %>%
          mutate(row = row + ((i-1)/ncol(combs)), # slightly alter row number
                 column = as.numeric(as.factor(column))) # column should be 1 or 2
        new_data = bind_rows(new_data, extra_columns)
      }
    }
  } # end of if
  # now select data
  if(is.null(new_data)==FALSE){
    bind_data = dplyr::select(new_data, -max_columns)
  }
  if(is.null(new_data)==TRUE){ # do not need to add extra data
    bind_data = dplyr::select(two_max, -max_columns)
  }
  
  
  # a) continuous (including CIs which have been changed to mean (SD))
  cstats = NULL
  if(any(indata$statistic == 'continuous')){
    
    cstats = filter(bind_data,
                    statistic %in% c('ci','continuous')) %>% # must have positive SD
      group_by(pmcid, row, statistic) %>%
      summarise(
        # now summaries
        size = sum(sample_size), # total sample size
        mdiff = t.test2(mean=stat1, sd=stat2, n=sample_size, return_what = 'difference'),
        sem = t.test2(mean=stat1, sd=stat2, n=sample_size, return_what = 'se'),
        t = t.test2(mean=stat1, sd=stat2, n=sample_size, return_what = 't'),
        p = t.test2(mean=stat1, sd=stat2, n=sample_size, return_what = 'p'),
        sem2 = sem^2) %>% # squared
      filter(!is.na(mdiff),
             !is.na(sem2)) %>%
      ungroup() 
    # add back statistics, used by bootstrap simulation
    add_back1 = filter(bind_data, statistic %in% c('ci','continuous'),
                       column == 1) %>%
      rename('m1' = 'stat1',
             'sd1' = 'stat2',
             'n1' = 'sample_size') %>%
      select(pmcid, row, m1, sd1, n1)
    add_back2 = filter(bind_data, statistic %in% c('ci','continuous'),
                       column == 2) %>%
      rename('m2' = 'stat1',
             'sd2' = 'stat2',
             'n2' = 'sample_size') %>%
      select(pmcid, row, m2, sd2, n2)
    cstats = full_join(full_join(cstats, add_back1, by=c('pmcid','row')),
                       add_back2, by=c('pmcid','row')) 
  }
  
  # percentages
  pstats = NULL
  if(any(indata$statistic == 'percent')){
    if(any(bind_data$statistic %in% c('percent','numbers')) == TRUE){ # needed because simulations may have no percents
      pstats = filter(bind_data,
                      statistic %in% c('percent','numbers')) %>%
        group_by(pmcid, row, statistic) %>%
        pivot_wider(id_cols=c(pmcid,statistic,row), names_from='column', values_from=c('stat1','sample_size'))  %>%
        mutate(a = stat1_1, # successes
               b = stat1_2,
               c = sample_size_1 - stat1_1, # failures
               d = sample_size_2 - stat1_2) %>%
        filter(a>=0, b>=0, c>=0, d>=0) %>%
        summarise(
          size = sample_size_1 + sample_size_2, # total sample size
          mdiff = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'difference'),
          sem = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'se'),
          t = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 't'),
          p = t.test2.binomial(a=a, b=b, c=c, d=d, return_what = 'p'),
          sem2 = sem^2) %>% # squared
        filter(!is.na(mdiff),
               !is.na(sem2),
               sem2 > 0) %>%
        ungroup() 
    }
    # add back statistics, used by bootstrap simulation
    add_back1 = filter(bind_data, statistic %in% c('percent','numbers'),
                       column == 1) %>%
      rename('n1' = 'stat1',
             'N1' = 'sample_size') %>%
      select(pmcid, row, n1, N1)
    add_back2 = filter(bind_data, statistic %in% c('percent','numbers'),
                       column == 2) %>%
      rename('n2' = 'stat1',
             'N2' = 'sample_size') %>%
      select(pmcid, row, n2, N2)
    pstats = full_join(full_join(pstats, add_back1, by=c('pmcid','row')),
                       add_back2, by=c('pmcid','row')) 
  }
  
  # combine continuous and percents
  stats = bind_rows(cstats, pstats) %>%
    arrange(pmcid, row) %>%
    mutate(study = as.numeric(as.factor(pmcid))) # turn into a number
  
  return(stats)
  
} # end of function


## function to combine columns that are labels ##
combine_cols =  function(intable){
  M = nrow(intable) - 1 # exclude last row, in case footnote is in there
  textc = sum(str_count(tolower(intable[1:M,2]), pattern='[a-z]'), na.rm=TRUE) # amount of text in 2nd column
  numbersc = sum(str_count(tolower(intable[1:M,2]), pattern='[0-9]'), na.rm=TRUE) # amount of numbers in 2nd column
  if( (textc/(numbersc+textc)) > 0.7){ # if over 70% text then must be a label column
    names(intable)[1:2] = c('label1','label2')
    intable = mutate(intable, 
                     label = ifelse(label1 != label2, paste(label1, label2), label1)) %>% # if repeat then just use first label
      select(-label1, -label2) %>%
      select(label, everything()) # move to first column
  }
  return(intable)
} # end of function

## function to move on table numbers if the table is text
exclude_text_tables = function(xml_table, table_number){
  complete = FALSE
  max_tables = length (xml_table %>% xml_nodes("table")) # maximum number of tables
  while(complete==FALSE){ # may need to be done for repeated tables
    # extract table
    table1 <- xml_table %>%
      xml_nodes("table") %>% # need have -wrap because of multiple tables in one
      .[table_number] %>%
      xml_text() # 
    
    if(length(table1)>0){
      if(nchar(table1)>0){ # needed two if statements because of graphical tables
        text_count = str_count(table1, '[a-z]')
        number_count = str_count(table1, '[0-9]')
        if((number_count / text_count) < 0.1){table_number = table_number +1} # if less than 10% numbers
        if((number_count / text_count) >= 0.1){complete = TRUE}
      }
    }
    
    # no tables available so end
    if(table_number > max_tables){
      table_number = 998
      complete = TRUE
    }
    
    if(length(table1) == 0){ # no text content in table, likely graphical table (need to check) - could add check for <graphic>
      table_number = 999
      complete = TRUE
    }
  } # end of while
  
  return(table_number)
} # end of function

# from https://github.com/agbarnett/decimal.places
decimalplaces <- function(x) {
  detect = str_detect(pattern='\\.', string=x)
  detect[is.na(detect)] = FALSE
  if(any(detect)==TRUE){ #
    res = nchar(str_split(x, pattern = '\\.', simplify = TRUE)[,2])
  }
  if(any(detect)==FALSE){ # no decimal places
    res = rep(0, length(x))
  }
  return(res)
}

### function to combine columns in table
## if mean/sd or n/% in separate columns that should be joined, e.g., PMC5861546
combine_columns = function(intable, stat1='', stat2='', reverse=FALSE){
  n = names(intable) # keep original names
  intable = data.frame(intable)
  names(intable) = n # add back original names
  ## matching patterns
  # versions for column headings, can have random letter to avoid duplicate column names
  stat1_no_n = stat1[stat1 != 'n'] # have to remove `n`, imperfect fix
  pat_stat1_h_words = paste('\\b', paste('[a-z]?', stat1_no_n, collapse='\\b|\\b', sep=''), '\\b', sep='') # whole words with or
  pat_stat1_h_startend = paste('^', paste('[a-z]?', stat1, collapse='$|^', sep=''), '$', sep='') # start/end
  pat_stat1_h_end = paste(' ',paste('[a-z]?', stat1, collapse='$| ', sep=''), '$', sep='') # start/end
  pat_stat1_h = paste(pat_stat1_h_words, pat_stat1_h_startend, pat_stat1_h_end, sep='|', collapse ='|')
  # version for first/second row
  pat_stat1_words = paste('\\b', paste(stat1_no_n, collapse='\\b|\\b'), '\\b', sep='') # whole words with or
  pat_stat1_startend = paste('^', paste(stat1, collapse='$|^'), '$', sep='') # start/end
  pat_stat1_end = paste(' ', paste(stat1, collapse='$| '), '$', sep='') # end
  pat_stat1 = paste(pat_stat1_words, pat_stat1_startend, pat_stat1_end, sep='|', collapse ='|')
  # versions for column headings, can have random letter to avoid duplicate column names
  stat2_no_n = stat2[stat2 != 'n'] # have to remove `n`, imperfect fix
  pat_stat2_h_words = paste('\\b', paste('[a-z]?', stat2_no_n, collapse='\\b|\\b', sep=''), '\\b', sep='') # whole words with or
  pat_stat2_h_startend = paste('^', paste('[a-z]?', stat2, collapse='$|^', sep=''), '$', sep='') # start/end
  pat_stat2_h_end = paste(' ',paste('[a-z]?', stat2, collapse='$| ', sep=''), '$', sep='') # start/end
  pat_stat2_h = paste(pat_stat2_h_words, pat_stat2_h_startend, pat_stat2_h_end, sep='|', collapse ='|')
  # version for first/second row
  pat_stat2_words = paste('\\b', paste(stat2_no_n, collapse='\\b|\\b'), '\\b', sep='') # whole words with or
  pat_stat2_startend = paste('^', paste(stat2, collapse='$|^'), '$', sep='') # start/end
  pat_stat2_end = paste(' ', paste(stat2, collapse='$| '), '$', sep='') # end
  pat_stat2 = paste(pat_stat2_words, pat_stat2_startend, pat_stat2_end, sep='|', collapse ='|')
  # not patterns (if both statistics together, then not a match) - negative matches
  # e.g., neighbouring columns are 'mean sd' 'mean sd' (or with plus/minus symbol)
  ## version for header (with letter) and for first row
  # stat1 then stat2
  pat_not_h = paste(apply(expand.grid(paste('[a-z]?',stat1, sep=''), 
                                      paste('[a-z]?',stat2, sep='')), 1, paste, collapse=".?.?."), collapse='|') # triple dots as could be mean sd or mean [plus/mins]sd with spaces
  pat_not = paste(apply(expand.grid(stat1, stat2), 1, paste, collapse=".?.?."), collapse='|') # triple dots as could be mean sd or mean [plus/mins]sd with spaces
  # stat2 then stat1 - can be in either order for header
  pat_not_h_rev = paste(apply(expand.grid(paste('[a-z]?',stat2, sep=''), 
                                          paste('[a-z]?',stat1, sep='')), 1, paste, collapse=".?.?."), collapse='|') # triple dots as could be mean sd or mean [plus/mins]sd with spaces
  pat_not_rev = paste(apply(expand.grid(stat2, stat1), 1, paste, collapse=".?.?."), collapse='|') # triple dots as could be mean sd or mean [plus/mins]sd with spaces
  ## searches
  #numbers_start = numbers_start_function(intable) -depending on where numbers start
  # search for stat1
  test_stat1_header = str_detect(string = str_squish(names(intable)), pattern=pat_stat1_h) # check names
  test_stat1_row = str_detect(string = str_squish(intable[1,]), pattern=pat_stat1) # check first row
  test_stat1_row2 = str_detect(string = str_squish(intable[2,]), pattern=pat_stat1) # check second row
  # search for stat2
  test_stat2_header = str_detect(string = str_squish(names(intable)), pattern=pat_stat2_h) # check names
  test_stat2_row = str_detect(string = str_squish(intable[1,]), pattern=pat_stat2) # check first row
  test_stat2_row2 = str_detect(string = str_squish(intable[2,]), pattern=pat_stat2) # check second row
  # remove `n=` from matches if looking for n
  n_equals1 = n_equals2 = NULL
  if(str_detect(string=pat_stat1, ' n\\$') | str_detect(string=pat_stat2, ' n\\$')){ # if n is in search
    n_equals_header = str_detect(str_squish(names(intable)), pattern='n\\s?=\\s?[0-9]')
    n_equals_row = str_detect(str_squish(intable[1,]), pattern='n\\s?=\\s?[0-9]')
    n_equals_row2 = str_detect(str_squish(intable[2,]), pattern='n\\s?=\\s?[0-9]')
    n_equals_header[is.na(n_equals_header)] = FALSE
    n_equals_row[is.na(n_equals_row)] = FALSE
    n_equals_row2[is.na(n_equals_row2)] = FALSE
    n_equals = as.logical(pmax(n_equals_header, n_equals_row, n_equals_row2))
    if(str_detect(string=pat_stat1, ' n\\$')){n_equals1 = n_equals} # stat1 or 2?
    if(str_detect(string=pat_stat2, ' n\\$')){n_equals2 = n_equals}
  }
  # search all rows for %'s in cells, only if % is one of the search strings
  if(str_detect(string=pat_stat1, '\\%') | str_detect(string=pat_stat2, '\\%')){
    percents = (str_count(intable, pattern='[0-9]\\%') / nrow(intable)) > 0.5 # more than 50% match; gives warning, but works
    if(any(percents) == TRUE){
      if(str_detect(string=pat_stat1, '\\%') & !str_detect(string=pat_stat1, 'CI')){ # % in first search
        test_stat1_row2[which(percents)] = TRUE
        test_stat1_row[which(percents)] = TRUE
        test_stat1_header[which(percents)] = TRUE
      }
      if(str_detect(string=pat_stat2, '\\%') & !str_detect(string=pat_stat2, 'CI')){ # % in second search, but not confidence interval
        test_stat2_row2[which(percents)] = TRUE # apply to header and row
        test_stat2_row[which(percents)] = TRUE # 
        test_stat2_header[which(percents)] = TRUE
      }
    }
  }
  # knock out `n=` on selected rows
  if(is.null(n_equals1) == FALSE){
    if(any(n_equals_header)){test_stat1_header[n_equals1] = FALSE}
    if(any(n_equals_row)){test_stat1_row[n_equals1] = FALSE}
    if(any(n_equals_row2)){test_stat1_row2[n_equals1] = FALSE}
  }
  if(is.null(n_equals2) == FALSE){
    if(any(n_equals_header)){test_stat2_header[n_equals2] = FALSE}
    if(any(n_equals_header)){test_stat2_row[n_equals2] = FALSE}
    if(any(n_equals_header)){test_stat2_row2[n_equals2] = FALSE}
  }
  # negative search for both
  test_both_header = str_detect(string = str_squish(names(intable)), pattern=pat_not_h) # 
  test_both_row = str_detect(string = str_squish(intable[1,]), pattern=pat_not) # 
  test_both_row2 = str_detect(string = str_squish(intable[2,]), pattern=pat_not) # 
  test_both_header_rev = str_detect(string = str_squish(names(intable)), pattern=pat_not_h_rev) # 
  test_both_row_rev = str_detect(string = str_squish(intable[1,]), pattern=pat_not_rev) # 
  test_both_row2_rev = str_detect(string = str_squish(intable[2,]), pattern=pat_not_rev) # 
  test_both_header = pmax(test_both_header, test_both_header_rev)
  test_both_row = pmax(test_both_row, test_both_row_rev)
  test_both_row2 = pmax(test_both_row2, test_both_row2_rev)
  # test if neighbours (lag shifts one to the right)
  neighbours_header = test_stat2_header == lag(test_stat1_header) & test_stat2_header == TRUE # header
  neighbours_row = test_stat2_row == lag(test_stat1_row) & test_stat2_row == TRUE # first row
  neighbours_row2 = test_stat2_row2 == lag(test_stat1_row2) & test_stat2_row2 == TRUE # second row
  neighbours_both = as.logical(pmax(test_both_header, test_both_row)) # combine negative searches
  neighbours_header[is.na(neighbours_header)] = FALSE
  neighbours_row[is.na(neighbours_row)] = FALSE
  neighbours_row2[is.na(neighbours_row2)] = FALSE
  neighbours_both[is.na(neighbours_both)] = FALSE
  neighbours = as.logical(pmax(neighbours_header, neighbours_row, neighbours_row2)) # combine header and row
  neighbours[neighbours_both==TRUE] = FALSE # turn off any negative matches
  if(sum(neighbours) > 1){ # merge columns, only if applied to 2+ groups, otherwise might just be getting order wrong, e.g., 1:% 2:n 3:% 4:n would wrongly merge 2 and 3
    names_order = NULL # for re-ordering columns
    index = which(neighbours)
    for (i in rev(index)){ # work backwards (from right to left)
      if(reverse==FALSE){# combine columns and column names
        intable[,i-1] = paste(intable[,i-1], intable[,i])
        names(intable)[i-1] = paste(names(intable)[i-1], names(intable)[i])
      } 
      if(reverse==TRUE){ # combine columns (reverse for `% n`)
        intable[,i-1] = paste(intable[,i], intable[,i-1])
        names(intable)[i-1] = paste(names(intable)[i], names(intable)[i-1])
      } 
      names_order = c(names_order, names(intable)[i-1])
      intable = intable[, -i] # now remove column 
    }
    names_order = rev(c(names_order, names(intable)[1])) # , keep first column at left-most position; put last, then reverse
    intable = select(intable, names_order, everything()) # keep original column order and move to front
  }
  return(intable)
  
} # End of function

## function to combine statistics on neighbouring rows
combine_rows = function(intable, stat1, stat2){
  pattern1 = paste("^", stat1, collapse='|','$', sep='') # must be whole words
  pattern2 = paste("^", stat2, collapse='|','$', sep='')
  detect_stat1 = str_detect(intable[,1], pattern=pattern1)
  detect_stat2 = str_detect(intable[,1], pattern=pattern2)
  neighbours = which(detect_stat2 == lag(detect_stat1) & detect_stat1 == TRUE)
  if(length(neighbours)>0){
    for(k in neighbours){
      intable[k, ] = paste(intable[k,], intable[k+1,])
      intable = intable[-(k+1),]
    }
  }
  return(intable)
} # end of function

# function to work out row that numbers start on
numbers_start_function  = function(intable){
  cells = intable[,-1] # don't use first column with labels
  long = mutate(cells, row = 1:n())
  numbers_start = reshape::melt(id.vars = 'row', data=data.frame(long)) %>%
    mutate(value=tolower(value),
           text = str_count(value, '[a-z]'), 
           n = str_detect(value, 'n=|n =|^n$'), # add `n=` to header, third is just "N"
           numbers = str_count(value, '[0-9]')) %>% 
    group_by(row) %>%
    summarise(
      text = sum(text, na.rm = TRUE),
      n = sum(n, na.rm = TRUE),
      numbers =  sum(numbers, na.rm = TRUE)) %>% # amount of text and numbers
    ungroup() %>%
    filter(n < 2) %>% # or with two `n=`
    filter(numbers > text) %>% # first row with more numbers than text
    slice(1) %>%
    pull(row)
  return(numbers_start)
} # end of function

## find duplicate columns in long table
find_duplicate_columns = function(intable){
  duplicates = select(intable, row, stat1, stat2) %>%
    duplicated()
  if(any(duplicates)==FALSE){
    return(intable)
  }
  # if there are duplicates  
  duplicate_rows = intable[duplicates,]
  max_row = max(intable$row) # table size
  # more than 75% match
  find_columns = (table(duplicate_rows$column) / max_row) > 0.75
  if(any(find_columns)==TRUE){
    to_remove = as.numeric(names(find_columns))[1] # just first column (do one at a time)
    intable = filter(intable, !column %in% to_remove) %>%
      mutate(column = as.numeric(as.factor(column))) # re-number columns
  }
  return(intable)
}

# function to stop processing because table is just one column, e.g., PMC3071307
stop_one_column = function(presult=NULL, pvalues=FALSE){ # FALSE if p-values not detected
  to.return = list() 
  if(pvalues==TRUE){to.return$pvalues_in_table = presult} # mostly do not record this as there are no groups to compare
  to.return$reason = 'Just one column in table'
  to.return$table = NULL
  return(to.return) # stop here
}

# function for rounding numbers with zeros kept
roundz = function(x, digits=0){
  dformat = paste('%.', digits, 'f', sep='')
  x = sprintf(dformat, round(x, digits))
  return(x)
}

# merging the complex data from the algorithm and hand entered data
complex_merge = function(table1, table2, by){
  # get all PMCIDS
  pmcids = unique(c(table1$pmcid,table2$pmcid))
  # loop
  merged_data = NULL
  for (this in pmcids){
    m1 = filter(table1, pmcid==this)
    m2 = filter(table2, pmcid==this)
    r1 = max(m1$row)
    r2 = max(m2$row)
    # if same number of rows, then simple merge
    if(r1 == r2){
      this_merge = full_join(m1, m2, by=by)
      merged_data = bind_rows(merged_data, this_merge)
      next 
    }
    # if same number of rows, then simple merge
    if(r1 != r2){
    }
  } # end of loop
  return(merged_data)
} # end of function


## get first author country
get_affiliation = function(inpage){
  
  # try for simple country first
  affiliation = xml_find_all(inpage, ".//aff//country") %>% xml_text()
  affiliation = affiliation[1] # first author
  # if empty look for country label
  if(is.na(affiliation) == TRUE){ 
    address = paste(xml_find_all(inpage, ".//aff") %>% xml_text(), collapse = ' ') # likely full address for all authors
    # search for country
    potential = str_locate_all(address, pattern=countries)
    match = which(lapply(potential, length) > 0) # which elements of the list match 
    if(length(match) > 0){
      all = NULL
      for (m in match){
        f = as.data.frame(potential[[m]]) %>% mutate(country = countries[m])
        all = bind_rows(all, f)
      }
      affiliation = arrange(all, start) %>% slice(1) %>% pull(country) # first country in address
    }
  }
  # if still empty look for email
  if(is.na(affiliation) == TRUE){ 
    email = str_split(str_split(string=address, '@')[[1]][2], ' ')[[1]][1] # get first email address after the @
    if(is.na(email) ==FALSE){
      potential = str_extract_all(email, pattern=emails$email)
      match = which(lapply(potential, length)>0) # which elements of the list match 
      if(length(match)>0) {affiliation = emails$country[match]}
    } # 
  }
  # if still empty look for US state
  if(is.na(affiliation) == TRUE){ 
    state_search = paste('\\b', paste(c(states$state, states$postal), collapse='\\b|\\b'), '\\b', sep='') # state names and postcodes, whole words only
    potential = str_extract_all(address, pattern=state_search) # search for state name or postcode
    match = which(lapply(potential, length)>0) # which elements of the list match 
    if(length(match) > 0){affiliation = 'USA'} # if any matches then must be USA
  }
  # if still empty try capital cities - to do, country codes too?
  if(is.na(affiliation) == TRUE){ 
    capitals = mutate(capitals, city_town = paste('\\b', city_town, '\\b', sep='')) # whole words only
    # search for capital
    potential = str_locate_all(address, pattern=capitals$city_town)
    match = which(lapply(potential, length) > 0) # which elements of the list match 
    if(length(match) > 0){
      all = NULL
      for (m in match){
        f = as.data.frame(potential[[m]]) %>% mutate(country = capitals$country[m])
        all = bind_rows(all, f)
      }
      affiliation = arrange(all, start) %>% slice(1) %>% pull(country) # first country in address
    }
  }
  return(affiliation)
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



## function to remove duplicate rows
remove_duplicate_rows = function(intable){
  duplicates = NULL
  M = ncol(intable)
  for(k in 2:nrow(intable)){
    test = as.character(intable[k,]) == as.character(intable[k-1,])
    test[intable[k-1,] == '' | is.na(intable[k-1,])] = TRUE # count missing as a duplicate
    test[is.na(test)] = FALSE
    if(sum(test) == M){duplicates= c(duplicates, k-1)}
  }
  if(is.null(duplicates) == FALSE){
    intable = intable[-duplicates,] # knock out rows
  }
  return(intable)
}

## detect the statistic type used in table
statistics_detect = function(intable, 
                             header,
                             caption, # sometimes clues on statistics are in the title ...
                             footnote, # ... more often in the footnote
                             foot_weight = 0.1, # mentions in title/footnote count less
                             weight = 2) 
{
  
  columns_to_remove = NA # store columns to remove
  
  ## a) look for stats in header
  #header = header[,-1] # do not use first column (usually labels) - turned off again because header can have useful info
  header = rbind(header, names(header)) # add header as text and remove names
  row.names(header) = NULL
  names(header) = NULL
  header = t(header) # transpose
  header = data.frame(cbind(1:nrow(header), header)) ###
  header = reshape::melt(id.vars = 'X1', header)
  stats_header = mutate(header,
                        value = str_squish(value),
                        X1 = as.numeric(X1),
                        percent = str_count(value, pattern = percent_pattern), # see 1_pattern.R for all patterns
                        continuous = str_count(value, pattern = continuous_pattern), # grepl(x = value, pattern = continuous_pattern),
                        numbers = str_count(value, pattern = number_pattern),
                        min_max = str_count(value, pattern = min_max_pattern),
                        pval = str_detect(value, pattern = pval_pattern), # just detect, no need for count
                        median = str_count(value, pattern = median_pattern),
                        ci = str_count(value, pattern = ci_pattern)) %>% 
    group_by(X1) %>%
    summarise(col_percent = sum(percent, na.rm = TRUE),
              col_continuous = sum(continuous, na.rm = TRUE),
              col_numbers = sum(numbers, na.rm = TRUE),
              col_min_max = sum(min_max, na.rm = TRUE),
              col_pvals = sum(pval, na.rm = TRUE),
              col_median = sum(median, na.rm = TRUE),
              col_ci = sum(ci, na.rm = TRUE)*5) %>% # added weight because likely also match with continuous
    rename('column' = 'X1')
  # convert column to row scores (apply column scores to every row)
  stats_header_row = uncount(stats_header, weights=length(intable$rrr)) %>% # spread over rows
    group_by(column) %>%
    mutate(rrr = intable$rrr) %>% # for merging
    ungroup() %>%
    mutate(col_percent = col_percent/(ncol(stats_header) - 1), # spread score over rows for later summing
           col_continuous = col_continuous/(ncol(stats_header) - 1),
           col_numbers = col_numbers/(ncol(stats_header) - 1),
           col_min_max = col_min_max/(ncol(stats_header) - 1),
           col_pvals = col_pvals/(ncol(stats_header) - 1),
           col_median = col_median/(ncol(stats_header) - 1),
           col_ci = col_ci/(ncol(stats_header) - 1))
  
  ## remove columns that are p-values
  pval_cols = as.numeric(filter(stats_header, col_pvals > 0) %>% pull(column))
  plus_one = 0 # should be one because of label columns? Can't work this out!
  if(length(pval_cols)>0){
    intable = intable[, -(pval_cols+plus_one)] # remove columns
    columns_to_remove = c(columns_to_remove, pval_cols+plus_one) # add to information to return (plus one because of label columns)
  }
  stats_header_row = filter(stats_header_row, !column %in% pval_cols)
  
  ## look for columns of p-values based on cells
  # make table cells; gives scores for statistics based on each cell
  table_cells = make_cells(intable[, -1]) # drop label column
  # now look for lots of p-values by column
  column_pvals = group_by(table_cells, column) %>%
    summarise(pval_fraction = sum(pvals) / n()) %>%
    ungroup() %>%
    filter(pval_fraction > 0.5) %>% # remove if over half are p-values
    pull(column) 
  if(length(column_pvals)>0){
    intable = intable[, -(column_pvals+1)] # remove columns, plus one because of label column
    table_cells = make_cells(intable[, -1]) # need to redo cells (drop label column)
    columns_to_remove = c(columns_to_remove, column_pvals+1) # add to information to return
  }
  stats_header_row = filter(stats_header_row, !column %in% column_pvals)
  
  ## b) look for stats in first column of non-header (label column)
  # first remove 'min/' as it gets confused with minimum
  to_test = as.vector(as.data.frame(intable)[,1])
  to_test = str_remove_all(to_test, pattern='min/')
  percent_alone = str_count(to_test, pattern = '\\%|percent') # just % (give higher weighting to this)
  percents = str_count(to_test, pattern = percent_pattern) # see 0_pattern.R for all patterns
  mean_alone = str_count(to_test, pattern = 'mean') # just mean (give higher weighting to this)
  continuous = str_count(to_test, pattern = continuous_pattern) # changed from grepl, may need to change back
  numbers = str_count(to_test, pattern = number_pattern)
  min_max = str_count(to_test, pattern = min_max_pattern)
  pvals = str_detect(to_test, pattern = pval_pattern) # just detect instead of count
  median_alone = str_count(to_test, pattern = 'median') # just median (give higher weighting to this)
  medians = str_count(to_test, pattern = median_words) # just look for median words, not number patterns
  cis = str_count(to_test, pattern = ci_pattern) # 
  stats_label_column = data.frame(rrr = intable$rrr, # 
                                  percents = (percent_alone*2) + as.numeric(percents) , # using weights, but not for percent because of things like Left ventricular ejection fraction and HbA1c
                                  continuous = ((mean_alone*2) + continuous) * weight,
                                  numbers = numbers * weight,
                                  min_max = min_max * weight,
                                  pvals = pvals * weight,
                                  medians = median_alone*2*weight + medians * weight*1.1, # double if median alone is found, tiny increase in median to avoid ties
                                  cis = cis)  # 
  
  ## c) melt to long to look at stats units in cells
  stats_cells = full_join(table_cells, stats_header_row, by=c('rrr','column')) %>% # add header estimates, using `table_cells` from above
    group_by(rrr) %>%
    summarise(cell_percents = sum(percents, na.rm = TRUE) + sum(col_percent, na.rm = TRUE), # total counts per row
              cell_continuous = sum(continuous, na.rm = TRUE) + sum(col_continuous, na.rm = TRUE),
              cell_numbers = sum(numbers, na.rm = TRUE) + sum(col_numbers, na.rm = TRUE),
              cell_min_max = sum(min_max, na.rm = TRUE) + sum(col_min_max, na.rm = TRUE),
              cell_pvals = sum(pvals, na.rm = TRUE) + sum(col_pvals, na.rm = TRUE),
              cell_median = sum(median, na.rm = TRUE) + sum(col_median, na.rm = TRUE),
              cell_cis = sum(ci, na.rm = TRUE) + sum(col_ci, na.rm = TRUE)) 
  
  ## d) look for stats in footnotes and captio, apply to all cells
  # empty stats in case there is no footnote
  footnote_stats = data.frame(rrr = stats_cells$rrr) %>%
    mutate(
      footnote_percents = 0, # 
      footnote_continuous = 0,
      footnote_min_max = 0,
      footnote_medians = 0,
      footnote_cis = 0
    )
  both = tolower(paste(footnote, caption, sep=' ')) # add title and footnote together
  if(!is.na(both)){
    if(nchar(both)>1){
      footnote_stats = data.frame(rrr = intable$rrr) %>%
        mutate(
          footnote_percents = str_count(both, pattern = footnote_percent), # 
          footnote_continuous = str_count(both, pattern = footnote_continuous), # changed from grepl, may need to change back
          footnote_min_max = str_count(both, pattern = footnote_min_max),
          footnote_medians = str_count(both, pattern = footnote_median),
          footnote_cis = str_count(both, pattern = ci_pattern)
        ) %>%
        mutate_all(as.numeric)
      # search for specific patterns
      median_specific_detect = str_detect(both, pattern=median_specific)
      continuous_specific_detect = str_detect(both, pattern=continuous_specific)
      percent_specific_detect = str_detect(both, pattern=percent_specific)
      ci_specific_detect = str_detect(both, pattern='\\% confidence interval')
      footnote_stats = mutate(footnote_stats, # divide by foot_weight below to make sure weights count big
                              footnote_medians = footnote_medians + ((1/foot_weight)*median_specific_detect),
                              footnote_continuous = footnote_continuous + ((1/foot_weight)*continuous_specific_detect),
                              footnote_percents = footnote_percents + ((1/foot_weight)*percent_specific_detect),
                              footnote_cis = footnote_cis + ((1/foot_weight)*1.1*ci_specific_detect)) # 1.1 to beat mean for continuous as both come together
    }
  }
  
  # now combine cells and column labels and make best guess about statistics per row    
  combine = full_join(stats_cells, stats_label_column, by = 'rrr') %>%
    full_join(footnote_stats, by='rrr') %>%
    mutate(percent_score = as.numeric(cell_percents + percents + foot_weight*footnote_percents), # scores that combine cell and column information
           continuous_score = as.numeric(cell_continuous + continuous + foot_weight*footnote_continuous),
           numbers_score = as.numeric(cell_numbers + numbers),
           min_max_score = as.numeric(cell_min_max + min_max + foot_weight*footnote_min_max),
           pvals_score = as.numeric(cell_pvals + pvals),
           median_score = as.numeric(cell_median + medians + foot_weight*footnote_medians),
           ci_score = as.numeric(cell_cis + cis + foot_weight*footnote_cis)) %>%
    select(rrr, ends_with('score')) %>%
    pivot_longer(cols = ends_with('score'), names_to = 'statistic', values_to = 'score') %>%
    group_by(rrr) %>%
    arrange(-score) %>% # take top coring type per row
    slice(1) %>%
    mutate(statistic = str_remove_all(statistic, pattern = '_score'),
           statistic = ifelse(score == 0, NA, statistic)) %>% # swap those with zero score to missing
    ungroup()
  # carry forward stats from previous row if there's no other type
  carry = tidyr::fill(combine, statistic, .direction = 'down') %>%
    select(-score) 
  
  ## find percents that are actually continuous  --- to finish
  extra = function(){
    long = reshape::melt(id.vars = 'rrr', data.frame(cells)) %>%
      mutate(value = str_replace_all(string = value, pattern = '·', replace = '.'), # remove odd decimal place used by Lancet
             value = str_replace_all(value, pattern = '[^0-9|.| ]', replacement = ' '), # just keep numbers, decimals and spaces
             value = str_squish(string = value)) %>% # remove any double spaces added, and spaces at start/end
      separate(value, c('stat1','stat2','stat3','stat4'), sep = ' |/', fill = 'right') %>%
      mutate(
        stat1 = suppressWarnings(as.numeric(stat1)),
        stat2 = suppressWarnings(as.numeric(stat2)),
        int1 = round(stat1) - stat1 == 0,
        int2 = round(stat2) - stat2 == 0) %>% # check for n (%) format
      filter(!is.na(stat1),
             !is.na(stat2)) %>%
      group_by(rrr) %>%
      summarise(n = n(), integers1 = sum(int1), integers2 = sum(int2)) %>%
      filter(n > 0,
             integers1 == n) # all first statistics are integers
    #carry = left_join(long, )
  }
  
  # return
  to_return = list()
  to_return$columns_to_remove = columns_to_remove
  to_return$table = carry
  return(to_return)
}

## function to select papers from Bayesian model results, used by 5_summary_results.Rmd
select_papers = function(
    in_data,
    stats,
    exclude = NULL, # exclude based on multiplier; useful to exclude extreme results
    flag, # looking at mean or variance flag
    flag_value, # flag value to select
    flag_null_value, # flag value to select for comparison
    mean_select = '', # positive or negative
    variable,
    n_select # number of studies to select
){
  # get number of rows per study to avoid including studies with few rows
  n_rows = group_by(in_data, study) %>% tally()
  # find studies that are flagged
  flagged = filter(stats, nicevar == flag, median == flag_value) %>%
    select(study) %>%
    left_join(n_rows, by='study') %>%
    filter(n > 2) # more than two results
  # merge with mean
  means = filter(stats, nicevar == variable) %>%
    right_join(flagged, by='study')
  # exclude based on precision multiplier (either too big or too small)
  if(is.null(exclude)==FALSE & mean_select == 'positive'){
    means = filter(means, mean <= log(exclude)) # given on non-log scale so need to transform
  }
  if(is.null(exclude)==FALSE & mean_select == 'negative'){
    means = filter(means, mean >= log(exclude)) # given on non-log scale so need to transform
  }
  if(mean_select == 'negative'){
    means = arrange(means, mean)
  }
  if(mean_select == 'positive'){
    means = arrange(means, desc(mean))
  }
  means = slice(means, 1:n_select) %>%
    select(study)
  # merge top studies with data
  t_stats_flagged = left_join(means, in_data, by='study')
  
  # now find studies that are not flagged as a contrast
  not_flagged = filter(stats, nicevar == flag, median == flag_null_value) %>%
    select(study) %>%
    left_join(n_rows, by='study') %>%
    filter(n > 2) # more than two results
  # merge with mean
  means = filter(stats, nicevar == variable) %>%
    right_join(not_flagged, by='study') %>%
    sample_n(n_select) %>% # just select at random
    select(study)
  # merge top studies with data
  t_stats_not_flagged = left_join(means, in_data, by='study')
  
  # make ordered factor (so that issue and not are grouped)
  pmcid1 = unique(t_stats_flagged$pmcid)
  pmcid1 = pmcid1[order(pmcid1)] # for neater ordering in plots
  pmcid2 = unique(t_stats_not_flagged$pmcid)
  pmcid2 = pmcid2[order(pmcid2)] # for neater ordering in plots
  for_order = c(pmcid1, pmcid2)
  
  # combine and return
  t_stats = bind_rows(t_stats_flagged, t_stats_not_flagged, .id='type') %>%
    mutate(pmcid_ordered = factor(pmcid, levels=for_order))
  
  return(t_stats)
}

# function to make nice results from winbugs, used by 4_model_cumulative.R
neaten_results = function(in_res, type, n_table_rows){
  store = data.frame(in_res$summary[,c(1,3,7)])
  names(store)[2:3] =  c('lower','upper')
  store$var = rownames(store)
  store = mutate(store, 
                 n_table_rows = n_table_rows,
                 type = type)
  row.names(store) = NULL
  return(store)
}
