# main_function.R
# function that does the heavy lifting
# not run as a function in shiny version

# start with no reason
reason = NULL

# maximum number of tables - probably no longer needed
max_table = length(webpage %>% xml_nodes("table-wrap"))
if(table_number > max_table){ # table likely in appendix or graphic
  results = list()
  results$reason = 'Table in appendix or table is graphic'
  results$table = NULL
  break # stop here
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
  results = list()
  results$reason = 'Table is graphic'
  results$table = NULL
  break # stop here
}
# remove footnote within table
xml_find_all(table1, ".//tfoot") %>% xml_remove()    
#
table1 =  xml_nodes(table1, "table") %>% # get first table in wrap
  html_table(header = TRUE, fill = TRUE) # include table headers
table1 = table1[[1]] # to data frame

# if null table then stop here
if(nrow(table1) == 0){ # table likely a graphic
  results = list()
  results$reason = 'Table is graphic'
  results$table = NULL
  break # stop here
}
if(nrow(table1) == 1){ # Badly formatted, all results in one row
  results = list()
  results$reason = 'Complex format'
  results$table = NULL
  break # stop here
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
  results = list() 
  results$reason = 'Follow-up results in baseline table'
  results$table = NULL
  break # stop here
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
    results = list()
    results$reason = 'Difficult layout'
    results$table = NULL
    break # bail out here, will be impossible to unpick
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
## update, do not combine if second column has two statistics
columns = as.matrix(table1)[,-1] # without first column
bcounts = colSums(apply(columns, FUN=str_count, MARGIN=2, pattern='\\('),na.rm = TRUE)
proportions = bcounts / nrow(columns)
n.cols = ncol(columns)
no_brackets = proportions < 0.05 | bcounts <= 1 # changed March 2023 due to 
with_brackets = proportions > 0.75
# update
two_stats_count = colSums(apply(columns, FUN=str_count, MARGIN=2, pattern='[0-9] ?\\(.?[0-9]'),na.rm = TRUE)
proportions_two_stats = two_stats_count / nrow(columns)
with_two_stats = proportions_two_stats > 0.75
#
index = which((no_brackets[1:(n.cols-1)] + with_brackets[2:n.cols] ==2) & with_two_stats[2:n.cols] == FALSE) # any columns of non-brackets followed by brackets
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
  results = list() 
  results$reason = 'Pre-post comparison'
  results$table = NULL
  break # stop here
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
      # get max sample size (update March 2024 - changed to max, assume that's greatest sample size)
      numbers = matrix(as.numeric(M), ncol = ncol(M)) # ignore NA warnings
      nmean = round(colMeans(numbers, na.rm=TRUE)) # means
      nmax = rep(NA, ncol(numbers))
      for (c in 1:ncol(numbers)){
        nmax[c] = max(numbers[,c], na.rm=TRUE) # calculate maximum, can be useful sample size as assuming other n's include item missing data
      }
      # use maximum if there's not a massive gap between max and mean
      small_gap = (nmax - nmean)/nmean < 0.1 # under 10%
      n_to_use = nmean # start with mean ... 
      if(all(small_gap)){n_to_use = nmax} # ... use max if gaps are small
      # find first row with numbers
      n_text= paste('n=', n_to_use, sep='') # make detectable text
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
  results = list()
  results$reason = 'Could not detect statistics'
  results$table = NULL
  break # stop here
}

## get sample size from top row(s) or cells, then add to table. 
# keep header from before removing p-value columns because of potential for mis-alignment
mrow = min(4, nrow(table1))
sample_sizes = sample_sizes_est(table_header = table_header[,-1], processed_table = table, first_rows = table1[1:mrow,]) # header without first column 
nrow_sample = 99
if(is.null(sample_sizes)==TRUE){nrow_sample = 0}
if(is.null(sample_sizes)==FALSE){if(nrow(sample_sizes) <= 1){nrow_sample = nrow(sample_sizes)}}
if(nrow_sample <= 1){
  results = list()
  results$reason = ifelse(nrow_sample == 0, 'No sample size', "Just one sample size")
  results$table = NULL
  break # stop here
}
if(nrow(sample_sizes)==1){
  results = list()
  results$reason = 'Just one column in table'
  results$table = NULL
  break # stop here
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

## return
results = list()
results$reason = reason
results$table = table
