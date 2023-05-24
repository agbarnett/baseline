# get_pmcid_table.R
# get the table data directly from the full-text
# May 2023
# example = PMC5640030; doi = '10.1136/bmjopen-2017-017592'

# function
get_pmcid_table = function(pmcid){

  reason = NULL # start with empty
  
pmcid_function = str_remove(pattern='PMC', string = pmcid) # have to remove numbers for function
# get full text as web page, need try catch because some are embargoed or otherwise not available
out_nlm = NULL
out_nlm = tryCatch(mt_read_pmcoa(pmcid = pmcid_function, file_format = "pmc", file_name="full.xml"), # save to external XML file
                   error = function(e) print(paste('NLM did not work', pmcid))) # flag for error
#
if(length(out_nlm) > 0){ # 
  reason = 'Full text page not available'
}

## get study title
webpage = read_xml("full.xml", encoding='UTF-8') 
title = xml_find_all(webpage, ".//article-title") %>% .[1] %>%xml_text() # tables and figures
# get abstract
abstract = xml_find_all(webpage, ".//abstract") %>% xml_text() # tables and figures
# exclude single arm studies
single_arm = any(str_detect(abstract, pattern='single.arm|singlearm'))
if(single_arm==TRUE){
  reason = 'Single-arm study'
}
# search for randomised trial in title or abstract
title_abstract = paste(title, abstract, collapse=' ')
rct = str_detect(title_abstract, pattern=rct_patterns) # pattern from 1_pattern.R
# search for cluster in title or abstract
cluster = str_detect(title_abstract, pattern='\\bcluster|\\bCluster') # 
# exclude cross-sectional studies, exploratory studies, pooled analysis, etc (could exclude others here)
cross_sec_title = any(str_detect(tolower(title), pattern = non_rct_pattern_title))
cross_sec_abstract = any(str_detect(tolower(abstract), pattern = non_rct_pattern_abstract))
if(cross_sec_title ==TRUE | cross_sec_abstract == TRUE){ # 
  reason = 'Not an RCT'
}

## remove some sections from XML
# now remove <front> (can also get confused with labels, like <header>)
xml_find_all(webpage, ".//front") %>% xml_remove()
# remove boxed text, gets confused with tables, e.g. "Research in context"
xml_find_all(webpage, ".//boxed-text") %>% xml_remove()
# remove supplementary material as tables in here are not accessible
xml_find_all(webpage, ".//supplementary-material") %>% xml_remove()
# remove graphic, causes problems in table
xml_find_all(webpage, ".//graphic") %>% xml_remove()
# remove copyright stuff
xml_find_all(webpage, ".//permissions") %>% xml_remove()
## remove cross-references (mucked up caption comparison) but keep text
# had to change to text, see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6660508/
# see https://stackoverflow.com/questions/68616875/using-xml-replace-leaves-behind-some-formatting?noredirect=1#comment121326430_68616875
page_char <- as.character(webpage)
page_remove_xref <- gsub("<xref[^>]*>|<\\/xref>", " ", page_char)
page_add_breaks <- gsub("<table-wrap", "\n<table-wrap", page_remove_xref) # add breaks before tables, helps with p-value in text by making sure table is separate to paragraph
# replace breaks with spaces (sometimes used in tables)
page_add_breaks <- gsub("<break\\/>|<break>", " ", page_add_breaks)
# replace badly formatted plus/minus, e.g., PMC7607674
page_add_breaks <- gsub("<underline>\\+<\\/underline>", "±", page_add_breaks)
# replace `n1=` with `n=`, e.g. PMC6731465
page_add_breaks <- gsub("\\bn\\s?[0-9]=", "n=", page_add_breaks)
# remove colspan, causes havoc in tables with labels; but removing it caused problems too, so commented out
#page_add_breaks = str_replace_all(pattern='colspan="."', replacement = 'colspan="1"', string=page_add_breaks)
webpage <- read_xml(page_add_breaks)

## remove paragraph returns in captions (see https://stackoverflow.com/questions/67864686/xml-in-r-remove-paragraphs-but-keep-xml-class/67865367#67865367)
# find the caption
caption <- xml_find_all(webpage, './/caption')
if(length(caption) > 0){
  # store existing text
  replacement = rep('', length(caption))
  for (q in 1:length(caption)){
    any_p = xml_find_all(caption[[q]], './/p') # are there paragraphs
    if(length(any_p)>0){
      with_p = caption[[q]] %>% xml_find_all( './/p') %>% xml_text() %>% paste(collapse = " ")
      without_p = caption[[q]] %>% xml_find_all( './/title') %>% xml_text() %>% paste(collapse = " ")
      replacement[q] <- paste(without_p, with_p) # need both because some first headers have no <p>
    }
    if(length(any_p) == 0){
      replacement[q] <- caption[[q]] %>% xml_text() %>% paste(collapse = " ")
    }
    replacement[q] = gsub(replacement[q], pattern='\n', replacement = ' ') # remove other style of breaks
  }
  # remove the desired text
  caption %>% xml_find_all( './/p') %>% xml_remove()
  #replace the caption
  xml_text(caption) <- replacement
}
## Over write XML with above changes
xml2::write_xml(webpage, file='full.xml', encoding = "UTF-8")

## find table captions, look just in tables
table_captions <- webpage %>%
  xml_nodes("table-wrap")  %>%
  xml_nodes("caption") %>%
  xml_text()
table_captions = tolower(table_captions) %>%
  str_remove_all("[^0-9|a-z| ]") %>% # remove all special characters to make matching below easier
  str_squish()

# are there any baseline tables based on captions?
captions_baseline = str_detect(string=tolower(table_captions), pattern=words_or) # find captions with baseline words, see 1_patterns.R
captions_negative = str_detect(string=tolower(table_captions), pattern=negative_words) # find captions with negative words
if(any(captions_negative) == TRUE){
  captions_baseline[which(captions_negative)] = FALSE # flip these captions to false
}
any_baseline_tables = any(captions_baseline)
if(any_baseline_tables == FALSE){
  reason = 'No baseline table'
}
## find table number for baseline table
if(sum(captions_baseline) == 1){
  table_number = min(which(captions_baseline)) 
}
if(sum(captions_baseline) > 1){
  base_count = str_count(string=tolower(table_captions), pattern=words_or)
  table_number = which(base_count == max(base_count))[1] # assume first table if there is a tie 
}

## next read XML as a text file and tidy up ...
in_text = scan(file = "full.xml", what='character', sep='\n', quiet=TRUE, encoding='UTF-8') # no separators, one big text
in_text = in_text[!is.na(in_text)] # remove missing
# a) replace any breaks
in_text = str_remove_all(string=in_text, pattern='<break/>') # remove breaks
# b) remove commas, spaces and hyphens from or between numbers, see https://stackoverflow.com/questions/67329618/replacing-commas-in-thousands-millions-but-not-smaller-numbers
in_text = gsub("(?<=\\(\\d{1}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # change badly formatted numbers, e.g. "(87,105)" that is actually two numbers -- must be in brackets
in_text = gsub("(?<=\\(\\d{2}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number, have to increment because of `look-backwards`
in_text = gsub("(?<=\\(\\d{3}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number
in_text = gsub("(?<=\\(\\d{4}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number
in_text = gsub("(?<=\\(\\d{5}),(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # same as above with larger number
in_text = gsub("(?<=\\(\\d{1})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE) # as above for hyphen
in_text = gsub("(?<=\\(\\d{2})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub("(?<=\\(\\d{3})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub("(?<=\\(\\d{4})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub("(?<=\\(\\d{5})-(?=\\d{1,}\\))", ' ', in_text, perl=TRUE)
in_text = gsub(",(?=\\d{3,})", "", in_text, perl = TRUE) # strip commas in numbers
in_text = gsub("(?<=\\d{1})\\W(?=000)", '', in_text, perl=TRUE) # strip spaces (including special spaces) in thousands (space before 000). \W is anything that isn't a letter, digit, or an underscore
# c) replace `high` decimal places used by Lancet
in_text = gsub("·", ".", in_text, perl = FALSE) 
in_text = gsub("·", ".", in_text, perl = FALSE) 
# d) replace negative signs with hyphens (else they get cut as unicode)
in_text = gsub(pattern='\u2212', replacement='-', in_text) # see https://stackoverflow.com/questions/67830110/replacing-non-ascii-dash-with-hyphen-in-r
# e) replace N/A as slash gets confused with number
in_text = gsub(pattern='N/A|n/a', replacement=' ', in_text) #
# f) remove formatting that impacts on searching in captions
in_text = str_remove_all(string=in_text, pattern=' toggle="yes"') # appears in italic
in_text = str_remove_all(string=in_text, pattern='<italic>') # 
in_text = str_remove_all(string=in_text, pattern='</italic>') # 
in_text = str_remove_all(string=in_text, pattern='<bold>') # 
in_text = str_remove_all(string=in_text, pattern='</bold>') # 
in_text = str_remove_all(string=in_text, pattern='<sup>') # 
in_text = str_remove_all(string=in_text, pattern='</sup>') # 
in_text = str_remove_all(string=in_text, pattern='<sub>') # 
in_text = str_remove_all(string=in_text, pattern='</sub>') # 
# g) convert dates to numbers (so they can be processed as summary stats)
dates_index = str_detect(in_text, pattern=dates_patterns) # find places of dates
if(any(dates_index) == TRUE){
  for (location in which(dates_index)){
    places = str_locate_all(string=in_text[location], pattern=dates_patterns)[[1]] # now find specific locations in paragraphs
    dates = NULL
    for (i in 1:nrow(places)){  # first find locations
      date = str_sub(in_text[location], places[i,1], places[i,2])
      dates = c(dates, date)
    }
    # now replace (can't do together because locations get mucked up)
    for (date in unique(dates)){
      daten = as.character(as.numeric(as.Date(parse_date_time(date, orders = c('mdy', 'dmy', 'ymd'), quiet = TRUE)))) # do not flag errors
      if(is.na(daten)==FALSE){ # only replace if there's a valid date
        #cat(date,', ', daten, '\n', sep='')
        in_text[location] = str_replace_all(in_text[location], date, daten) # replace date with number
      }
    }
  }
}
# h) remove hyphens that are not negative signs
in_text = gsub("-\\W(?=\\d{1})", " ", in_text, perl = TRUE) # things like "22- 33"; hyphen, any non-numeric/letter char (\W), then number

## extract just the tables using the simplified table captions
# first create simple version of in_text
#in_text= in_text %>% str_replace_all(pattern='&lt;', replacement = '<') # not working
simple_in_text = tolower(in_text) %>%
  str_replace_all('&amp;', '&') %>% 
  str_replace_all('&gt;', '>') %>% # must be a better way, but for now just removing special characters as they come!
  str_replace_all('&lt;', '<') %>%
  str_remove_all("[^0-9|a-z| ]") %>% # remove all special characters to make matching below easier
  str_squish()
tables_in_text = list()
for (j in which(captions_baseline)){ # just export tables that are possible baseline tables
  end = ifelse(nchar(table_captions[j])>50, 50, nchar(table_captions[j]))
  short = str_sub(table_captions[j], 1, end)
  tables_caption_index = str_detect(simple_in_text, pattern=short) #
  # find closest start of table to caption (can be multiple results on both sides)
  tables_start_index = which(str_detect(in_text, pattern='\\<table-wrap '))
  tables_start_index_closest = expand.grid(tables_start_index, which(tables_caption_index)) %>%
    mutate(diff = Var2 - Var1) %>%
    filter(diff >= 0) %>% # caption after table start
    arrange(diff) %>%
    slice(1) %>% # take smallest difference
    pull(Var1)
  # find closest end of table to caption (can be multiple results on both sides)
  tables_end_index = which(str_detect(in_text, pattern='\\</table-wrap\\>'))
  tables_end_index_closest = expand.grid(tables_end_index, which(tables_caption_index)) %>%
    mutate(diff = Var2 - Var1) %>%
    filter(diff <= 0) %>% # caption after table start
    arrange(desc(diff)) %>%
    slice(1) %>% # take smallest difference
    pull(Var1)
  #  
  to_add = tables_start_index_closest:tables_end_index_closest
  tables_caption_index[to_add] = TRUE  # add on more text as table might be split
  text_to_work_with = paste(in_text[tables_caption_index], collapse='\n')
  tables_start_index = str_locate(text_to_work_with, pattern='\\<table-wrap ') # 
  tables_end_index = str_locate(text_to_work_with, pattern='\\</table-wrap\\>') # 
  tables_in_text[[j]] = substr(text_to_work_with, tables_start_index[1], tables_end_index[2])
}
## Write just the tables to external document 
to_write = paste(in_text[1], '\n<just-tables>\n', # add top row and create overall node
                 paste(tables_in_text, collapse = '\n'),
                 '\n</just-tables>', sep='')
just_tables_xml = tryCatch(as_xml_document(read_html(to_write)),
                           error = function(e) print('XML error for tables'))
xml2::write_xml(just_tables_xml, file='just_tables.xml', encoding = "UTF-8")

## read in just tables into R
just_tables = read_html("just_tables.xml", encoding='UTF-8') 
## check table is not mostly text (usually a 'what this paper adds study')
old_table_number = table_number # needed for text search for p-values
table_number = exclude_text_tables(just_tables, table_number=1) # restart table number at one because non-baseline tables have been dropped
if(table_number == 998 | table_number == 999){ # function above also checks for availability
  reason = 'No baseline table'
}
# update old table number used for searching text
old_table_number = old_table_number + (table_number-1)
# get table footnotes
tabs = xml_find_all(just_tables, ".//table-wrap")
footnotes = rep('', length(tabs))
for (l in 1:length(tabs)){ # need to loop through tables because some tables have no footnotes
  any_foot = xml_find_all(tabs[[l]], ".//table-wrap-foot") %>% xml_text() 
  if(length(any_foot)>0){footnotes[l] = paste(any_foot, collapse=' ')}
}
footnote = footnotes[table_number] # get the table footnote for checking p-values

## exclude if follow-up data in table
table_captions = table_captions[which(captions_baseline)] # just keep baseline tables
caption = table_captions[table_number] #
follow_up = str_detect(string=tolower(caption), pattern = fu_pattern)
if(follow_up == TRUE){
  reason = 'Follow-up results in baseline table'
}


# function that does heavy lifting, changed to source instead of function for shiny app
webpage = just_tables
weight = 2
source('main_function.R', local = environment())
if(class(results) == 'character'){
  reason = results$reason
}

# tidy up
file.remove('full.xml')
file.remove('just_tables.xml')

# change to match format from Excel file
continuous = filter(results$table, statistic == 'continuous') 
if(nrow(continuous)>0){
  g1 = filter(continuous, column == 1) %>% # first group
  rename('n1' = 'sample_size',
         'm1' = 'stat1',
         'sd1' = 'stat2') %>%
  select('row','n1','m1','sd1')
  #
  g2 = filter(continuous, column == 2) %>% # first group
    rename('n2' = 'sample_size',
           'm2' = 'stat1',
           'sd2' = 'stat2') %>%
    select('row','n2','m2','sd2')
  # could add third group ..
  continuous = full_join(g1, g2, by='row') %>%
    mutate(v1 = as.character(row)) # dummy for variable names
}
percent = filter(results$table, statistic == 'percent')
if(nrow(percent)>0){
  g1 = filter(percent, column == 1) %>% # first group
    rename('N1' = 'sample_size',
           'n1' = 'stat1') %>%
    select('row','n1','N1')
  #
  g2 = filter(percent, column == 2) %>% # first group
    rename('N2' = 'sample_size',
           'n2' = 'stat1') %>%
    select('row','n2','N2')
  # could add third group ..
  percents = full_join(g1, g2, by='row') %>%
    mutate(v1 = as.character(row))# dummy for variable names
}

# null if no rows
if(nrow(continuous)==0){continuous = NULL}
if(nrow(percents)==0){percents = NULL}

# return data with percents and continuous
data = list()
data$continuous = continuous
data$percents = percents
data$reason = reason
data$original_table = results$table
return(data)
} # end of function
