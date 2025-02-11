# global.R
source('99_functions.R')
library(shiny)
library(shinyFiles) # for saving XML files
library(ggplot2)
library(readxl) # for reading Excel
library(dplyr)
library(tidyr)
library(stringr)
library(nimble)
library(metareadr) # for full text, download library from github
library(rvest)
library(xml2)
source('get_pmcid_table.R') # for full text extraction
source('1_pattern.R') # helps with full text extraction
library(openxlsx)
