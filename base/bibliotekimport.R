library(shiny)
library(shinyFiles)
library(tidyverse)
library(openxlsx)
library(readxl)
library(officer)
library(mschart)
library(stringr)
library(janitor)
# library(XLConnect)
# library(gtools) #usikker på om denne gjer noko

# importer SB_prepare-funksjonar og variabelkoding
source("base/Studentsvar_prepare_tools.R", encoding = "UTF-8")
# importer utskriftskode
source("base/Studentsvar_report.R", encoding = "UTF-8")