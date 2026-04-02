# Info ----------------------------------------------------------------------

# Title:  DLC Joins Presentation 
# By: Meredith Davis & Travis Oishi 
# Date created: 04/02/2026
# R version: 4.3.0 "Already Tomorrow"


# Instructions -----------------------------------------------------------------


# The code below imports a deidentified disease dataset for the Central 
# Shenandoah Health District from the Virginia Electronic Disease Surveillance 
# System (VEDSS) and a simulated outbreak line list. 


# Learning Objectives:
#     1. Determine the appropriate type of R join to use in a given situation
#     2. Join VEDSS and line list datasets using appropriate R syntax
#     3. Create summary statistics based on the joined dataset
#     4. Create a visualization using ggplot2 to highlight one aspect of the joined data 
#       
# ****************** ADD DATA DICTIONARY LINES HERE




# Set-up --------------------------------------------------------------------

## Install packages (only necessary for the first use of package) -----------

install.packages("tidyverse") # includes ggplot2, dplyr, stringr, tidyr, lubridate
install.packages("readxl") # import excel documents
install.packages("writexl") # export excel documents


## Load packages -----------------------------------------------------------

library(tidyverse)            # QUALITY OF LIFE IMPROVEMENTS
library(readxl)               # IMPORT EXCEL DOCUMENTS
library(writexl)              # EXPORT EXCEL DOCUMENT
library(janitor)              # DATA CLEANING 


# Import data -------------------------------------------------------------

# Note: You will need to edit the file paths below to point to the location you have saved each file

# VEDSS Data
R_study_group_VEDSS_Deidentified <- read_excel("Test/DLC/R_study_group_VEDSS_Deidentified3.xlsx")

# Line List
R_study_group_line_list<-read_excel("Test/DLC/R_study_group_line_list_v2.xlsx")


# Create a copy of each dataset for editing

VEDSS <- R_study_group_VEDSS_Deidentified

Line_list <- R_study_group_line_list


# Explore data ------------------------------------------------------------

# Take a quick look at the contents of each dataset. Below are some options to view data

## Rows by columns, column names and data type, first few cells of each column

str(VEDSS)
str(Line_list)

## Names of all columns

names(VEDSS)
names(Line_list)

## Top few rows of data

head(VEDSS)
head(Line_list)



# Clean data --------------------------------------------------------------

# Trim white space, make all character variables uppercase, filter data
VEDSS <- VEDSS %>% 
  mutate_if(is.character, trimws) %>%             # Remove extra white spaces
  mutate_if(is.character, toupper) %>%            # Convert character strings to all caps
  mutate(Source_V = "VEDSS",                      # Create a column indicating data source
         Age=as.integer(Age)) %>%                 # Save age column as an integer
  filter(Case.Status == "SUSPECT"|                # Keep only suspect, probable, or confirmed cases
           Case.Status == "PROBABLE"|
           Case.Status == "CONFIRMED")                 


Line_list <- Line_list %>% 
  mutate_if(is.character, trimws) %>%             # Remove extra white spaces
  mutate_if(is.character, toupper) %>%            # Convert character strings to all caps
  mutate(Source_L = "LINE LIST",                  # Create a column indicating data source
         Age_Years =as.integer(Age_Years ))           



# YOUR TURN: Clean Data ---------------------------------------------------


# Join data ---------------------------------------------------------------
# This section contains examples of each type of join using VEDSS and Line List data

## 1:  Left, Right, and Full Joins ####

# Left - keeps all Line List records plus any joined VEDSS records
Left <- Line_list %>% 
  left_join(VEDSS, by = c("Last" = "Patient.Last.Name", 
                           "First" = "Patient.First.Name", 
                           "Age_Years" = "Age"))


# Right join - all VEDSS data plus any shared Line List records 
Right <- Line_list %>% 
  right_join(VEDSS, by = c("Last" = "Patient.Last.Name", 
                           "First" = "Patient.First.Name", 
                           "Age_Years" = "Age"))


# Full - show all matched records plus all non-matched records
Full <- Line_list %>% 
  full_join(VEDSS, by = c("Last" = "Patient.Last.Name", 
                            "First" = "Patient.First.Name", 
                            "Age_Years" = "Age")) 


## 2:  Inner Joins ####

# Inner - Keeps only those records that are shared between both datasets   
Inner <-Line_list %>% 
  inner_join(VEDSS, by = c("Last" = "Patient.Last.Name", 
                           "First" = "Patient.First.Name", 
                           "Age_Years" = "Age"))

## 3:  Semi Joins ####

# Semi - Keeps records that are shared between the datasets, but only shows VEDSS columns
Semi_VEDSS <- VEDSS %>% 
  semi_join(Line_list, by = c("Patient.Last.Name" = "Last", 
                             "Patient.First.Name" = "First", 
                             "Age" = "Age_Years"))

## 4:  Anti Joins ####

# Anti - Show records from VEDSS that are missing from Line List

VEDSS_only <- VEDSS %>% 
  anti_join(Line_list, by = c("Patient.Last.Name" = "Last", 
                               "Patient.First.Name" = "First", 
                               "Age" = "Age_Years"))



# Anti - Show records from Line List that are missing from VEDSS
Line_List_Only <- Line_list %>% 
  anti_join(VEDSS, by = c("Last" = "Patient.Last.Name", 
                           "First" = "Patient.First.Name", 
                           "Age_Years" = "Age"))




# Create a visualization --------------------------------------------------



Line_list %>% 
  # Get total cases by onset date
  group_by(Date_Onset) %>% 
  summarize(Total = n()) %>% 
  # Plot onset date vs total cases
  ggplot(aes(Date_Onset, Total))+
  # Barplot
  geom_bar(stat="identity", color = "black",fill = "olivedrab")+
  # Custom labels
  labs(title = "Line List Cases",    
       x = "Onset Date",
       y = "Number of Cases",
       fill = "Case Status")   





Line_list %>% 
  # Join to VEDSS dataset
  left_join(VEDSS, by = c("Last" = "Patient.Last.Name", 
                          "First" = "Patient.First.Name", 
                          "Age_Years" = "Age")) %>% 
  # Get total cases by onset date and case status
  group_by(Date_Onset,Case.Status) %>% 
  summarize(Total = n()) %>% 
  # Plot onset date vs total cases vs case status
  ggplot(aes(Date_Onset, Total, fill = Case.Status))+
  # Barplot
  geom_bar(stat="identity", color = "black")+
  # Custom labels
  labs(title = "Line List Cases Matched to VEDSS Records by Case Status 2022-2025",    
       x = "Onset Date",
       y = "Number of Cases",
       fill = "Case Status")   

# ************** Are there cases that might be missing from the line list? **************

# Get date ranges of line list
min(Line_list$Date_Onset)
max(Line_list$Date_Onset)


VEDSS_only <- VEDSS %>% 
  # Get VEDSS cases that are not present in the line list
  anti_join(Line_list, by = c("Patient.Last.Name" = "Last", 
                              "Patient.First.Name" = "First", 
                              "Age" = "Age_Years")) %>% 
  # Filter for matches within a time frame
  filter(Week.Ending.VEDSS>="2024-06-01") %>%
  filter(Week.Ending.VEDSS<="2024-09-01") %>%
  # Filter matches for only relevant conditions
  filter(Condition =="CAMPYLOBACTERIOSIS"|
         Condition=="E. COLI INFECTION, SHIGA TOXIN-PRODUCING") %>% 
  arrange(desc(Week.Ending.VEDSS))
           
