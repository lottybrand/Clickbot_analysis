##########################################################
##                       START HERE                     ##
##
## This script organises the project. We borrow the organisation scheme from Claudia von Bastian's R workshops
## http://www.claudiavonbastian.com
##
## But notes on the general idea are here
## https://tomstafford.github.io/psy6422/project-organisation.html
##
##########################################################


##########################################################
##                       LIBRARIES                      ##
##########################################################

# needed for analysis script only:
library(rethinking)

# needed for plotting script only:
library(tidyverse)
library(plyr)
library(ggplot2)
library(gghalves)
library(ggpubr)
library(here)

##########################################################
##                       SETTINGS                       ##
##########################################################

# set working directory to project directory


##########################################################
##                      PROCESSING                      ##
##########################################################

#to update when we have organised the scripts, but they'll be something like

# 01: Prepare --------------------------------------------
# load and merge the data files
# source(paste(workspace, "1_prepare.R", sep = "/"))

# 02: Analyse --------------------------------------------
# extract and summarise the relevant variables from the data
# source(paste(workspace, "2_process.R", sep = "/"))

# 03: Supplementary file ---------------------------------
# explore and analyse the data
# source(paste(workspace, "3_analyse.R", sep = "/"))
