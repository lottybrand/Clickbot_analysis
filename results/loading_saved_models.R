
# read the models

##### Pre-registered models:

# model 1: effect of condition on likelihood of taking vaccine in the future

model1 <- readRDS("../model_objects/model1.rds")
precis(model1)
precis(model1)[2,1]
precis(model1)[2,3]
precis(model1)[2,4]

# model nos: effect of experiment in general on likelihood of taking vaccine

modelnos <- readRDS("../model_objects/model_nos.rds")
precis(modelnos)

# model 2: best fitting model for looking at attitude change. single experiment effect stronger than interaction

h2_exp <- readRDS("../model_objects/h2_exp.rds")
precis(h2_exp)

# model 2 interaction: no interaction between condition and time

h2_int <- readRDS("../model_objects/h2_int.rds")
precis(h2_int)

# model 3: no diff in conditions in engagement

h3_model <- readRDS("../model_objects/h3_model.rds")
precis(h3_model)

##### Exploratory: 

# model1.1 is there a condition effect on vaccination intention for those who spent the most time with the info? no

model1.1 <- readRDS("../model_objects/model1.1_timing.rds")
precis(model1.1)

# model1.2 is there an effect of time spent on intention to get vaccinated? no

model1.2 <- readRDS("../model_objects/model1.2_timing.rds")
precis(model1.2)

# h2_full_time - there is an interaction between most time spent and the experiment effect, 
# in that vaccination attitudes increase after seeing the info, but increase more in those who spent most time with the info

h2_full_time <- readRDS("../model_objects/model_h2_full_time.rds")
h2_int_time <- readRDS("../model_objects/model_h2_int_time.rds")
h2_time <- readRDS("../model_objects/model_h2_time.rds")

precis(h2_full_time)

# load rest of the h2 models for WAIC comparison 
h2_full <- readRDS("../model_objects/h2_full.rds")
h2_null <- readRDS("../model_objects/h2_null.rds")


