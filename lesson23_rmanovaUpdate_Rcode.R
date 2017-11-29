# ==================================
# N736 Lesson 23 - RM-ANOVA and MLM
#
# updated 11/29/2017
# Melinda Higgins, PhD
# 
# ==================================

# ==================================
# we're be working with the 
# helpmkh dataset
# ==================================

library(tidyverse)
library(haven)

helpdat <- haven::read_spss("helpmkh.sav")

# ============================================.
# For this lesson we'll use the helpmkh dataset
#
# In the HELP dataset there are 5 time points
# baseline and 4 follow-up time points 
# at 6m, 12m, 18m and 24m
#
# for today's lesson we will be working with the PCS
# physical component score for the SF36 quality of life tool
# let's look at how these 5 PCS measurements are
# correlated across time
#
# and we'll also look at the treat group.
# ============================================.

# keep subject id also
h1 <- helpdat %>%
  select(id, treat, pcs, pcs1, pcs2, pcs3, pcs4)

# compute number of missing values
# across the 5 time points of cesd measurements
h1$nmiss_pcs <- rowSums(is.na(h1[,3:7]))
table(h1$nmiss_pcs)

# see http://psych.wisc.edu/moore/Rpdf/610-R11_MixedDesign.pdf
# keep only the 98 cases with complete data
# where nmiss_cesd equals 0 (none missing)
h198 <- h1 %>%
  filter(nmiss_pcs==0)

# to do these analyses in R, we first
# have to reshape the data from WIDE
# to long

h198long <- h198 %>%
  gather(key=item,
         value=value,
         -c(id,treat,nmiss_pcs))

# add a time variable to long format
h198long <- h198long %>%
  mutate(time=c(rep(0,98),
                rep(1,98),
                rep(2,98),
                rep(3,98),
                rep(4,98)))

# we can use the aov() function but we
# need to first convert these to factors

h198long$id <- factor(h198long$id) # subject id
h198long$treat <- factor(h198long$treat) # group - between factor
h198long$time <- factor(h198long$time) # time - within factor

# rename the variables
names(h198long) <- c("id","treat","nmiss_pcs",
                   "pcsitem","pcsvalue","time")

m2 <- aov(pcsvalue ~ treat*time + Error(id/time), 
          data=h198long) 
summary(m2)

# compare this output to SAS and SPSS output
