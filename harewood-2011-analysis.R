# Analysis for Harewood 2009 experiment (harvested in 2011)
# Luis A. Apiolaza
# School of Forestry
# University of Canterbury

# libraries
require(lattice)
require(asreml)
require(lme4)

# options and default working directory
options(stringsAsFactors = FALSE)

setwd('~/Documents/Research/2012/harewood/') # Change this for yur machine

# Reading the original file without the need of getting rid of the
# additional columns summarizing data
opp <- scan('Harewood2011Oppo.csv', sep = ',', skip = 1, flush = 1,
            what = list(col = '',  row = 0, wood.type = '', clone = 0, dia.along = 0, dia.across = 0, 
                        gav = 0, gmass = 0, gvol = 0, gld = 0, gcurve = 0, remarks ='', 
                        dav = 0, dmass = 0, dvol = 0, dld = 0, dcurve = 0, empty = 0,
                        mcont = 0, gden = 0, dden = 0, bden = 0, gmoe = 0, dmoe = 0,
                        lshr = 0, vshr = 0))
opp <- as.data.frame(opp)
summary(opp)

# There are a few empty extra rows at the end of the file
opp <- subset(opp, !is.na(row))
str(opp)

# In fact, 'rows' are the replicates, clones are a factor
opp$rep <- factor(opp$row)
opp$clone <- factor(opp$clone)
summary(opp)

# Summary statistics and plots
summary(opp)

boxplot(dav ~ clone, data = opp)
boxplot(lshr ~ clone, data = opp)

# This looks pretty bad. Likely explanation is that we have commingling
# of compression and opposite wood due to too much leaning.
xyplot(lshr ~ dav, data = opp)

# Need to identify samples with commingling to drop here


# Meanwhile let's go for simple univariate analyses
# using lme4. Suffix .u stands for univariate
gav.u <- lmer(gav ~ 1 + (1 | rep) + (1 | clone), data = opp)
summary(gav.u)
gav.uvc <- VarCorr(gav.u)
gav.uh2 <- gav.uvc$clone[1]/(gav.uvc$clone[1] + gav.uvc$rep[1])