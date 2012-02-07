# Analysis for Harewood 2009 experiment (harvested in 2011)
# Luis A. Apiolaza
# School of Forestry
# University of Canterbury

# libraries
require(lattice)
require(Hmisc)
require(asreml)

# options and default working directory
options(stringsAsFactors = FALSE)

setwd('~/Documents/Research/2012/harewood/') # Change this for your machine

# Reading the original file without the need of getting rid of the
# additional columns summarizing data
#opp <- scan('Harewood2011Oppo.csv', sep = ',', skip = 1, flush = 1,
#            what = list(col = '',  row = 0, wood.type = '', clone = 0, dia.along = 0, dia.across = 0, 
#                        gav = 0, gmass = 0, gvol = 0, gld = 0, gcurve = 0, remarks ='', 
#                        dav = 0, dmass = 0, dvol = 0, dld = 0, dcurve = 0, empty = 0,
#                        mcont = 0, gden = 0, dden = 0, bden = 0, gmoe = 0, dmoe = 0,
#                        lshr = 0, vshr = 0))
#opp <- as.data.frame(opp)

#### Reading Harewood opposite wood data ####
opp <- read.csv('selected-samples.csv', header = FALSE, skip = 1)
names(opp) <- Cs(col, row, wood.type, clone, status, dia.along, dia.across,
                 gav, gmass, gvol, gld, gcurve, remarks, dav, dmass, dvol,
                 dld, dcurve, moist, gden, dden, bden, gmoe, dmoe,
                 lshr, vshr)
head(opp)
summary(opp)

# Replicate Z is an afterthought that can be dropped, clones would be
# easier as character
opp <- subset(opp, col != 'Z')
opp$clone <- as.character(opp$clone)
names(opp)[1] <- 'rep'

#### Reading code translation 2 ~ 3 digits
code <- read.csv('code-translation.csv', header = TRUE)
code$FGclone <- as.character(code$FGclone)
code$Harewood <- as.character(code$Harewood)
code$GoldenDowns00 <- as.character(code$GoldenDowns00)
names(code)[5] <- 'TENclone'

#### Summary statistics and plots ####
summary(opp)

boxplot(dav ~ clone, data = opp)
boxplot(lshr ~ clone, data = opp)


# This looks pretty bad. Likely explanation is that we have commingling
# of compression and opposite wood due to too much leaning.
xyplot(lshr ~ dav, data = opp)
with(opp, cor(lshr, dav, use = 'complete.obs'))
# -0.17

# Need to identify samples with commingling to drop here
# use status variable
opp.in <- subset(opp, status == 'IN')
opp.in$clone <- factor(opp.in$clone)
opp.in$rep <- factor(opp.in$rep)
 
xyplot(lshr ~ dav, data = opp.in)
with(opp.in, cor(lshr, dav, use = 'complete.obs'))
# -0.29

#### Simple analysis for meeting in Auckland ####
# Only 'good' trees
dmoe.asr <- asreml(dmoe ~ 1, random = ~ rep + clone, data = opp.in)
summary(dmoe.asr)

# Extracting genetic values
dmoe.gv <- data.frame(coef(dmoe.asr, pattern = 'clone'))
dmoe.gv$clone <- apply(data.frame(rownames(dmoe.gv)), 1, 
                         FUN = function(x) unlist(strsplit(x, '_'))[2])

dmoe.gv <- merge(dmoe.gv, code[,c(1:2, 5),], by.x = 'clone', by.y = 'Harewood')

dmoe.gv[order(dmoe.gv$effect, decreasing = TRUE),]

# All trees, 'good' and 'bdd'
all <- opp
all$clone <- factor(all$clone)
all$rep <- factor(all$rep)

dav.u2 <- asreml(dav ~ 1, random = ~ rep + clone, data = all)
summary(dav.u2)

dav.gv2 <- data.frame(coef(dav.u2, pattern = 'clone'))
dav.gv2$clone <- apply(data.frame(rownames(dav.gv2)), 1, 
                      FUN = function(x) unlist(strsplit(x, '_'))[2])

dav.gv2 <- merge(dav.gv2, code[,c(1:2, 5),], by.x = 'clone', by.y = 'Harewood')

dav.gv2[order(dav.gv$effect, decreasing = TRUE),]





# Meanwhile let's go for simple univariate analyses
# using lme4. Suffix .u stands for univariate
gav.u <- lmer(gav ~ 1 + (1 | rep) + (1 | clone), data = opp)
summary(gav.u)
gav.uvc <- VarCorr(gav.u)
gav.uh2 <- gav.uvc$clone[1]/(gav.uvc$clone[1] + gav.uvc$rep[1])
