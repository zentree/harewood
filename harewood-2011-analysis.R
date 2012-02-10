# Analysis for Harewood 2009 experiment (harvested in 2011)
# Luis A. Apiolaza
# School of Forestry
# University of Canterbury

# libraries
require(ggplot2)
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
opp <- read.csv('selected-samples-final.csv', header = FALSE, skip = 1)
names(opp) <- Cs(col, row, wood.type, clone, status, harv.time, dia.along, 
                 dia.across, gav, gmass, gvol, gld, gcurve, sample.length,
                 time, fakopp.vel, remarks, dav, 
                 dmass, dvol, dld, dcurve, moist, gden, dden, bden, gmoe, 
                 dmoe, lshr, vshr, new.moe, new.moe.comments)
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

# There are some outliers
qplot(new.moe, lshr, data = opp)
opp <- subset(opp, new.moe > 2) # This drops a single observation
qplot(new.moe, lshr, data = opp)

with(opp, cor(lshr, new.moe, use = 'complete.obs'))
# -0.41

# Sort out factors
opp$rep <- factor(opp$rep)
opp$clone <- factor(opp$clone)
opp$harv.time <- factor(opp$harv.time)


#### Simple analysis for meeting in Auckland ####
moe.asr <- asreml(new.moe ~ harv.time, random = ~ rep + clone, data = opp)
summary(moe.asr)

#### Crap! RUn out of ASReml license in uni desktop; I'll fit this with lme4
# for today.
require(lme4)
moe.lme4 <- lmer(new.moe ~ harv.time + (1|rep) + (1|clone), data = opp)
summary(moe.lme4)

moe.gv <- data.frame(ranef(moe.lme4)$clone)
moe.gv$clone <- rownames(moe.gv)
names(moe.gv)[1] <- 'effect'
#####

# $loglik
# [1] 110.2819
# 
# $varcomp
#                      gamma   component   std.error    z.ratio constraint
# rep!rep.var     0.01653934 0.003173325 0.003921593  0.8091928   Positive
# clone!clone.var 0.95934730 0.184065425 0.062381329  2.9506493   Positive
# R!variance      1.00000000 0.191865265 0.013248538 14.4819956   Positive

# Heritability: 0.184065425/(0.184065425+0.003173325+0.191865265) = 0.486

# Extracting genetic values
moe.gv <- data.frame(coef(moe.asr, pattern = 'clone'))
moe.gv$clone <- apply(data.frame(rownames(moe.gv)), 1, 
                         FUN = function(x) unlist(strsplit(x, '_'))[2])

moe.gv <- merge(moe.gv, code[,c(1:2, 5),], by.x = 'clone', by.y = 'Harewood')
moe.gv <- subset(moe.gv, !is.na(TENclone))
moe.gv$effect <- moe.gv$effect + coef(moe.asr, pattern = 'Intercept')
moe.gv$class <- factor(cut(moe.gv$effect, breaks = c(0, 4.35, 4.72, 6), 
                       labels = c('bottom', 'middle', 'top')))

moe.gv[order(-moe.gv$effect),]

# Color-blind-friendly palette
# http://wiki.stdout.org/rcookbook/Graphs/Colors%20(ggplot2)/
cbfPalette <- scale_colour_manual(values=c("#D55E00", "#000000", "#56B4E9"))

moe.plot <- ggplot(moe.gv, aes(x = 1, y = effect, colour = class, label = TENclone)) 
moe.plot <- moe.plot + geom_text(size = 3) + scale_y_continuous('MoE (GPa)') +
            scale_x_continuous('') + cbfPalette +
            opts(axis.title.y = theme_text(size = 12, angle = 90),
                 axis.text.y = theme_text(size = 10, colour = 'black'),
                 axis.text.x = theme_text(colour = 'white'),
                 legend.position = 'none')

moe.plot

pdf('moe-ranking-plot.pdf', width = 2, height = 7)
moe.plot
dev.off()

save(moe.gv, file='newmoe-genetic-values.Rdata')