# Merging Harewood and Clones10 data

require(lattice)

# This loads moe.gv (Harewood) and clone.thr (older trials)
load('~/Documents/Research/2012/harewood/newmoe-genetic-values.Rdata')
load('~/Documents/Research/2012/clones10/GeneticValues15Clones.Rdata')

clone10.gv <- data.frame(clone = clone.thr$clone[1:15],
                         goldgv = clone.thr$effect[1:15],
                         waitgv = clone.thr$effect[16:30])
clone10.gv$gvacross = with(clone10.gv, (goldgv + waitgv)/2)

compare <- merge(moe.gv, clone10.gv, by.x = 'TENclone', by.y = 'clone')

xyplot(gvacross ~ effect, data = compare)
with(compare, cor(gvacross, effect))

xyplot(gvacross ~ effect, data = compare, 
       subset = !(TENclone %in% c('125', '126')))

with(compare[!(compare$TENclone %in% c('125', '126')),], cor(gvacross, effect))
