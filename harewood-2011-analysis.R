options(stringsAsFactors = FALSE)

setwd('~/Documents/Research/2012/harewood/')


#,Green AV (km/s),Green mass (grms),Green vol. (grms),Green   LD     (mm),Green curve (mm),Remarks,Dry AV (km/s),Dry mass (grms),Dry vol. (grms),Dry LD     (mm),Dry curve (mm),,Moisture content (%),Green density (kg/m3),Dry density (kg/m3),Basic density (kg/m3),Green MOE (Gpa),Dry MOE (Gpa),Lon. Shr. (%),Vol. shr. (%),,,Green av,Dry Av,Moisture content (%),Green density (kg/m3),Dry density (kg/m3),Basic density (kg/m3),Green MOE (Gpa),Dry MOE (Gpa),Lon. Shr. (%),Vol. shr. (%)

opp <- scan('Harewood2011Oppo.csv', sep = ',', skip = 1, flush = 1,
#                       Col           Row       Wood Type   Clone     Along lean   Across
            what = list(col = '',  row = 0, wood.type = '', clone = 0, dia.along = 0, dia.across = 0))
opp <- as.data.frame(opp)