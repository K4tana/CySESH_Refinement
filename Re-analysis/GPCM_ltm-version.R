##Reanalysis of Borgert et al. (2023)
## This script will re-analyse validation data from Borgert et al. to test suitable items and IRT models with a state-of-the-art approach. 

#Required packages
pkg <- c("ltm", "tidyverse", "psych")
for (i in pkg){
  library(i, character.only = T)
}
set.seed(1337)
#gpcm
#load processed data directly from the github repository of Borgert et al. (2023)
rdata <- read_csv("https://raw.githubusercontent.com/neleborgert/CySESH-scale/main/main%20study/data/processed/data_processed.csv?token=GHSAT0AAAAAACIBKYC3G35XY74HSOXHPWJUZISEX5A")

#specify items
items <- c(8:19)

#fit gpcm
fit_g <- gpcm(rdata[items], constraint = "gpcm")

summary(fit_g)
GoF.gpcm(fit_g, seed = 1337) #chi-square significant at 0.05 (p=0.01)

#factor scores
fsc <- factor.scores.gpcm(fit_g)

#descriptives
desc <- descript(rdata[items])

#plots
plot(fit_g, type = "ICC")
plot(fit_g, type = "IIC")
plot(fit_g, type = "OCCu")
plot(fit_g, type = "OCCl")
plot(fsc)

#average discrimination of all items. Select below and at that point.
dscrmean <- lapply(fit_g$coefficients, "[[", 7) |> as.data.frame() |> rowMeans()
dscrmean

#problem: ltm isnt majorly supportive of gpcm models, lots of plots and functions are not supported. trying mirt next.
