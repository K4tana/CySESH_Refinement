##Reanalysis of Borgert et al. (2023)
## This script will re-analyse validation data from Borgert et al. to test suitable items and IRT models with a state-of-the-art approach. 
#install the ggmirt package from github: devtools::install_github("pkmasur/ggmirt")
#Required packages
pkg <- c("mirt", "tidyverse", "psych","ggmirt")
for (i in pkg){
  library(i, character.only = T)
}
set.seed(1337)
#gpcm
#load processed data directly from the github repository of Borgert et al. (2023)
rdata <- read_csv("Re-analysis/data_processed.csv")

#specify items
items <- c(8:19)

#fit unidimensional gpcm with mh algorithm (performs well with large amounts of data)
fit_g <- mirt(rdata[items], model=1, itemtype = "gpcmIRT", method="MHRM", verbose = T)

#analysis
fit_g
smr <- summary(fit_g)$h2 |> as.data.frame()
ice <- coef(fit_g, IRTpars =T, simplify =T)|> as.data.frame()
M2(fit_g, type = "C2", calcNull = F) #fit stats for model. similar to Borgert et al.
itf <- itemfit(fit_g) #this already eliminates patterns_4, s_x2 is significant at 0.01 level
plot(fit_g)

#infit and outfit stats 
infi <- itemfit(fit_g, fit_stats = "infit") |> as.data.frame()#all look fine here. Ideal is infit unter 1, this means better discrimination properties.
itemfitPlot(fit_g)

#personfit
personfit(fit_g) |> 
  reframe(infit.outside = prop.table(table(z.infit > 1.96 | z.infit < -1.96)),
          outfit.outside = prop.table(table(z.outfit>1.96 | z.outfit < -1.96))) 
# lower row = non-fitting people. We have high abberrant rates in our non-fitting persons. usually 5% is highest acceptable value. Not sure why this is. Seems like a function of the scale parameters (7 point likert)
# 
#factorscores (stimated)
fscores(fit_g) |> as.data.frame() |> ggplot(aes(F1))+geom_density(bw=0.5) #smoothed density of factor scores for people. We have fat tails. This means, to refine the scale we need more difficult items that are less sensitive at extremes as those kill our scope. at least I assume that to be true. 
fscores(fit_g) |> density() |> plot() # un-smoothed KD shows a few very extreme ends. outlier problem?

#Option characteristic curves for items. Flagged = bad.
itemplot(fit_g, item = 1)  #flagged
itemplot(fit_g, item = 2)  #flagged
itemplot(fit_g, item = 3)  #flagged
itemplot(fit_g, item = 4)  #flagged
itemplot(fit_g, item = 5)  #good
itemplot(fit_g, item = 6)  #flagged
itemplot(fit_g, item = 7)  #really good
itemplot(fit_g, item = 8)  # good
itemplot(fit_g, item = 9)  # good
itemplot(fit_g, item = 10) #flagged
itemplot(fit_g, item = 11) #flagged
itemplot(fit_g, item = 12) #flagged

#item information: 
itemInfoPlot(fit_g, facet = T)
#absolute garbage according to II: 6,10,8,12
#worth considering according to II: 1,11,3,5,7,9

#test information
testInfoPlot(fit_g, adj_factor = 2)

#final item judgement criteria in one df.
itemjudge <- data.frame(
  item = row.names(ice),
  communality = round(smr$h2, digits = 2),
  p.S_x2 = round(itf$p.S_X2, digits = 2),
  discr=round(ice$items.a1, digits = 2),
  infit= round(infi$infit, digits = 2)
)
itemjudge
#So far, looks like 5,7 and 9 have the best possible values for selection.

#To do: Decide where we want to go in terms of theta sensitivity: Do we want fatter tails in the short measure? Then we can integrate an item that has flat IIF with two others that have clear peaks at midrange. If we want less sensitivity in the extremes, we can have three in midrange etc. Defining the desired target distribution of TIF is paramount here. 

#DIF
#cut down dataframe to fit the command
difdat <- rdata[,c(items,4)]
fit_dif <- multipleGroup(difdat, model = 1, group = "gender", method = "MHRM")