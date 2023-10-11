##Reanalysis of Borgert et al. (2023)
## This script will re-analyse validation data from Borgert et al. to test suitable items and IRT models with a state-of-the-art approach.

#install the ggmirt package from github if needed: 
#devtools::install_github("pkmasur/ggmirt")

#Required packages
pkg <- c("mirt", "tidyverse","ggmirt")
for (i in pkg){
  library(i, character.only = T)
}
set.seed(1337)
#gpcm
#load processed data directly from the github repository of Borgert et al. (2023)
rdata <- read_csv("Re-analysis/data_processed.csv")

#specify items
items <- c(8:19)

#cut down dataframe to fit the model: remove low represented gender options to make the models identifiable.
rdata <- rdata[-which(rdata$gender=="Non-binary / third gender"),]
rdata <- rdata[-which(rdata$gender=="Prefer not to say"),]

#fit unidimensional gpcm with mh algorithm (performs well with large amounts of data)
fit_g <- mirt(rdata[items], model=1, itemtype = "gpcmIRT", method="MHRM", verbose = T)

#fit a rsm (rating scale model, muraki-type) for comparison
fit_r <- mirt(rdata[items], model=1, itemtype = "grsm", method="MHRM", verbose = T)
summary(fit_r)
#fit a gpcmIRT model for comparison
fit_gi <- mirt(rdata[items], model = 1, itemtype = "gpcmIRT", method = "MHRM", verbose = T)
summary(fit_gi)
#compare the models.
mirt::anova(fit_r,fit_g,fit_gi)#rsm fits best.

#GPCM MODEL ANALYSIS
fit_g
smr <- summary(fit_g)$h2 |> as.data.frame()
ice <- coef(fit_g, IRTpars =T, simplify =T)|> as.data.frame()
M2(fit_g, type = "C2", calcNull = F) #fit stats for model. similar to Borgert et al.
itf <- itemfit(fit_g) #this already eliminates patterns_4, s_x2 is significant at 0.01 level. NOT SURE IF THIS IS OK TO USE THOUGH...measure was developed for dichotomous IRT models. 
plot(fit_g)

#infit and outfit stats 
infi <- itemfit(fit_g, fit_stats = "infit") |> as.data.frame()#all look fine here. Ideal is infit unter 1, this means better discrimination properties.
itemfitPlot(fit_g)

#personfit
personfit(fit_g) |> 
  reframe(infit.outside = prop.table(table(z.infit > 1.96 | z.infit < -1.96)),
          outfit.outside = prop.table(table(z.outfit>1.96 | z.outfit < -1.96))) 
# lower row = non-fitting people. We have high abberrant rates in our non-fitting persons. usually 5% is highest acceptable value. Not sure why this is. Might be a funciton of scaling?
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

#Differential item functioning (DIF)
#build the two groups model. I assume that items with a similar factor loading (from previous estimation) have no invariance between men and women. There is no theoretical reason for gender difference anchors, which is the preferred method of determination.
dif_gen <- multipleGroup(rdata[items], model = 1, 
                         group = rdata$gender, 
                         method = "MHRM",
                         invariance = c("cy_sesh_help1", "free_means", "free_variance"),
                         itemtype = "gpcm")

#look at the coefficients of interest
coef(dif_gen)

#apply dif checks
DIF(dif_gen, 
    which.par = c("a1"), 
    seq_stat = "BIC",
    plotdif = T, 
    simplify = T,
    verbose = T)

#does not detect DIF in items. this may be to the fact that all reference Items have DIF or gender was not really a DIF variable. find a variable in data set that does not show variance and use it as reference.

##RATING SCALE MODEL ANALYSIS
smr_r <- summary(fit_r) |> as.data.frame()
ice_r <- coef(fit_r, simplify =T)|> as.data.frame() #a = discrimination, c = guessing.
modfit_r <- M2(fit_r, type = "C2", calcNull = F) #fit stats for model. similar to Borgert et al.
itf_r <- itemfit(fit_r) #eliminates 5,7,8,11 (effectively) --> s_x2 (Itemfit index) is significant at 0.01 level
plot(fit_r)

#infit and outfit stats 
infi_r <- itemfit(fit_r, fit_stats = "infit") |> as.data.frame()#most look fine here. Close to 1 is best, Ideal is infit unter 1, this means better discrimination properties.
itemfitPlot(fit_r)

#personfit
personfit(fit_r) |> 
  reframe(infit.outside = prop.table(table(z.infit > 1.96 | z.infit < -1.96)),
          outfit.outside = prop.table(table(z.outfit>1.96 | z.outfit < -1.96))) 
# lower row = non-fitting people. We have high aberrant rates in our non-fitting persons. usually 5% is highest acceptable value. Not sure why this is. Might be a function of scaling?
# 
#factorscores (estimated)
fscores(fit_r) |> as.data.frame() |> ggplot(aes(F1))+geom_density(bw=0.5) #smoothed density of factor scores for people. We have fat tails. This means, to refine the scale to a normal distribution, we need more difficult items that are less sensitive at extremes as those kill our scope. at least I assume that to be true. Also the plot shows a slightly heavier tail on the negative side. Might be a sample problem (outliers with low SE).
fscores(fit_r) |> density() |> plot() # un-smoothed KD shows that the main bulk of factor scores is in the desired range. a few extreme end outliers can be seen at extreme ends, hightening the bandwidth. This might be a good thing, as it shows we are even able to detect those!

#Option characteristic curves for items. Flagged = bad.
itemplot(fit_r, item = 1)  #ok
itemplot(fit_r, item = 2)  #flagged: skewed to negative theta
itemplot(fit_r, item = 3)  #flagged: skewed to negative theta.
itemplot(fit_r, item = 4)  #flagged: skewed to positive theta.
itemplot(fit_r, item = 5)  #ok, lower difficulty.
itemplot(fit_r, item = 6)  #ok, but more problematic, low discrimination on the upper end.
itemplot(fit_r, item = 7)  #really good
itemplot(fit_r, item = 8)  #flagged, probability of extreme ends is higher than category before (overshadowing extremes).
itemplot(fit_r, item = 9)  #flagged: skewed to positive theta.
itemplot(fit_r, item = 10) #flagged: overshadowing extremes. 
itemplot(fit_r, item = 11) #flagged: skewed to negative theta.
itemplot(fit_r, item = 12) #flagged: skewed to negative theta + overshadowing extremes.

#item information: 
itemInfoPlot(fit_r, facet = T)
#absolute garbage according to II: 12,2,6,8
#worth considering according to II: 11,1,3,5,7,9

#test information
testInfoPlot(fit_r, adj_factor = 2). #Looks nice. Low SE and high information in a broad range. steep ends though and weird bimodal-ish distribution. What do we make of this?

#final item judgement criteria in one df.
itemjudge <- data.frame(
  item = row.names(ice_r),
  communality = round(smr_r$h2, digits = 2),
  p.S_x2 = round(itf_r$p.S_X2, digits = 2),
  discr=round(ice_r$items.a1, digits = 2),
  infit= round(infi_r$infit, digits = 2)
)
itemjudge
#this would eliminate items 5,7 and 8 from significant p.S_x2.NOT SURE IF THIS IS OK TO USE THOUGH...measure was developed for dichotomous IRT models. best discrimination is seen in items 1,5,7,9,11. Highest communality in items 7,9,11,1,5

# DIF
dif_gen_r <- multipleGroup(rdata[items], model = 1, 
                         group = rdata$gender, 
                         method = "MHRM",
                         invariance = c("cy_sesh_help1", "free_means", "free_variance"),
                         itemtype = "grsm")

#To do: Decide where we want to go in terms of theta sensitivity: Do we want fatter tails in the short measure? Then we can integrate an item that has flat IIF with two others that have clear peaks at midrange. If we want less sensitivity in the extremes, we can have three in midrange etc. Defining the desired target distribution of TIF is paramount here. 

DIF(dif_gen_r, 
    which.par = c("a1"), 
    seq_stat = "BIC",
    plotdif = T, 
    simplify = T,
    verbose = T)
