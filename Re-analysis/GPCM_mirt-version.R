##Reanalysis of Borgert et al. (2023)
## This script will re-analyse validation data from Borgert et al. to test suitable items and IRT models with a state-of-the-art approach.

#install the ggmirt package from github if needed: 
#devtools::install_github("pkmasur/ggmirt")

#Required packages
pkg <- c("mirt", "tidyverse","ggmirt", "patchwork")
for (i in pkg){
  library(i, character.only = T)
}
set.seed(1337)
#pre-processing
#load processed data directly from the github repository of Borgert et al. (2023)
rdata <- read_csv("Re-analysis/data_processed.csv")
if(interactive()) mirtCluster()
#specify items
items <- c(8:19)
dem <- c("education", "employment", "gender", "age")


#further look at response variable properties. for this, we create boxplots for every item and full cysesh scores. 
boxplots <- rdata[items] |> pivot_longer(cols = 1:12, names_to = "Item", values_to = "Value") |> ggplot(aes(x=Item, y=Value))+geom_boxplot(notch = T)+stat_summary(fun=mean, geom = "point", shape=20,size=4,color="red")+theme(legend.position = "none")

hist_full_score <- rdata|> ggplot(aes(x=cy_sesh_score))+geom_histogram()

box_full_score <- ggplot(rdata, aes(y=cy_sesh_score, x=""))+geom_boxplot(notch = T, outlier.color = "red", outlier.shape = 1)+stat_summary(fun=mean, geom = "point", shape=20,size=4,color="red")+theme(legend.position = "none")

#looking at CYSESH scores by different demographic variables. cut out primary school, too few data for a meaningful boxplot.
rdata[-which(rdata$education=="Primary School"),c("cy_sesh_score", "education")] |> ggplot(aes(y=cy_sesh_score, x="", fill = education))+geom_boxplot(notch = T, outlier.colour = "red")

#age and score. this regression line is fake. should be much closer to parallel, as we saw in our total demographics regression...its missing other factors. 
rdata[which(rdata$age>17),] |> ggplot(aes(x=age, y=cy_sesh_score))+geom_point()+geom_smooth(method = "lm", fill=NA)

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

#construct table with high or low theta scores and see what that constitutes
unfit_top <- rdata[which(fscores(fit_r)>2),]
unfit_bot <- rdata[which(fscores(fit_r)< -2),]

#now lets follow up on the person fit stats: plot and show descriptives of that data. 
unfit_top |> ggplot(aes(y=cy_sesh_score, x="", fill = education))+geom_boxplot(notch = T, outlier.colour = "red")

unfit_bot |> ggplot(aes(y=cy_sesh_score, x="", fill = education))+geom_boxplot(notch = T, outlier.colour = "red") 

#look at item parameters for these subsamples

psych::describe(unfit_bot[items])
psych::describe(unfit_top[items])
unfit_bot$age |> hist()
unfit_top$age |> hist()

unfit_bot |> ggplot(aes(x=education))+geom_bar()
unfit_top |> ggplot(aes(x=education))+geom_bar()
#i see what this is. Overestimation of abilities and underestimation respectively kicking in.


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
xdata <- rdata |> select(c(gender, colnames(rdata[items]))) |> filter(gender=="Male"|gender=="Female")
dif_gen_r2 <- multipleGroup(xdata[2:13], model = 1,
                            group = xdata$gender,
                            method = "MHRM",
                            invariance = c("cy_sesh_policies1", "cy_sesh_connecting1", "free_means", "free_variance"),
                            itemtype = "grsm")

#To do: Decide where we want to go in terms of theta sensitivity: Do we want fatter tails in the short measure? Then we can integrate an item that has flat IIF with two others that have clear peaks at midrange. If we want less sensitivity in the extremes, we can have three in midrange etc. Defining the desired target distribution of TIF is paramount here. 

dif_grsm <- DIF(dif_gen_r2, 
    which.par = c("a1", "c"),
    scheme = "add",
    plotdif = T, 
    simplify = T,
    verbose = T)

#sensitivity analysis in terms of outliers. train the same model for data without the outliers and see where that goes (removing fat tails)
r2data <- anti_join(rdata,unfit_dat, by="id")
fit_r2 <- mirt(r2data[items], model=1, itemtype = "grsm", method="MHRM", verbose = T)
summary(fit_r2)
modfit_r2 <- M2(fit_r2, type = "C2", calcNull = F) #a tad better. 

#lets try some new models, shall we. use the "graded" and "rsm" models in a more rasch-y fashion.

fit_grad <- mirt(rdata[items], model=1, itemtype = "graded", method="MHRM", verbose = T)
fit_rsm <- mirt(rdata[items], model=1, itemtype = "rsm", method="MHRM", verbose = T)
modfit_grad <- M2(fit_grad, type = "C2", calcNull = F) #worse in RMSEA, better in SRMSR, though.
modfit_rsm <- M2(fit_rsm, type = "C2", calcNull = F)  #terrible modfit in all aspects, discard.

apatheme <- theme(panel.grid.major = element_blank(), 
      panel.background = element_blank(), 
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black"))

