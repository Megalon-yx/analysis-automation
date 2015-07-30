

# Copyright statement comment
# Author comment
# File description comment, including purpose of program, inputs, and outputs
# source() and library() statements

```{r, echo=FALSE, fig.width=10, fig.height=10}
ipak <- function(pkg){
  # Github supplied this and it's great https://gist.github.com/stevenworthington/3178163#file-ipak-r
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("car", "stats", "FactoMineR", "ggbiplot", "caret", "e1071", "betareg", "knitr", "MASS")
ipak(packages)
# Function definitions
# Executed statements, if applicable (e.g., print, plot)

```{r, echo=FALSE, fig.width=10, fig.height=10}
#log 5:27 PM 3/23/2015
# Multidimensional analysis of Weapons Data 
cat("In this report we will analyze a dataset consisting of killcounts for each type of weapon in Destiny, 
for players with a trueskill rating of 50 in the Control playlist. ")
```{r, eval=FALSE}
###############################################################################################################
#                                                   Workspace I                                               #
###############################################################################################################
```{r, echo=FALSE, fig.width=10, fig.height=10}
#Load in data from file, collected on 3/3/2015, saved as a comma seperated value sheet)

  DestinyData <- read.csv("C:/Users/Ehren`/Downloads/DestinyData.csv")
cat("DestinyData <- read.csv('C:/Users/Ehren`/Downloads/DestinyData.csv')")

cat("Consists of by-weapon kill counts for all players rated TrueSkill50 on XboxLive, in the Crucible gametype
'Control', their K/D, and and their WinPercentage. Because the dataset is small outliers may be identified
but not removed. In order to perform a Principal Component Analysis (PCA) on the data, the data will be 
normalized so variables with high means but low variances do not unduly influence the components. Any 
regressions performed will not be weighted, though heteroskedasticity may be identified and commented on.")
cat("Initial Observations:  
  1.Only weapons missing from any players are ScoutRifle (1 zero) and PulseRifle (3 zeros).  
      Lack of large, open spaces may be a contributing factor.  
  2.85% of top players are hunters, with 2 Warlocks and 1 Titan.  
  3.HandCannons seem to be the most popular gun. Comparison to lower-ranked players will tell if  
      HandCannon use is a factor in success.   
  4.Homogeneity in data may cause generally high and low valued weapons to produce an incorrect   
      general scaling factor.
  5.This is only a very small slice of data from one end of the spectrum. With a larger, random sampling of
      observations over more variables, much more accurate, informative, and useful conclusions can be made.") 

  numguns <- 12 #number of guns/predictors
  numcols <- 16 #number of columns in the original dataset
  numvars <- 14 #number of predictors + responses
  numobs <- 20 #number of observations in the dataset.
```{r, eval=FALSE}
###############################################################################################################
#                                                   DATA PREP II                                              #
###############################################################################################################
```{r, echo=FALSE, fig.width=10, fig.height=10}
#make the coding a little more manageable, while also dropping each player's gamertag from the set
  kcounts <- DestinyData[2:numcols]
#Cursory look at relationships between the counts
  scatterplotMatrix(kcounts[1:numguns])
cat("Since this data is very complicated, the first thing we should do is look at a scatterplot of the data.
  This plot seems too busy to be of any use, but if you look closely you can see negatively correlated 
  relationships between weapons that occupy the same slot. We will be able to see this better later. We also
  see a lot of skewness in the density plots. This isn't necessarily a problem, but could prevent any
  conclusions drawn from this dataset from being applicable to the general population if these distributions
  don't also hold there as well.")
#Change the kill COUNTS into kill PROPORTIONS. 
cat("We can construct a second dataset from the first one, changing the kill COUNTS into kill PROPORTIONS.
  Doing this will help account for the fact that players have played different numbers of games, while 
  still reflecting their playstyle by counting how they effectively spend their time, instead of total 
  experience. Another way of doing this would be to instead use the mean or median Kills/Game stat for
  each weapon.")
  kprop <- kcounts
  kprop[,(numvars+1)]=0 #using "Class" as storage for total kills, since it will be immediately dropped.
  for(a in 1:numobs){   
    for(b in 1:numguns){
      kprop[a,b] <- kcounts[a,b]/sum(kcounts[a,1:numguns]) #dividing each entry by total number of kills
    }
  }
  kprop<-kprop[1:numvars] #dropping "Class"/total kills column.
#Now we will normalize each data frame, to be able to apply PCA.
  knorm <- kcounts #create knorm
  knorm[1:numguns] <- scale(kcounts[1:numguns]) #set knorm as a normalized copy of kcounts
  kpnorm <- kprop #create kpnorm
  kpnorm[1:numguns] <- scale(kprop[1:numguns]) #set kpnorm as a normalized copy of kprop
  scatterplotMatrix(kpnorm[1:numguns])  
# Spoiler: hunters, handcannons, shotguns.
cat("To Summarize the datasets:
    kcounts: normalized kill count data, with 'K.D'' (K/D) and 'Win.' (W%) in columns 13 and 14. '
    knorm: normalized version of kcounts
    kprop: proportion of kills for each method, by player. KD/WL in 13/14. 
    kpnorm: normalized version of kprop, by method. KD/WL in 13/14.

While I don't have it available, Kills/Weapon/Game would be definitely be useful to have since it would
take into account a player's general effectiveness AS WELL as their playstyle. Means could be calculated
from available data, medians may prove more useful, as they are less susceptible to outliers. Using medians
would also provide more 0 counts in the data, which could give more a meaningful analysis, but could also
prove to be a problem when estimating parameters. Since we don't have median data, we will not need to worry
about this.")
```{r, eval=FALSE}
###############################################################################################################
#                                                   EXPLORAITION III                                          #
###############################################################################################################
```{r, echo=FALSE, fig.width=6, fig.height=6}
cat("To explore kcount data we will use Principal Component Analysis, a technique that uses covariance and/or 
correlation matrices to construct a a linear combination of the original variables that explains as much of 
the variance of the original data as possible. These components represent a new set of observed variables
through which we can more efficiently model our response variable. In addition to that, they often reveal
underlying patterns in the data which were previously hidden.")
#Principal Component Analysis of knorm (zscores of kcounts)
  knorm.pca <- princomp(knorm[1:numguns], center=TRUE, scale.=TRUE)
  plot(knorm.pca, type="lines")
cat("This is a scree plot of the PCA, with the ordinal of the component on the x-axis and the variance of
the component on the y-axis. The efficiency of PCA comes in being able to select fewer variables that
each explain more variance than variables in the original set. There are several ways to do this, but
selecting the first three components meets both the Kaiser Criterion and the 'elbow method', so that
is what we will do. We can list the components in descending order here, and look at how much of the
original variance they each account for. Note that each variable in the original set had a variance
of 1 because we normalized them first. Consequently, anything with a standard deviation greater than
1 is an improvement in efficiency on the original data. We can see that the first 3 components we 
selected to use all have a variance greater than 1.")
  summary(knorm.pca)
cat("By using the first and second components as our axes, we can see where each of the original variables
and observations falls in relation to them. Dim.1, the first component, seems to indicate overall 
experience. Dim.2 is much more interesting, on one side of the axis are what can be considered risky,
short to mid range runs with higher time-to-kill (TTK), and on the other side are safe, long-range
runs with a small TTK. ")
  knorm.pca2 <- ggbiplot(knorm.pca, choices=c(1,2), obs.scale = 1, var.scale = 1, groups = knorm$Class, 
                         ellipse = TRUE, circle = TRUE) 
  knorm.pca2 <- knorm.pca2 + scale_color_discrete(name = 'Class') + theme(legend.direction = 'horizontal', 
                                                                          legend.position = 'top')
  print(knorm.pca2)
cat("When we plot Dim.3, the third highest component, against the second component, we see that weapons 
which occupy the same weapon slot appear almost diametrically oppososite each other. We also see some
other groupings here: MachineGun/AutoRifle, which require target tracking; Super(GoldenGun most likely)
/Sniper, which has the lowest TTK; Pulse/ScoutRifle, long range weapons; Grenade/Shotgun/Melee, which are
all incredibly short range weapons; and FusionRifle/RocketLauncher, which both require good timing. The
HandCannon is alone, opposite the long range rifles, this may be because they occupy the same slot and
share similar firing patterns, but at different ranges. Also noticeable is the fact that the vectors for
HandCannons and Shotguns, the two most popular weapons, are almost completely perpendicular.")
  knorm.pca3 <- ggbiplot(knorm.pca, choices=c(3,2), obs.scale = 1, var.scale = 1, groups = knorm$Class, 
                         ellipse = TRUE, circle = TRUE) 
  knorm.pca3 <- knorm.pca3 + scale_color_discrete(name = 'Class') + theme(legend.direction = 'horizontal', 
                                                                          legend.position = 'top')
  print(knorm.pca3)
#We will use PCA() to create a new dataset using the new dimensions as variables for the players.
  knorm4fit <- PCA(knorm[1:numguns], graph=FALSE)
cat("We can look at the relationships between the components in another set of scatterplots, replacing the
original variables with the scores of each of the components for each observation. Notice that the green
fit lines have zero slope, confirming the fact that the components have no correlation with each other. Also
consider the fact that we are now dealing with one-quarter the data, while still representing 75% of the
total variation in the dataset, also we have also learned more about how the data is structured.")
#the fit lines for the components are horizontal, and the smooths don't look too terrible.
  knormdata <- kcounts[,1:5]
  knormdata[,1:5] <- knorm4fit$ind$coord
  knormdata[,4] <- knorm[,(numguns+1)]#replacing them with K.D and Win. in order to regress against them later
  knormdata[,5] <- knorm[,(numguns+2)]
  knormdata <-rename(knormdata, c("AutoRifle"="Dim.1", "FusionRifle"="Dim.2", "Grenade"="Dim.3", 
                                  "HandCannon"="K.D", "Machinegun"="Win."))
  scatterplotMatrix(knormdata[ ,1:3])
```{r, eval=FALSE}
###############################################################################################################
#                                                   ANALYSIS IIII                                             #
###############################################################################################################
```{r, echo=FALSE, fig.width=4.5, fig.height=4.5}
cat("Now that we've taken some time to explore the data we can try fitting some models. The two statistics we 
have that we would like to predict are Kill/Death ratio (KD.) and Win rate (Win.). We can use the
standard linear models, but will also use a beta regression, which is a specialized regression model used
to model responses on the unit interval, like a winrate. This type of model is particularly useful here
because it won't give us negative winrates, or values higher than 1, but of which would be possible with
a general linear model.")
##################################################
  WLpred.knorm <- betareg(Win. ~ AutoRifle+FusionRifle+Grenade+HandCannon+Machinegun+Melee+
                          PulseRifle+RocketLauncher+ScoutRifle+Shotgun+Sniper+Super, data=data.frame(knorm), 
                        link="logit",link.phi=NULL)
cat("We can see from the results below that the model does indeed have some good predictive power, with an
R-squared of '.65'. However, most of the coefficients are not statistically significant, so while it can
be considered a useful model, it's not necessarily a 'good' model.")
  summary(WLpred.knorm)
##################################################
  WLpred.knormpca <- lm(Win. ~ Dim.1 + Dim.2 + Dim.3, data=as.data.frame(knormdata))
cat("The results below are why a beta regression is useful in place of a linear one. The adjusted R-squared is 
actually negative. (As an aside, I've never seen a model fit this poorly before). We won't even bother 
looking at model diagnostics for this.")
  summary(WLpred.knormpca)
###################################################
  KDpred.knormpca <- lm(K.D ~ Dim.1 + Dim.2 + Dim.3, data=as.data.frame(knormdata))
cat("When modeling Kill/Death ratio it makes sense to use a linear model, since values aren't confined to the
unit interval. However the fact that there are no negative values could prove problematic. If this model
doesn't fit as well as we would like we can try fitting another linear model that accounts for this fact.")
  summary(KDpred.knormpca)
cat("The model diagnostics below look good. The residuals don't show any trending; appear to be normally
distributed, independent, and homoskedastic; and none of the points have overly large Cook's Distances.
The strong fit of the model doesn't necessarily make up for the lack of predictive power, though.")
  plot(KDpred.knormpca, caption =list("Residuals vs Fitted", "Normal Q-Q", "Scale-Location", 
                                    "Cook's distance", "Residuals vs Leverage"))
###################################################
cat("Since our linear model didn't have as much predictive power as we would have liked, we can transform
the response variable to try and find a better fit. Using a Box-Cox plot we can see which, if any, 
transform is likely to yield desirable results.")
  boxcox(KDpred.knormpca,plotit=T, lambda=seq(-3, 3, by=.5))
cat("The Box-Cox plot suggests that there is not necessarily a transformation needed. But as the confidence
interval is centered around 0, it might be beneficial to try a log transform.")
  bcKDpred.knormpca <- lm(log(K.D) ~ Dim.1 + Dim.2 + Dim.3, data=as.data.frame(knormdata))
  summary(bcKDpred.knormpca)
cat("Unfortunately this doesn't provide a better fit than the previous model, and without displaying the fit,
the inverse transformation doesn't work either. It it is likely at this point that more data is needed to
produce a better fit.")
###################################################
cat("White it may seem disappointing that these models have such low predictive power, that's not necessarily
a bad thing. The fact that a player's winrate and K/D are largely independent from which weapons they are
choosing provides no evidence that PvP is unbalanced. But by fitting more powerful and accurate models, small
imbalances in gameplay can be identified and corrected for. And just to show that better players DO in 
fact win more often, here is winrate modeled on a player's K/D:")
  WLKDpred.knormpca <- lm(Win. ~ K.D, data=as.data.frame(knormdata))
  summary(WLKDpred.knormpca)
```{r, eval=FALSE}
###############################################################################################################
#                                                   EXPLORATION V                                             #
###############################################################################################################
```{r, echo=FALSE, fig.width=6, fig.height=6}
cat("We will perform another PCA on the set of kill proportions, kpnorm. While the previous dataset provided
information on a player's total experience with a gun, their total number of kills, this data provides 
information about how players are choosing to spend their time. Given the structure of the first component
in the previous PCA, it seems likely that all the information contained by it is absent in this dataset.")
#Principal Component Analysis of kpnorm (zscores of kprop)
  kpnorm.pca <- princomp(kpnorm[1:numguns], center=TRUE, scale.=TRUE)
  summary(kpnorm.pca)
  kpnorm.pca2 <- ggbiplot(kpnorm.pca, choices=c(1,2), obs.scale = 1, var.scale = 1,  
                          ellipse = TRUE, circle = TRUE) 
  kpnorm.pca2 <- kpnorm.pca2 + theme(legend.direction = 'horizontal', 
                                                                            legend.position = 'top')
  print(kpnorm.pca2)
cat("The first component in this new set is almost exactly the same as the second component in the previous
dataset. What we can infer from this is that the information contained in the previous first component
was lost as a player's aggregate experience was removed from the dataset, meaning that at least a large
portion of that old first component's information was a player's overall experience. Looking at the third
component, the most popular weapons are all on one side, while less popular ones are on the opposite side.")
  kpnorm.pca3 <- ggbiplot(kpnorm.pca, choices=c(1,3), obs.scale = 1, var.scale = 1, groups = kpnorm$Class, 
                          ellipse = TRUE, circle = TRUE) 
  kpnorm.pca3 <- kpnorm.pca3 + theme(legend.direction = 'horizontal', 
                                                                            legend.position = 'top')
  print(kpnorm.pca3)
cat("Selecting the number of components to use for this set is a little more difficult. The scree plot below
doesn't show a definitive elbow. Using the Kaiser criterion though we can choose to use the first five
components.")
#Looking at the kpnorm.pca scree plot, we will take the first 5 components, according to the the Kaiser 
#Criterion
#We will use PCA() to create a new dataset using the new dimensions as variables for the players.
  kpnorm4fit <- PCA(kpnorm[1:numguns], graph=FALSE)
  kpnormdata <- kcounts[,1:7]
  kpnormdata[,1:5] <- kpnorm4fit$ind$coord
  kpnormdata <- kpnormdata[,1:7]#we will be dropping components 4 and 5 since we don't need them, and
  kpnormdata[,6] <- kpnorm[,(numguns+1)]#replacing them with K.D and Win. in order to regress against them later
  kpnormdata[,7] <- kpnorm[,(numguns+2)]
  kpnormdata <-rename(kpnormdata, c("AutoRifle"="Dim.1", "FusionRifle"="Dim.2", "Grenade"="Dim.3", 
                                  "HandCannon"="Dim.4", "Machinegun"="Dim.5", "Melee"="K.D", 
                                  "PulseRifle"="Win."))
```{r, eval=FALSE}
###############################################################################################################
#                                                   ANALYSIS VI                                               #
###############################################################################################################
```{r, echo=FALSE, fig.width=4.5, fig.height=4.5}
###################################################
cat("A good blueprint for analysis of kpnorm is to follow the path we took with knorm. To start we can fit a
beta regression for 'Win.' to the data and see how well the model fits. The betareg model for 'Win.' 
actually fits better for kpnorm than it did for knorm, and with less significant predictors, too. It is 
likely that running this model with more data will produce a stronger model, or at least more certainty as 
to the value of the coefficients.")
  WLpred.kpnorm <- betareg(Win. ~ AutoRifle+FusionRifle+Grenade+HandCannon+Machinegun+Melee+
                          PulseRifle+RocketLauncher+ScoutRifle+Shotgun+Sniper, data=data.frame(kpnorm), 
                        link="logit",link.phi="log")
  summary(WLpred.kpnorm)
###################################################
cat("This model does an alright job of modeling the winrate, but not as good as the first. Still, the 
addition of more data could make the model considerable stronger.")
WLpred.kpnormpca <- betareg(Win. ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5, 
                            data=as.data.frame(kpnormdata), link="logit",link.phi="log")
summary(WLpred.kpnormpca)
###################################################
cat("From the printout below, this is actually one of the strongest models produced so far. It's a strong
predictor of 'K.D' and the F-statistic is the best so far. When we look at the coefficients, the higher number
of kills you have with grenades, proportionally, the lower your 'K.D'. Can't aim? Just stick someone with
your grenade.")
  KDpred.kprop <- lm(K.D ~ AutoRifle+FusionRifle+Grenade+HandCannon+Machinegun+Melee
                   +PulseRifle+RocketLauncher+ScoutRifle+Shotgun+Sniper+Super, data=data.frame(kprop))
  summary(KDpred.kprop)
###################################################
cat("This model is actually very interesting. It shows a very strong relationship between 'Dim.2', the second
component of the PCA for kpnorm, and 'K.D'. Interpretting this means that as players get more kills with 
HandCannons, Super, and Rockets, their average K/D decreases. Judging from the fact that it increases with
kills form all other weapons, it could represent some sort of general versatility metric of the players, how
willing they are to experiment and try new things, and adapt to situations. Again, this is only one possible
interpretation of the model.")
  KDpred.kpnormpca <- lm(K.D ~ Dim.1 + Dim.2 + Dim.3 + Dim.4 + Dim.5, data=as.data.frame(kpnormdata))
  summary(KDpred.kpnormpca)
###################################################
cat("Looking at a Box-Cox plot of the linear model, it seems that a transform using either log or inverse could
yield better results for the model. However, I ran these models, and I'm not going to show them here because
the adjusted R-squared didn't even change by 1% of the original value of .42 . ")
  boxcox(KDpred.kpnormpca)
###################################################
```{r, eval=FALSE}
###############################################################################################################
#                                                   CONCLUSION VII                                            #
###############################################################################################################
```{r, echo=FALSE, fig.width=4.5, fig.height=4.5}
cat("In conclusion, Destiny player data contains an untapped wealth of information. There are many techniques 
which were not able to be used here, due to the nature and quantity of the original data. Many models' fits 
may be limited by the small slice of the player spectrum for which they were fit, as well as the available
data for each player selected. Headshots, subclass loadouts, RoF-Impact catagories for each gun, as well as
which armor shaders and exotics players use could also have effects on the models. Cox Proportional Hazard
models can be used to model when First Blood will occur, poission distributions can predict how long
players will live. Even the prediction of a player's K.D can be improved by predicting kills and deaths
separately and calculating the ratio afterwards. Other factors come into play as well, like the number
of meaningful interactions per game (kills+deaths), as well as assists and the distribution of a player's
lifespans. 
    
Making accurate and useful models of all these statistics will enable us to make more informed choices in
both game design and PvP weapon balance, as well as possibly producing a better and more accurate rating 
than TrueSkill for matchmaking. Constructing models on how players of different skills interact and how 
quickly they get and give up kills could mean closer more exciting matches for players of all skill levels.")