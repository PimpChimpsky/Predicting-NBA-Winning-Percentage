##### LIBRARIES #####
install.packages("mvtnorm")
library(bayesrules)
library(rstanarm)
library(bayesplot)
library(tidyverse)
library(tidybayes)
library(broom.mixed)
library(mvtnorm)
##### END OF LIBRARIES #####

##### DATA #####
file<-file.choose()
data<-read.csv(file)
attach(data)
##### END OF DATA #####

##### START OF PROJECT #####
# Create our variables
y <- WIN_PER  # our response variable
X <- as.matrix( cbind(PTS, DREB, OREB, BLK, STL, AST, TOV, PF, OFFRTG, 
                      DEFRTG, FG_PER, THREES_PER, FT_PER, PACE) )  # X matrix

# plot all variables to get an idea of general trends
name.list <- c("Points", "Defensive Rebounds", "Offensive Rebounds", "Blocks", "Steals", "Assists", "Turnovers", 
               "Personal Fouls", "Offensive Rating", "Defensive Rating", "Field Goal Percentage",
               "Three Point Percentage", "Free Throw Percentage", "Pace")

par(mfrow = c(2,2))
for(i in 1:14) {
  plot(y~X[,i],
       xlab = name.list[i],
       ylab = "Win Percentage")
}


# Check if there is any multicolinearity issues
cor_matrix <- cor(X)
heatmap(cor_matrix)

# some concerning correlations like pts and offrtg/fg%

### PRIORS ###
prior.mean.list <- c(2, 1, 1, 1, 1, 1, -1, -1, 2, 2, 2, 2, 1, 1)  # small coefficients 
prior.sd <- 15  # non-informative sd
### END OF PRIORS ###

### REGRESSION ###
nba_model <- stan_glm(y~PTS+DREB+OREB+BLK+STL+AST+TOV+PF+OFFRTG+DEFRTG+FG_PER+THREES_PER+FT_PER+PACE,
         data = data,
         family = gaussian,
         prior_intercept = normal(0, 15),
         prior = normal(prior.mean.list, prior.sd, autoscale=TRUE), 
         chains = 4, iter = 5000*2)

# MCMC diagnostics
mcmc_trace(nba_model)
mcmc_dens_overlay(nba_model)
mcmc_acf(nba_model)
neff_ratio(nba_model)
rhat(nba_model)

# Summarize betahats
tidy(nba_model, conf.int = TRUE, conf.level = 0.95)

### END OF REGRESSION ###
### Determining Variables ###
# Utilize ELPD to determine if OREB, BLK, PF, and FT_PER are worth keeping
prior.mean.list.2 <- c(2, 1, 1, 1, -1, 2, 2, 2, 2, 1)  # same coefficients
nba_model.2 <- stan_glm(y~PTS+DREB+STL+AST+TOV+OFFRTG+DEFRTG+FG_PER+THREES_PER+PACE, # got rid of mentioned variables
                      data = data,
                      family = gaussian,
                      prior_intercept = normal(0, 15),
                      prior = normal(prior.mean.list.2, prior.sd, autoscale=TRUE), 
                      chains = 4, iter = 5000*2)

tidy(nba_model.2, conf.int = TRUE, conf.level = 0.95)

prior.mean.list.3<- c(2, 1, 1, 1, 1, -1, 2, 2, 2, 2, 1)  # same coefficients
nba_model.3 <- stan_glm(y~PTS+DREB+BLK+STL+AST+TOV+OFFRTG+DEFRTG+FG_PER+THREES_PER+PACE, # added back BLK's
                        data = data,
                        family = gaussian,
                        prior_intercept = normal(0, 15),
                        prior = normal(prior.mean.list.3, prior.sd, autoscale=TRUE), 
                        chains = 4, iter = 5000*2)

tidy(nba_model.3, conf.int = TRUE, conf.level = 0.95)

model_elpd_main <- loo(nba_model)
model_elpd_main$estimates
model_elpd_2 <- loo(nba_model.2)
model_elpd_2$estimates
model_elpd_3 <- loo(nba_model.3)
model_elpd_3$estimates
### End of Determining Variables ###

### Addressing Correlation ###
X.updated <- as.matrix( cbind(PTS, DREB, BLK, STL, AST, TOV, OFFRTG, DEFRTG, FG_PER, THREES_PER, PACE) )
cor_matrix.updated <- cor(X.updated)
heatmap(cor_matrix.updated)

#Model 1 - no points
prior.mean.list.no_pts <- c(1, 1, 1, 1, -1, 2, 2, 2, 2, 1)  # same coefficients
nba_model.no_pts <- stan_glm(y~DREB+BLK+STL+AST+TOV+OFFRTG+DEFRTG+FG_PER+THREES_PER+PACE, # no points
                        data = data,
                        family = gaussian,
                        prior_intercept = normal(0, 15),
                        prior = normal(prior.mean.list.no_pts, prior.sd, autoscale=TRUE), 
                        chains = 4, iter = 5000*2)
#Model 2 - no fg%
prior.mean.list.no_fg <- c(2, 1, 1, 1, 1, -1, 2, 2, 2, 1)  # same coefficients
nba_model.no_fg <- stan_glm(y~PTS+DREB+BLK+STL+AST+TOV+OFFRTG+DEFRTG+THREES_PER+PACE, # no fg%
                                data = data,
                                family = gaussian,
                                prior_intercept = normal(0, 15),
                                prior = normal(prior.mean.list.no_fg, prior.sd, autoscale=TRUE), 
                                chains = 4, iter = 5000*2)
#Model 3 - no points and fg%
prior.mean.list.no_fg_pts <- c(1, 1, 1, 1, -1, 2, 2, 2, 1)  # same coefficients
nba_model.no_fg_pts <- stan_glm(y~DREB+BLK+STL+AST+TOV+OFFRTG+DEFRTG+THREES_PER+PACE, # no pts and fg%
                        data = data,
                        family = gaussian,
                        prior_intercept = normal(0, 15),
                        prior = normal(prior.mean.list.no_fg_pts, prior.sd, autoscale=TRUE), 
                        chains = 4, iter = 5000*2)
#Model 4 - no offensive rating
prior.mean.list.no_offrtg <- c(2, 1, 1, 1, 1, -1, 2, 2, 2, 1)  # same coefficients
nba_model.no_offrtg <- stan_glm(y~PTS+DREB+BLK+STL+AST+TOV+DEFRTG+FG_PER+THREES_PER+PACE, # added back BLK's
                        data = data,
                        family = gaussian,
                        prior_intercept = normal(0, 15),
                        prior = normal(prior.mean.list.no_offrtg, prior.sd, autoscale=TRUE), 
                        chains = 4, iter = 5000*2)
#Model 5 - no 3p%
prior.mean.list.no_three <- c(2, 1, 1, 1, 1, -1, 2, 2, 2, 1)  # same coefficients
nba_model.no_three <- stan_glm(y~PTS+DREB+BLK+STL+AST+TOV+OFFRTG+DEFRTG+FG_PER+PACE, # added back BLK's
                        data = data,
                        family = gaussian,
                        prior_intercept = normal(0, 15),
                        prior = normal(prior.mean.list.no_three, prior.sd, autoscale=TRUE), 
                        chains = 4, iter = 5000*2)

# Check ELPD Values of these models compared to our new model
model_elpd_3 <- loo(nba_model.3); model_elpd_3$estimates # our current model
model_elpd_no_pts <- loo(nba_model.no_pts); model_elpd_no_pts$estimates # no points
model_elpd_no_fg <- loo(nba_model.no_fg); model_elpd_no_fg$estimates # no fg%
model_elpd_no_fg_pts <- loo(nba_model.no_fg_pts); model_elpd_no_fg_pts$estimates # no points and fg%
model_elpd_no_offrtg <- loo(nba_model.no_offrtg); model_elpd_no_offrtg$estimates # no offrtg
model_elpd_no_three <- loo(nba_model.no_three); model_elpd_no_three$estimates # no 3p%

# Model 1 is the best model here
# let's look at the coeeficients
tidy(nba_model.no_pts, conf.int = TRUE, conf.level = 0.95)
tidy(nba_model.no_pts, conf.int = TRUE, conf.level = 0.8)

### End of Addressing Correlation ###

### Gibbs Sampler to Test All Models ###
lpy.X <- function(y,X,g=length(y),nu0=1,s20=try(summary(lm(y~-1+X))$sigma^2,silent=TRUE)) {
  n<-dim(X)[1]; p<-dim(X)[2]
  if(p==0) {Hg<-0; s20<-mean(y^2)}
  if(p>0) {Hg<-(g/(g+1))*X%*%solve(t(X)%*%X)%*%t(X)}
  SSRg<- t(y)%*%( diag(1,nrow=n) - Hg)%*%y
  
  -.5*(n*log(pi)+p*log(1+g)+(nu0+n)*log(nu0*s20+SSRg)-nu0*log(nu0*s20))+lgamma((nu0+n)/2)-lgamma(nu0/2)
}

X.gibbs <- as.matrix( cbind(rep(1, length(DREB)), PTS, DREB, OREB, BLK, STL, AST, TOV, PF, OFFRTG, 
                            DEFRTG, FG_PER, THREES_PER, FT_PER, PACE) )

### Starting values for Gibbs Sampler:
z<-rep(1,dim(X.gibbs)[2])  # starting with z = all 1's (all terms in model)
lpy.c<-lpy.X(y,X.gibbs[,z==1,drop=FALSE])
S <- 10000  # number of Monte Carlo iterations
Z<-matrix(NA,S,dim(X.gibbs)[2])

## The Gibbs Sampler:
for(s in 1:S)
{
  for(j in sample(1:dim(X.gibbs)[2]))
  {
    zp<-z; zp[j] <- 1-zp[j]
    lpy.p<-lpy.X(y,X.gibbs[,zp==1,drop=FALSE])
    r<- (lpy.p - lpy.c)*(-1)^(zp[j]==0)
    z[j]<-rbinom(1,1,1/(1+exp(-r)))
    if(z[j]==zp[j]) {lpy.c<-lpy.p}
  }
  Z[s,]<-z
}
## ###

# Considering all possible subsets:
poss.z.vectors <-  unique(Z,MARGIN=1)
z.probs <- rep(0, times= nrow(poss.z.vectors))

for(i in 1:nrow(poss.z.vectors)) {
  z.probs[i] <- sum(apply(Z,1,identical, y=poss.z.vectors[i,]))
}
z.probs <- z.probs/sum(z.probs)

cbind(poss.z.vectors, z.probs)[rev(order(z.probs)),]
### End of Gibbs Sampler to Test All Models ###

### Coefficients of our remaining significant variables ###
nba_model.OFFRTG_DEFRTG_FG <- stan_glm(y~OFFRTG+DEFRTG+FG_PER, # added back BLK's
                        data = data,
                        family = gaussian,
                        prior_intercept = normal(0, 15),
                        prior = normal(2, prior.sd, autoscale=TRUE), 
                        chains = 4, iter = 5000*2)
nba_model.DREB_BLK_OFFRTG_DEFRTG_FG_THREE <- stan_glm(y~DREB+BLK+OFFRTG+DEFRTG+FG_PER+THREES_PER, # added back BLK's
                                       data = data,
                                       family = gaussian,
                                       prior_intercept = normal(0, 15),
                                       prior = normal(2, prior.sd, autoscale=TRUE), 
                                       chains = 4, iter = 5000*2)


tidy(nba_model.OFFRTG_DEFRTG_FG, conf.int = TRUE, conf.level = 0.8)
tidy(nba_model.DREB_BLK_OFFRTG_DEFRTG_FG_THREE, conf.int = TRUE, conf.level = 0.8)

#MCMC Checks of 6 variables
mcmc_trace(nba_model.DREB_BLK_OFFRTG_DEFRTG_FG_THREE)
mcmc_dens_overlay(nba_model.DREB_BLK_OFFRTG_DEFRTG_FG_THREE)
mcmc_acf(nba_model.DREB_BLK_OFFRTG_DEFRTG_FG_THREE)
### End of Coefficients of our remaining significant variables ###





### Curiosity Finds ###
# Out of Pure Curiosity - what if we only have offrtg and defrtg
nba_model.off_def <- stan_glm(y~OFFRTG+DEFRTG, # only offensive rating and defensive rating
                              data = data,
                              family = gaussian,
                              prior_intercept = normal(0, 15),
                              prior = normal(2, prior.sd, autoscale=TRUE), 
                              chains = 4, iter = 5000*2)

tidy(nba_model.off_def, conf.int = TRUE, conf.level = 0.95)

model_elpd_off_def <- loo(nba_model.off_def)
model_elpd_off_def$estimates

# Out of Pure Curiosity - what if we use beta regression
nba_model.betareg <- stan_betareg(y~OFFRTG+DEFRTG,
                                  data = data,
                                  prior_intercept = normal(0, 15),
                                  prior = normal(2, prior.sd, autoscale=TRUE), 
                                  chains = 4, iter = 5000*2)

tidy(nba_model.betareg, conf.int = TRUE, conf.level = 0.95)

# Out of Pure Curiosity - let's test models with significant variables
nba_model.sig95 <- stan_glm(y~OFFRTG+DEFRTG+FG_PER,
                              data = data,
                              family = gaussian,
                              prior_intercept = normal(0, 15),
                              prior = normal(2, prior.sd, autoscale=TRUE), 
                              chains = 4, iter = 5000*2)

tidy(nba_model.sig95, conf.int = TRUE, conf.level = 0.95)

model_elpd_off_sig95 <- loo(nba_model.sig95)
model_elpd_off_sig95$estimates

nba_model.sig80 <- stan_glm(y~DREB+BLK+OFFRTG+DEFRTG+FG_PER+THREES_PER,
                            data = data,
                            family = gaussian,
                            prior_intercept = normal(0, 15),
                            prior = normal(c(1,1,2,2,2,1), prior.sd, autoscale=TRUE), 
                            chains = 4, iter = 5000*2)

tidy(nba_model.sig80, conf.int = TRUE, conf.level = 0.95)

model_elpd_off_sig80 <- loo(nba_model.sig80)
model_elpd_off_sig80$estimates

summary(lm(y~OFFRTG+DEFRTG+FG_PER))
summary(lm(y~DREB+BLK+OFFRTG+DEFRTG+FG_PER+THREES_PER))


# Out of Pure Curiosity - regression of simple statistics
lm(y~PTS+DREB+OREB+AST+TOV+BLK+STL)

### End of Curiosity Finds ###
##### END OF PROJECT #####
