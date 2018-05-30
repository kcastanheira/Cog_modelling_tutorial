#Set the current working directory to where you have the data file
setwd("~/Desktop/Modelling Tutorial/")

# Load the ggplot2 Library
# IF YOU DO NOT HAVE IT INSTALLED UNCOMMENT THE NEXT LINE
#install.packages("ggplot2")
library(ggplot2)

####### UTILITY FUNCTION #######
Dollars = seq(0, 3, 0.1)
U_0 = Dollars
U_1 = Dollars^0.85
U_2 = Dollars^0.65
U_3 = Dollars^0.25

Utility = data.frame(Utility = c(U_0, U_1, U_2, U_3),
                    Dollars = c(Dollars, Dollars, Dollars, Dollars),
                    Alpha= c(rep(1, length(Dollars)),
                            rep(0.85, length(Dollars)),
                            rep(0.65, length(Dollars)),
                            rep(0.25, length(Dollars))))
Utility$Alpha = as.factor(Utility$Alpha)

ggplot(Utility, aes(Dollars, Utility, group=Alpha, color=Alpha))+
geom_line(size=1)+
theme_minimal()

###### PROBABILITY WEIGHTING FUNCTION ######
probability = seq(0, 1, 0.001)
p_0 = probability
p_1 = (probability^0.85)/(((probability^0.85)+(1-probability)^0.85)^(1/0.85))
p_2 = (probability^0.65)/(((probability^0.65)+(1-probability)^0.65)^(1/0.65))
p_3 = (probability^0.45)/(((probability^0.45)+(1-probability)^0.25)^(1/0.45))


Probability = data.frame(Subjective_Probability = c(p_0, p_1, p_2, p_3),
                    Probability = c(probability, probability, probability, probability),
                    Beta= c(rep(1, length(probability)),
                            rep(0.85, length(probability)),
                            rep(0.65, length(probability)),
                            rep(0.45, length(probability))))
Probability$Beta = as.factor(Probability$Beta)

ggplot(Probability, aes(Probability, Subjective_Probability, group=Beta, color=Beta))+
geom_line(size=1)+
  xlab("p(Win)")+
theme_minimal()

###### PSYCHOMETRIC FUNCTION ######
Delta_U = seq(-5, 5, 0.001)
p_0 = 1/(1+exp(-1*Delta_U))
p_1 = 1/(1+exp(-0.85*Delta_U))
p_2 = 1/(1+exp(-0.65*Delta_U))
p_3 = 1/(1+exp(-0.25*Delta_U))


Probability = data.frame(Probability = c(p_0, p_1, p_2, p_3),
                    Delta_U = c(Delta_U, Delta_U, Delta_U, Delta_U),
                    Mu= c(rep(1, length(Delta_U)),
                            rep(0.85, length(Delta_U)),
                            rep(0.65, length(Delta_U)),
                            rep(0.25, length(Delta_U))))
Probability$Mu = as.factor(Probability$Mu)

ggplot(Probability, aes(Delta_U, Probability, group=Mu, color=Mu))+
geom_line(size=1)+
  ylab("P(Risky)")+
theme_minimal()

##### DATA ANALYSIS ####
data = read.csv("Data.csv")

head(data, 10)

# Percent Gamble by Participant
P_Gamble_PID = aggregate(Gamble ~ PID+condition, data, mean)
head(P_Gamble_PID, 10)

# Average Percent Gamble by Condition
aggregate(Gamble~ condition, P_Gamble_PID, mean)

# Test Difference
t.test(P_Gamble_PID$Gamble[P_Gamble_PID$condition==1], 
       P_Gamble_PID$Gamble[P_Gamble_PID$condition==2], var.equal=FALSE)

#Plot Distributions
P_Gamble_PID$condition = as.factor(P_Gamble_PID$condition)
ggplot(P_Gamble_PID, aes(Gamble, group=condition, fill=condition, alpha=condition))+
geom_histogram(bins=15, position="identity") +theme_minimal()+ 
ggtitle("P(Gamble) by Condition")+
scale_alpha_manual(values=c(1,0.5)) + scale_fill_manual(values=c("hotpink1", "deepskyblue"))


##### MLE #####
# Load the stats4 Library
# IF YOU DO NOT HAVE IT INSTALLED UNCOMMENT THE NEXT LINE
#install.packages("stats4")
library(stats4)


#Make data frames to hold your results
fit_params=data.frame()
logs=data.frame()

#How many subjects are we going to have to estimate parameters for?
Nsubjects=length(unique(data$PID))


#Loop through the subjects to estimate their parameters
for(i in 1:(Nsubjects)){

    #Define the loglikelihood function we will use to estimate the parameters
    #Remember the output should be the sum of the logs of the likelihoods
    #Likelihood is the probability the supplied parameters explain the observed data
    LL <- function(alpha, beta, mu) {
        #Only look at the data for the participant
        temp<-data[data$PID==i,]
        
        #Make assumptions about the value of the parameters
        #always positive
        alpha = exp(alpha)
        beta= exp(beta)
        mu = exp(mu)
    
        #Estimate the Utility of the gamble
        U_gamble = (((temp$P_Gamble)^beta)/((((temp$P_Gamble)^beta) + ((1-temp$P_Gamble)^beta))^(1/beta)))
        U_gamble = U_gamble*(temp$A_Gamble)^alpha
        
        #Estimate the Utility of the certain
        U_certain = 1*(temp$A_Certain)^alpha
        
        #Calculate the probbility of selecting the gamble given Delta U
        #This is the probability our parameters explain the data for the trials in which
        #the paeticipant chose the Gamble
        Likelihood = (((1)/(1+exp((-1)*mu*(U_gamble-U_certain)))))
    
        #Invert the probability for the trials in which the participant actually chose the certain option
        Likelihood[which(temp$Gamble==0)] = 1-Likelihood[which(temp$Gamble==0)]
        
        #Turn the data frame into a vector so we can log each value and sum it all up
        Likelihood = unlist(Likelihood)
  
        #Log each likelihood and then sum them up...
        #Add any Additional distributional constraints
        #Here I am telling the function to penalize any estimate which is "extreme"
        #by assuming the estimated parameters should be close to some values taken from the literature
        -sum(log(Likelihood)) -log(dnorm(mu, exp(1),1)) - log(dnorm(alpha, 0.88, 0.15)) - log(dnorm(beta, 0.6, 0.15))
    }
  
    #Make a temporary variable to store the estimates
    #We're going to be running the MLE a few times to make sure we get a good estimate for the parameters
    temp1=c()
    temp2=c()
    
    #Run the MLE a few times... here I chose 10 for the sake of time
    for(k in 1:10){
        fit_param = tryCatch({ mle(LL, start = list(alpha=runif(1,-0.5,0), beta=runif(1,-0.5,0),
                                                mu=runif(1,1,1.5)))},
                                                error = function(e){
                                                               NULL})
        #Make sure the MLE produced an estimate
        #If something went wrong the MLE function will return NULL
        #Thus I'm checking to make sure the output is not NULL before adding it to the temporary variable
        if(length(fit_param)>0){
          temp1 = rbind(temp1,fit_param@coef)
          temp2 = rbind(temp2, LL(fit_param@coef[1], fit_param@coef[2], fit_param@coef[3]))
        }
    }
  
    #Now that we have a few estimates for the parameter values
    #Look at the estimates and find the one with the smallest logLikelihood (closest to 0)
    #Add the PID to the resulting Data Fa=rame so I know who this estimate is for!
    
    #First I am checking if there is a tie for the smallest value
    # If there is just return whatever was the first estimate & the LL was associated with the first instance
    if(length(temp1[which(temp2==min(temp2)),])>1){
        fit_params=rbind(fit_params, c(temp1[which(temp2==min(temp2))[1],], PID=i))
        logs=rbind(logs, temp2[which(temp2==min(temp2)[1]),])
    } 
    #If NOT then just save the minimum value
    else{
        fit_params=rbind(fit_params, c(temp1[which(temp2==min(temp2)),], PID=i))
        logs=rbind(logs, temp2[which(temp2==min(temp2)),])
    }
}
#Re-Name the colums of the MLE Estimates into something understandable
#The order in which I name them depends on the order in which I supplied them to the LL and MLE functions above
colnames(fit_params)<-c("Fit_alpha", "Fit_beta", "Fit_mu", "PID")

# What does this data Frame look like?
head(fit_params, 10)

# Turn the parameters back into values we can interpet
fit_params$Fit_alpha = exp(fit_params$Fit_alpha)
fit_params$Fit_beta = exp(fit_params$Fit_beta)
fit_params$Fit_mu = exp(fit_params$Fit_mu)

#Print out the Means of the Estimates
mean(fit_params$Fit_alpha)
mean(fit_params$Fit_beta)
mean(fit_params$Fit_mu)

# Load the plyr Library
# IF YOU DO NOT HAVE IT INSTALLED UNCOMMENT THE NEXT LINE
#install.packages("plyr")
library(plyr)

# Map the values of the parameters onto the data frame
data$Fit_alpha = mapvalues(data$PID, fit_params$PID, fit_params$Fit_alpha)
data$Fit_beta = mapvalues(data$PID, fit_params$PID, fit_params$Fit_beta)
data$Fit_mu = mapvalues(data$PID, fit_params$PID, fit_params$Fit_mu)

temp = aggregate(Fit_alpha ~ PID+condition, data, mean)
t.test(temp$Fit_alpha[temp$condition==1], temp$Fit_alpha[temp$condition==2], var.equal = FALSE)

#Plot Distributions
qqnorm(temp$Fit_alpha[temp$condition==1]); qqline(temp$Fit_alpha[temp$condition==1])
qqnorm(temp$Fit_alpha[temp$condition==2]); qqline(temp$Fit_alpha[temp$condition==2])

temp$condition = as.factor(temp$condition)
ggplot(temp, aes(Fit_alpha, group=condition, fill=condition, alpha=condition))+
geom_histogram(bins=10, position="identity") +theme_minimal()+ 
ggtitle("Alpha by Condition")+
scale_alpha_manual(values=c(1,0.5)) + scale_fill_manual(values=c("hotpink1", "deepskyblue"))


Dollars = seq(0, 3, 0.1)
alpha = mean(temp$Fit_alpha[temp$condition==2])
U_0 = Dollars^alpha
alpha = mean(temp$Fit_alpha[temp$condition==1])
U_1 = Dollars^alpha
U_2 = Dollars


Utility = data.frame(Utility = c(U_0, U_1, U_2),
                    Dollars = c(Dollars, Dollars, Dollars),
                    Condition= c(rep(2, length(Dollars)),
                            rep(1, length(Dollars)),
                                rep(NA, length(Dollars))))
Utility$Condition = as.factor(Utility$Condition)


ggplot(Utility, aes(Dollars, Utility, group=Condition, color=Condition))+
geom_line(size=1)+
theme_minimal()

temp = aggregate(Fit_beta ~ PID+condition, data, mean)
t.test(temp$Fit_beta[temp$condition==1], temp$Fit_beta[temp$condition==2], var.equal = FALSE)

#Plot Distributions
qqnorm(temp$Fit_beta[temp$condition==1]); qqline(temp$Fit_beta[temp$condition==1])
qqnorm(temp$Fit_beta[temp$condition==2]); qqline(temp$Fit_beta[temp$condition==2])

temp$condition = as.factor(temp$condition)
ggplot(temp, aes(Fit_beta, group=condition, fill=condition, alpha=condition))+
ggtitle("Beta by Condition") +
geom_histogram(bins=10, position="identity") +theme_minimal()+ 
scale_alpha_manual(values=c(1,0.5)) + scale_fill_manual(values=c("hotpink1", "deepskyblue"))

#I Wouldn't test for group differences in the Mu parameter... 
#It is supposed to be a noise/temperature parameter
#Thus it is not really meaningful



probability = seq(0, 1, 0.001)
beta= mean(temp$Fit_beta[temp$condition==2])
p_0 = (probability^beta)/(((probability^beta)+(1-probability)^beta)^(1/beta))
beta= mean(temp$Fit_beta[temp$condition==1])
p_1 = (probability^beta)/(((probability^beta)+(1-probability)^beta)^(1/beta))
p_2 = probability



Probability = data.frame(Subjective_Probability = c(p_0, p_1, p_2),
                    Probability = c(probability, probability, probability),
                    Condition= c(rep(2, length(probability)),
                            rep(1, length(probability)),
                                rep(NA, length(probability))))
Probability$Condition = as.factor(Probability$Condition)

ggplot(Probability, aes(Probability, Subjective_Probability, group=Condition, color=Condition))+
geom_line(size=1)+
theme_minimal()

#### Now let's see how far off we we're from the 'real' value of these parameters
sum((aggregate(alpha ~ PID, data, mean)$alpha - aggregate(Fit_alpha ~ PID, data, mean)$Fit_alpha)**2)
sum((aggregate(beta ~ PID, data, mean)$beta - aggregate(Fit_beta ~ PID, data, mean)$Fit_beta)**2)

