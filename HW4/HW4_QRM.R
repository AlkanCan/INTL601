View(cses_imd)

df=cses_imd[c('IMD1006_NAM','IMD1008_YEAR','IMD1009','IMD5054_2','IMD3001','IMD3006','IMD2003','IMD5051_2','IMD2001_1','IMD2016','IMD5007','IMD2007','IMD2002','IMD2005_2')]
colnames(df)=c('country','year','election_type','unemployment_t1','turnout_main_election','ideology_self','Education','polity_score','age','socio_econ','compulsory_voting','rural_urban','gender','religiosity')
#Dependent Variable: Turnout:Main Election (whether the respondent voted or not for the main election. For this variable in the absence of a presidential election in the specified year, the choice for another election is taken into account, e.g lower house)

#Explanatory Variable: There are 3 variables that can be indicators for economic health:Inflation rate, annual GDP growth, unemployment rate

#I selected unemployment rate (t-1) but for the other two variables, two different models can also be constructed

#Controls:Ideology,education, polity score(democracy-autocracy),Rural-Urban Residence, Age,Compulsory Voting, Religiosioty

#These variables are not expected to affect unemployment rate, but theoretically expected to influence voting decision. Therefore they are included in the model

#Polity Score: In democracies voting matters more than it does in autocracies because it can actually change something. Therefore it can be expected that in democracies citizens tend to vote more than they do in autocracies.Regarding this I included polity score as a control variable.

#All the missing variables are replaced with NaN and omitted while running the model
df$turnout_main_election[df$turnout_main_election>=9999993]<-NA
df$ideology_self[df$ideology_self>=95]<-NA
df$Education[df$Education>=6]<-NA
polityreplace=c(-66,-77,-88,99)
df$polity_score[df$polity_score %in% polityreplace]<-NA
df$age[df$age>=9997]<-NA
df$socio_econ[df$socio_econ>=5]<-NA
df$compulsory_voting[df$compulsory_voting>=7]<-NA
df$rural_urban[df$rural_urban>=7]<-NA
df$gender[df$gender>=3]<-NA
df$religiosity[df$religiosity>=7]<-NA

#I used logistic regression since the outcome is binary

#To see whether the economic health is conditional on self ideological replacement, I added an interaction term



mod=glm(turnout_main_election ~ unemployment_t1*ideology_self + Education + polity_score+
      age + compulsory_voting +
      religiosity,
    family = binomial, data=df,na.action = na.omit)
#Based on the results, the relation between unemployment rate from a year before the election and voting turnout is positive and significant. It means that people tends to vote more when unemployment rate is high a year before the election.
#The effect of interaction term is not significant indicating that the effect of economic health is not conditional on self-ideological replacement

#Since the N is large and data seems to be balanced, a fixed effect model can be used
#One way fixed effect (unit)
modf1=glm(turnout_main_election ~ unemployment_t1*ideology_self + Education + polity_score+
            age + compulsory_voting +
            religiosity+as.factor(country),
          family = binomial, data=df,na.action = na.omit)
summary(modf1)



#One way fixed effect (time)
modf1y=glm(turnout_main_election ~ unemployment_t1*ideology_self + Education + polity_score+
            age + compulsory_voting +
            religiosity+as.factor(year),
          family = binomial, data=df,na.action = na.omit)
summary(modf1y)

#Based on the results of one way fixed effect models, the relationship between explanatory and dependent variable are still positive but significant only at 0.1 significance level

#TWo way fixed effect

modf2=glm(turnout_main_election ~ unemployment_t1*ideology_self + Education + polity_score+
          age + compulsory_voting +
          religiosity+as.factor(country)+as.factor(year),
        family = binomial, data=df,na.action = na.omit)

#Based on the the results of two-way fixed effect model, the relationship between unemployment rate and turnout is still positive but not significant 
#I am not sure how to interpret this change in significance, feedback will be appreciated

#Standart errors  
library(sandwich)
library(multiwayvcov)
library(lmtest)
vcovCL1 <- cluster.vcov(mod, ~as.factor(df$country) + as.factor(df$year))
coeftest(mod, vcovCL1)

#Multiwayvcov package was not compatible with my version of R so it didn't work




#Future Work:
#Comparison to null model
#Dummy variables will be created for categorical variables (gender-socio econ) and they will be included in the model 
#A random effect model will be applied
#Models will be compared using Hausman Test
#Two alternative models will be constructed using inflation rate, annual GDP growth as explanatory variable
