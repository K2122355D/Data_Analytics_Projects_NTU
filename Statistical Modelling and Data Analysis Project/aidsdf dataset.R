#install.packages('JM')
#install.packages('survminer')

library(JM)  
library(ggplot2)
library(dplyr)  
library(geepack)  # For GEE model
library(survival) 
library(survminer) 

aids.id <- data.frame(aids.id)
write.csv(aids.id, file = "C:/Users/AN000/OneDrive/Desktop/Masters Program/aids.id.csv", row.names = FALSE)
summary(aids.id $Time)

aidsdf <- data.frame(aids)
head(aidsdf)
str(aidsdf)

# Check for missing values
colSums(is.na(aidsdf))

#converting to factors
aidsdf <- aids %>%
  mutate(across(where(is.factor), as.character)) %>%
  mutate(
    patient = as.factor(patient),
    death = as.factor(death),
    obstime = as.numeric(as.character(obstime)),  # Convert obstime to numeric for modeling
    drug = as.factor(drug),
    gender = as.factor(gender),
    prevOI = as.factor(prevOI),
    AZT = as.factor(AZT),
    death = as.factor(death),
    event = as.factor(event)
  )

summary(aidsdf$CD4)
mean(aidsdf$CD4)
sd(aidsdf$CD4)
summary(aidsdf$Time)
sd(aidsdf$Time)

summary(aidsdf$Time)
summary(aidsdf$stop)
sd(aidsdf$stop)

table(aidsdf$death)
levels(aidsdf$death)
table(aidsdf$drug)
levels(aidsdf$drug)
table(aidsdf$gender)
table(aidsdf$prevOI)
table(aidsdf$AZT)
table(aidsdf$event)
levels(aidsdf$AZT)

#Data exploratory work

ggplot(aidsdf, aes(x = CD4)) + 
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + 
  labs(title = "CD4 Count Distribution")


ggplot(aidsdf,aes(x=drug,fill = death))+
  geom_bar(position = "dodge",color = "black")+
  labs(title = "Drug Group Distribution by Death")+
  theme_minimal()

ggplot(aidsdf, aes(x = obstime, group = patient, y = CD4, color = prevOI)) + 
  geom_line(alpha = 0.6) + 
  labs(title = "CD4 Count Over Time by ", x = "Observation Time", y = "CD4 Count")


ggplot(aidsdf, aes(x = obstime, y = CD4, color = gender)) +
  geom_line() +
  geom_point() +
  labs(title = "CD4 Count Over Obstime by Gender")


ggplot(aidsdf, aes(x = Time, y = CD4, group = gender, color = drug)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ gender)+ 
  labs(title = "CD4 Count Over Time of death based on the gender", x = "Time", y = "CD4 Count")

ggplot(aidsdf, aes(x = Time, y = CD4, group = gender, color = gender)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ drug)+ 
  labs(title = "CD4 Count Over Time of death based on the drug type"
       , x = "Time", y = "CD4 Count")


# Average CD4 Count by obstime and Drug
average_cd4 <- aidsdf %>%
  group_by(obstime, drug) %>%
  summarize(average_CD4 = mean(CD4))

print(average_cd4)

ggplot(average_cd4, aes(x = obstime, y = average_CD4, fill = drug)) +
  geom_bar(stat = "summary", position = "dodge") +
  labs(title = "Average CD4 Count by obstime and Drug",
       x = "obstime",
       y = "Average CD4 Count")


# deep dive is there a difference in gender?

average_cd4_G <- aidsdf %>%
  group_by(obstime, drug, gender) %>%
  summarize(average_CD4 = mean(CD4))

print(average_cd4_G)

ggplot(average_cd4_G, aes(x = obstime, y = average_CD4, fill = drug)) +
  geom_bar(stat = "summary", position = "dodge") +
  facet_wrap(~ gender) +
  labs(title = "Average CD4 Count by obstime, Drug, and Gender",
       x = "obstime",
       y = "Average CD4 Count")

# from the plot seems like drug ddC works better on Females than Males

# deep dive is there a difference in AZT?

average_cd4_AZT <- aidsdf %>%
  group_by(obstime, drug, AZT) %>%
  summarize(average_CD4 = mean(CD4))

print(average_cd4_AZT)

ggplot(average_cd4_AZT, aes(x = obstime, y = average_CD4, fill = drug)) +
  geom_bar(stat = "summary", position = "dodge") +
  facet_wrap(~ AZT) +
  labs(title = "Average CD4 Count by obstime, Drug, and AZT",
       x = "obstime",
       y = "Average CD4 Count")

# from the plot seems like drug ddl performs better than ddC and 
# it stabilizes the CD4 count in AZT intolerance group

# deep dive is there a difference in prevOIAIDS?

average_cd4_prevOI <- aidsdf %>%
  group_by(obstime, drug, prevOI) %>%
  summarize(average_CD4 = mean(CD4))

print(average_cd4_prevOI)

ggplot(average_cd4_prevOI, aes(x = obstime, y = average_CD4, fill = drug)) +
  geom_bar(stat = "summary", position = "dodge") +
  facet_wrap(~ prevOI) +
  labs(title = "Average CD4 Count by obstime, Drug, and prevOIAIDS",
       x = "obstime",
       y = "Average CD4 Count")

# from the plot seems like drug ddl performs better than ddC and 
# it stabilizes the CD4 count in prevOI groups




library(ggplot2)

ggplot(aidsdf, aes(x = Time, y = CD4, color = drug)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ event) +
  labs(title = "Event by Intervention and Time")


library(dplyr)
#mean of CD4 count for groups = death (Yes/No)
aidsdf_death_summary <- aidsdf %>%
  group_by(death) %>%
  summarize(mean_CD4_count = mean(CD4))

aidsdf_death_summary

ggplot(aidsdf_death_summary, aes(x = death, y = mean_CD4_count)) +
  geom_line() +
  geom_point()

#no deaths = mean CD4 count = 8.03
# deaths = mean CD4 count = 4.59


#another layer -> whether AZT = failure/ intolerance

#mean of CD4 count for groups = death (Yes/No) and groups = AZT (Intolerance/Failure)
aidsdf_death_AZT_summary <- aidsdf %>%
  group_by(death, AZT) %>%
  summarize(mean_CD4_count = mean(CD4))

aidsdf_death_AZT_summary

ggplot(aidsdf_death_AZT_summary, aes(x = death, y = mean_CD4_count)) +
  geom_line() +
  geom_point()

#failure group has an average lower CD4 count than the intolerance group

#mean of CD4 count for groups = event (Yes/No)
aidsdf_event_summary <- aidsdf %>%
  group_by(event) %>%
  summarize(mean_CD4_count = mean(CD4))

aidsdf_event_summary

#not event of interest (usually drop out before the study finishes etc/ still alive
#after the study) etc = mean CD4 count = 7.49
# event of interest (deaths) & death during the study = mean CD4 count = 4.01


ggplot(aidsdf_event_summary, aes(x = event, y = mean_CD4_count)) +
  geom_line() +
  geom_point()

#mean of CD4 count for groups = event (Yes/No) and groups = Gender (Male/Female)
aidsdf_event_gender_summary <- aidsdf %>%
  group_by(event, gender) %>%
  summarize(mean_CD4_count = mean(CD4))

aidsdf_event_gender_summary

ggplot(aidsdf, aes(x = Time, y = CD4, color = drug)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ start) +
  labs(title = "CD4 Count Progression by Start Point (Facet Plots)")



#mean of CD4 count for groups = event (Yes/No) and groups = AZT (Intolerance/Failure)
aidsdf_event_AZT_summary <- aidsdf %>%
  group_by(event, AZT) %>%
  summarize(mean_CD4_count = mean(CD4))

aidsdf_event_AZT_summary

ggplot(aidsdf, aes(x = Time, y = CD4, color = AZT)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ start) +
  labs(title = "CD4 Count Progression by Start Point (Facet Plots)") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = "bottom")

#mean of CD4 count for groups = event (Yes/No) and groups = prev0I
aidsdf_event_AZT_summary <- aidsdf %>%
  group_by(event, AZT) %>%
  summarize(mean_CD4_count = mean(CD4))

aidsdf_event_AZT_summary

ggplot(aidsdf, aes(x = Time, y = CD4, color = prevOI)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ start) +
  labs(title = "CD4 Count Progression by Start Point (Facet Plots)") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),
        legend.position = "bottom")


#people with AIDS have lower cd4 cell count 

# Fit a Cox proportional hazards model
cox_model <- coxph(Surv(stop, event) ~ drug + gender + prevOI + AZT, data = aidsdf)

summary(cox_model)

# Kaplan-Meier survival curve
km_fit <- survfit(Surv(stop, event) ~ drug, data = aidsdf)

ggsurvplot(km_fit, data = aidsdf,
           pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE,
           conf.int.style = "default",   # Use default confidence interval (log transformation)
           xlab = "Time (months)",       
           ylab = "Survival Probability",
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Curves by Drug Group")

# Kaplan-Meier survival curve
km_fit <- survfit(Surv(stop, event) ~ prevOI, data = aidsdf)

ggsurvplot(km_fit, data = aidsdf,
           pval = TRUE, conf.int = TRUE, 
           risk.table = TRUE,
           conf.int.style = "default",   # Use default confidence interval (log transformation)
           xlab = "Time (months)",       
           ylab = "Survival Probability",
           ggtheme = theme_minimal(),
           title = "Kaplan-Meier Survival Curves by PrevOI")


library(survival)
library(survminer)

# Fit the Kaplan-Meier survival model for drug groups
km_fit <- survfit(Surv(stop, event) ~ prevOI, data = aidsdf)

# Plot log cumulative hazard using ggsurvplot
ggsurvplot(km_fit, 
           data = aidsdf, 
           fun = "cloglog",            
           pval = TRUE,                 # Display p-value for log-rank test
           conf.int.style = "default",             # Show confidence intervals
           xlab = "Time (months)",      
           ylab = "Log Cumulative Hazard", 
           title = "Log Cumulative Hazard by PrevOI", 
           ggtheme = theme_minimal())   


#Research Qn: Does the effect of a drug on the occurrence of an 
#event (e.g., disease progression or death) change over time?

#by removing obstime where by 2 is present so that obstime is taken on 0,6,12,18
aidsdf_1 <- data.frame(aids)
head(aidsdf_1)
str(aidsdf_1)
aidsdf_1$obstime.f <- as.factor(aidsdf_1$obstime)

# Create a logical vector indicating which rows have obstime equal to 2
keep_rows <- aidsdf_1$obstime != 2

# Filter the data frame to keep only the rows where obstime is not equal to 2
aidsdf_1_filtered <- aidsdf_1[keep_rows, ]
aidsdf_1_filtered$obstime.f <- droplevels(aidsdf_1_filtered$obstime.f)

#to investigate the relationship between drug, observation time (obstime.f), 
#and the occurrence of an event (presumably a binary outcome). 

# covariance structure = ar1

gee.ar1_f <-geeglm(event ~ drug + obstime.f + drug:obstime.f, data=aidsdf_1_filtered, id=patient, 
                 family=binomial(), corstr="ar1") 

summary(gee.ar1_f)
anova(gee.ar1_f)
exp(0.931089)
exp(-1.4782)

#print.ORCIs.gee(gee.ar1_f)
QIC(gee.ar1_f)
#QIC value is 762.567

# covariance structure = unstructured

gee.uns_f <-geeglm(event ~ drug + obstime.f + drug:obstime.f, data=aidsdf_1_filtered, id=patient, 
                 family=binomial(), corstr="unstructured") 

summary(gee.uns_f)
anova(gee.uns_f)

#print.ORCIs.gee(gee.uns_f)
QIC(gee.uns_f)
#QIC value is 762.31

# covariance structure = exchangeable

gee.exe_f <-geeglm(event ~ drug + obstime.f + drug:obstime.f, data=aidsdf_1_filtered, id=patient, 
                 family=binomial(), corstr="exchangeable") 

summary(gee.exe_f)
anova(gee.exe_f)

#print.ORCIs.gee(gee.exe_f)
QIC(gee.exe_f)
#QIC value is 762.42 


#to investigate the relationship between drug, observation time (obstime.f), 
#and the occurrence of an event (presumably a binary outcome). 

# covariance structure = ar1
aidsdf_1 <- data.frame(aids)
head(aidsdf_1)
str(aidsdf_1)
aidsdf_1$obstime.f <- as.factor(aidsdf_1$obstime)

gee.ar1 <-geeglm(event ~ drug + obstime.f + drug:obstime.f, data=aidsdf_1, id=patient, 
                 family=binomial(), corstr="ar1") 

summary(gee.ar1)
anova(gee.ar1)

#print.ORCIs.gee(gee.ar1)
QIC(gee.ar1)
#QIC value is 1067.64

# covariance structure = unstructured

gee.uns <-geeglm(event ~ drug + obstime.f + drug:obstime.f, data=aidsdf_1, id=patient, 
                 family=binomial(), corstr="unstructured") 

summary(gee.uns)
anova(gee.uns)

#print.ORCIs.gee(gee.uns)
QIC(gee.uns)
#QIC value is 1067.49 

# covariance structure = exchangeable

gee.exe <-geeglm(event ~ drug + obstime.f + drug:obstime.f, data=aidsdf_1, id=patient, 
                 family=binomial(), corstr="exchangeable") 

summary(gee.exe)
anova(gee.exe)

#print.ORCIs.gee(gee.exe)
QIC(gee.exe)
#QIC value is 1067.27 


#Research Qn: Investigate if patient-level characteristics 
#(e.g. gender, baseline disease severity) 
#modify the drug-time interaction.

#Does the effect of a drug on the occurrence of an event change over time, 
#and how are these effects influenced by gender? 

# covariance structure = ar1
aidsdf_1 <- data.frame(aids)
head(aidsdf_1)
str(aidsdf_1)
aidsdf_1$obstime.f <- as.factor(aidsdf_1$obstime)

gee.ar1_g <-geeglm(event ~ drug + obstime.f + drug:obstime.f
                   + gender + gender:obstime.f, data=aidsdf_1, id=patient, 
                   family=binomial(), corstr="ar1") 

summary(gee.ar1_g)
anova(gee.ar1_g)

#print.ORCIs.gee(gee.exe)
QIC(gee.ar1_g)
#QIC: 1049.393


#Does the effect of a drug on the occurrence of an event change over time, 
#and how are these effects influenced by gender, AZT status?


# covariance structure = ar1
aidsdf_1 <- data.frame(aids)
head(aidsdf_1)
str(aidsdf_1)
aidsdf_1$obstime.f <- as.factor(aidsdf_1$obstime)

gee.ar1_A <-geeglm(event ~ drug + obstime.f + drug:obstime.f
                   + gender + gender:obstime.f +
                     AZT + AZT:obstime.f, data=aidsdf_1, id=patient, 
                   family=binomial(), corstr="ar1") 

summary(gee.ar1_A)
anova(gee.ar1_A)

#print.ORCIs.gee(gee.exe)
QIC(gee.ar1_A)
#QIC: 1017.296

#Does the effect of a drug on the occurrence of an event change over time, 
#and how are these effects influenced by gender, AZT status, 
#and previous opportunistic infections (prevOIAIDS)?

#using the filtered aids dataset whereby obstime =2 is removed

# covariance structure = ar1
aidsdf_1 <- data.frame(aids)
head(aidsdf_1)
str(aidsdf_1)
aidsdf_1$obstime.f <- as.factor(aidsdf_1$obstime)

gee.ar1_c_f <-geeglm(event ~ drug + obstime.f + drug:obstime.f
                   + gender + gender:obstime.f +
                     AZT + AZT:obstime.f +
                     prevOI + AZT:obstime.f, data=aidsdf_1_filtered, id=patient, 
                   family=binomial(), corstr="ar1") 

summary(gee.ar1_c_f)
anova(gee.ar1_c_f)

#print.ORCIs.gee(gee.ar1_c_f)
QIC(gee.ar1_c_f)
#QIC: 695.547




# covariance structure = ar1
aidsdf_1 <- data.frame(aids)
head(aidsdf_1)
str(aidsdf_1)
aidsdf_1$obstime.f <- as.factor(aidsdf_1$obstime)

gee.ar1_c <-geeglm(event ~ drug + obstime.f + drug:obstime.f
                   + gender + gender:obstime.f +
                   AZT + AZT:obstime.f +
                  prevOI + AZT:obstime.f, data=aidsdf_1, id=patient, 
                 family=binomial(), corstr="ar1") 

summary(gee.ar1_c)
anova(gee.ar1_c)

#print.ORCIs.gee(gee.exe)
QIC(gee.ar1_c)
#QIC: 984.00





#question to ask teacher do we test individually? or a combination of those factors?



####Max to take a look at the below################################################

#Research Qn:How does CD4 cell count change over time for patients on ddC and ddI?
#Within-subject effect (obstime.f: time points at which the CD4 cells count was recorded)
#Between-subject effect (drug treatment)

#gee.exc <- geeglm(CD4 ~ Time + drug + Time:drug, data = aidsdf_1, id = patient, 
#                  family = "gaussian", corstr = "exchangeable")

#summary(gee.exc)
#anova(gee.exc)
#print.ORCIs.gee(gee.exc)
#QIC(gee.exc)

#exchangeable

#gee.exc <- geeglm(CD4 ~ Time + drug + Time:drug, data = aidsdf_1, id = patient, 
#                  family = "gaussian", corstr = "exchangeable")

#summary(gee.exc)
#anova(gee.exc)
#print.ORCIs.gee(gee.exc)
#QIC(gee.exc)

#independence
#gee.ind <- geeglm(CD4 ~ Time + drug + Time:drug, data = aidsdf_1, id = patient, 
#                  family = "gaussian", corstr = "independence")

#summary(gee.ind)
#anova(gee.ind)

#print.ORCIs.gee(gee.ind)
#QIC(gee.ind)


"unstructured"
#gee.uns <- geeglm(CD4 ~ Time + drug + Time:drug, data = aidsdf_1, id = patient, 
#                  family = "gaussian", corstr = "unstructured")

#summary(gee.uns)
#anova(gee.uns)
#print.ORCIs.gee(gee.uns)
#QIC(gee.uns)




