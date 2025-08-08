### Final Project
# Rahmannuaji Satuhu
# Khalida Aurora Amanda Putri

# Library
library(survival)
library(survminer)
library(dplyr)
library(ggsurvfit)
library(psych)


# Load Dataset
df <- read.csv("D:/KULIAH/Semester 7/Pengantar Analisis Survival/FP ANSUR/heart_failure_clinical_records_dataset.csv")
head(df)
View(df)

# Describe
summary(df)
describe(df)

# Checking 
sum(duplicated(df))
colSums(is.na(df))

# Change Name
df <- df %>%
  rename(
    status = DEATH_EVENT
  )

# Define
y <- Surv(df$time, df$status==1)

# KM Curve
plot(survfit(y ~ 1),
     xlab = "Survival Time (Days)",
     ylab = "Survival Probabilities")
survfit(y ~ 1) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability") +
  add_confidence_interval()

# KM Curve Based On Age
age1 <- ifelse(df$age >= 64, "Non-Productive", "Productive")
fit <- survfit(y ~ age1)
plot(fit,
     col = c("blue", "red"), # Warna untuk masing-masing kelompok
     lty = 1:2, # Jenis garis untuk membedakan kelompok
     xlab = "Survival Time (Days)",
     ylab = "Survival Probabilities",
     main = "Kaplan-Meier Survival Curve by Age")
legend("topright",
       legend = c("Non-Productive", "Productive"),
       col = c("blue", "red"),
       lty = 1)

survfit(y ~ age1) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve by Age"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Non-Productive", "Productive")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on Anaemia
survfit(y ~ df$anaemia) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve
    by Anaemia"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Non Anaemia", "Anaemia")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on High Blood Pressure
survfit(y ~ df$high_blood_pressure) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve by 
    High Blood Pressure"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Not Hypertension", "Hypertension")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on Creatinine Phosphokinase
creatinine <- ifelse(df$creatinine_phosphokinase > 120, "High", "Normal")
survfit(y ~ creatinine) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve by 
    Creatinine Phosphokinase"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("High", "Normal")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on Diabetes
survfit(y ~ df$diabetes) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve by 
    Diabetes"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Non diabetes", "Diabetes")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

# KM Curve Based EF
ef <- ifelse(df$ejection_fraction >= 40 & df$ejection_fraction <= 70, 
             "No-Heart Failure", 
             "Heart Failure")
fit3 <- survfit(y ~ ef)
plot(fit3,
     col = c("blue", "red"), # Warna untuk masing-masing kelompok
     lty = 1:2, # Jenis garis untuk membedakan kelompok
     xlab = "Survival Time (Days)",
     ylab = "Survival Probabilities",
     main = "Kaplan-Meier Survival Curve by EF")
legend("topright",
       legend = c("No-Heart Failure", "Heart Failure"),
       col = c("blue", "red"),
       lty = 1)
survfit(y ~ ef) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve
    by Ejection Fraction"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("No-Heart Failure", "Heart Failure")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on Sex
survfit(y ~ df$sex) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve by Sex"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Female", "Male")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on Platelets
platelets1<- ifelse(df$platelets >= 150000 & df$platelets <= 450000, 
                    "Normal", 
                    "Abnormal")
survfit(y ~ platelets1) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve by Platelets"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Normal", "Abnormal")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on Serum Creatinine
ser_creatinine <- ifelse(df$serum_creatinine > 1.5, "High", "Normal")
survfit(y ~ ser_creatinine) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve
    by Serum Creatinine"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("High", "Normal")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

#KM Curve Based on Serum Creatinine
ser_sodium <- ifelse(df$serum_sodium < 135, "Low", "Normal")
survfit(y ~ ser_sodium) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve
    by Serum Sodium"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("Low", "Normal")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))

# KM Curve Based On Smoke
fit1 <- survfit(y ~ smoking, data = df)
plot(fit1,
     col = c("blue", "red"), # Warna untuk masing-masing kelompok
     lty = 1:2, # Jenis garis untuk membedakan kelompok
     xlab = "Survival Time (Days)",
     ylab = "Survival Probabilities",
     main = "Kaplan-Meier Survival Curve by Smoke")
legend("topright",
       legend = levels(as.factor(df$smoking)),
       col = c("blue", "red"),
       lty = 1)

survfit(y ~ df$smoking) %>%
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall Survival Probability",
    title = "Kaplan-Meier Survival Curve by Smoking"
  ) +
  add_confidence_interval() +
  scale_color_manual(
    values = c("blue", "red"),
    labels = c("No Smoke", "Smoke")
  ) +
  guides(fill = "none", color = guide_legend()) +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))


# Log-Rank Test
survdiff(y ~ age1)
survdiff(y ~ df$anaemia)
survdiff(y ~ df$high_blood_pressure)
survdiff(y ~ creatinine)
survdiff(y ~ df$diabetes)
survdiff(y ~ ef)
survdiff(y ~ df$sex)
survdiff(y ~ platelets1)
survdiff(y ~ ser_creatinine)
survdiff(y ~ ser_sodium)
survdiff(y ~ df$smoking)

# 95% CI
survfit(y ~ age1)
survfit(y ~ ef)

# Model Cox PH
y <- Surv(df$time, df$status==1)

model <- coxph(formula=y ~ age + anaemia + creatinine_phosphokinase +
                 diabetes + ejection_fraction + high_blood_pressure + platelets + serum_creatinine
               + serum_sodium + sex + smoking, data = df)
summary(model)
gof1 <- cox.zph(model, transform = "identity")
gof1

# Pemeriksaan Asumsi PH
# Grafik -ln(-ln S^) KM plot
minlog <- function(x){
  return(-log(-log(x)))
}

log_km_fit_age <- survfit(y ~ age1)
plot(log_km_fit_age, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Age")
legend("topright", legend = c("Non-Productive", "Productive"), 
       col = c("red", "blue"), lty = 1, title = "Category")

log_km_fit_anaemia <- survfit(y ~ anaemia, data = df)
plot(log_km_fit_anaemia, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Anaemia")
legend("topright", legend = levels(as.factor(df$anaemia)), 
       col = c("red", "blue"), lty = 1, title = "Anaemia")

log_km_fit_creatinine <- survfit(y ~ creatinine)
plot(log_km_fit_creatinine, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Creatinine Phosphokinase")
legend("topright", legend = c("High", "Normal"), 
       col = c("red", "blue"), lty = 1, title = "Creatinine Phosphokinase")

log_km_fit_diabetes <- survfit(y ~ diabetes, data = df)
plot(log_km_fit_diabetes, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Diabetes")
legend("topright", legend = levels(as.factor(df$diabetes)), 
       col = c("red", "blue"), lty = 1, title = "Diabetes")

log_km_fit_ef <- survfit(y ~ ef)
plot(log_km_fit_ef, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Ejection Fraction")
legend("topright", legend = c("No-Heart Failure", "Heart Failure"), 
       col = c("red", "blue"), lty = 1, title = "Ejection Fraction")

log_km_fit_blood <- survfit(y ~ high_blood_pressure, data = df)
plot(log_km_fit_blood, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for High Blood Pressure")
legend("topright", legend = levels(as.factor(df$high_blood_pressure)), 
       col = c("red", "blue"), lty = 1, title = "High Blood Pressure")

log_km_fit_platelets1 <- survfit(y ~ platelets1)
plot(log_km_fit_platelets1, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Platelets")
legend("topright", legend = c("Normal", "Abnormal"), 
       col = c("red", "blue"), lty = 1, title = "Platelets")

log_km_fit_ser_creatinine <- survfit(y ~ ser_creatinine)
plot(log_km_fit_ser_creatinine, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Serum Creatinine")
legend("topright", legend = c("High", "Normal"), 
       col = c("red", "blue"), lty = 1, title = "Serum Creatinine")

log_km_fit_ser_sodium <- survfit(y ~ ser_sodium)
plot(log_km_fit_ser_sodium, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Serum Sodium")
legend("topright", legend = c("High", "Normal"), 
       col = c("red", "blue"), lty = 1, title = "Serum Sodium")

log_km_fit_sex <- survfit(y ~ sex, data = df)
plot(log_km_fit_sex, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Sex")
legend("topright", legend = levels(as.factor(df$sex)), 
       col = c("red", "blue"), lty = 1, title = "Sex")

log_km_fit_smoking <- survfit(y ~ smoking, data = df)
plot(log_km_fit_smoking, fun = minlog, col = c("red", "blue"), 
     xlab = "Time (day)", ylab = "-log(-log(Survival Probability))", 
     main = "-log(-log S(t)) Plot for Smoking")
legend("topright", legend = levels(as.factor(df$smoking)), 
       col = c("red", "blue"), lty = 1, title = "Smoking")


#Model Stratified - No interaction
model_stra1 = coxph(formula=y ~ age1 + anaemia + creatinine +
                      diabetes + high_blood_pressure + platelets1 + ser_creatinine
                    + ser_sodium + sex + smoking + strata(ef), data=df)
model_stra1
summary(model_stra1)

model_stra2 = coxph(formula=y ~ age1 + anaemia + creatinine 
                    + high_blood_pressure + platelets1 + ser_creatinine
                    + ser_sodium + sex + smoking + strata(ef), data=df)
model_stra2
summary(model_stra2)

model_stra3 = coxph(formula=y ~ age1 + anaemia + creatinine +
                      diabetes + high_blood_pressure + platelets1 + ser_creatinine
                    + ser_sodium + smoking + strata(ef), data=df)
model_stra3
summary(model_stra3)

model_stra4 = coxph(formula=y ~ age1 + anaemia 
                    + high_blood_pressure + ser_creatinine
                    + ser_sodium + strata(ef), data=df)
model_stra4
summary(model_stra4)

## Variabel mana yang baik
anova(model_stra4, model_stra1, test ="LRT")
pchisq(-2*model_stra2$loglik[2]-(-2*model_stra1$loglik[2]), 1, lower.tail = FALSE)
pchisq(-2*model_stra3$loglik[2]-(-2*model_stra1$loglik[2]), 1, lower.tail = FALSE)
pchisq(-2*model_stra4$loglik[2]-(-2*model_stra1$loglik[2]), 1, lower.tail = FALSE)

#Model Stratified - interaction
model_stra_interaction1 = coxph(formula=y ~ age1 + creatinine +
                                  diabetes + platelets1 + ser_creatinine
                                + ser_sodium + sex + smoking + strata(ef)*age1 + strata(ef)*creatinine +
                                  strata(ef)*diabetes + strata(ef)*platelets1 + strata(ef)*ser_creatinine + 
                                  strata(ef)*ser_sodium + strata(ef)*sex + strata(ef)*smoking, data=df)
summary(model_stra_interaction1)

model_stra_interaction2 = coxph(formula=y ~ age1 + anaemia 
                                + high_blood_pressure + ser_creatinine
                                + ser_sodium + strata(ef)*age1 + strata(ef)*ser_creatinine + 
                                  strata(ef)*ser_sodium, data=df)
summary(model_stra_interaction2)



anova(model_stra4, model_stra_interaction2, test="LRT")
LR <- -2*(model_stra1$loglik[2]-model_stra_interaction1$loglik[2])
LR <- -2*(model_stra4$loglik[2]-model_stra_interaction2$loglik[2])
LR
chisqtab <- qchisq(0.95,2)
chisqtab
# KEPUTUSAN
if(LR > chisqtab){
  print("Tolak H0")
  print("Model Dengan Interaksi Lebih Tepat")
}else{
  print("Gagal Tolak H0")
  print("Model Tanpa Interaksi Lebih Tepat")
}

# Final Model Stratified
model_stra4 = coxph(formula=y ~ age1 + anaemia 
                    + high_blood_pressure + ser_creatinine
                    + ser_sodium + strata(ef), data=df)
model_stra4
summary(model_stra4)

# Model Extended
df.cp <- survSplit(df, cut=df$time[df$status==1], 
                   end="time", event = "status", start = "start", id="id")
df.cp$logtef <- df.cp$ejection_fraction*log(df.cp$time)
df.cp[df.cp$id==16,c('id','start','time','status','ejection_fraction','logtef')]

# model extended cox
extended = coxph(Surv(df.cp$start,df.cp$time,df.cp$status) ~ 
                   age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction +
                   high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex +
                   smoking + logtef + cluster(id),data=df.cp)           

# cek gof
gof.ex = cox.zph(extended)
gof.ex

# time 50 days
df.cp50 <- survSplit(df,cut=50,end="time",event="status",start="start",id="id")

# heaviside
df.cp50$hv1 <- df.cp50$ejection_fraction*(df.cp50$start<50)
df.cp50$hv2 <- df.cp50$ejection_fraction*(df.cp50$start>=50)

df.cp50 <- df.cp50[order(df.cp50$id,df.cp50$start), ]
View(df.cp50)

Y50 <- Surv(df.cp50$start,df.cp50$time,
            df.cp50$status)

# 2 heaviside function
hvf = coxph(Y50 ~ age + anaemia + creatinine_phosphokinase + diabetes + high_blood_pressure 
            + platelets + serum_creatinine + serum_sodium + sex +
              smoking + hv1 + hv2 + cluster(id),
            data=df.cp50)
summary(hvf)
gof_hvf = cox.zph(hvf);gof_hvf

# stack exchange syntax
mod.extended = coxph(Surv(df.cp$start,df.cp$time,df.cp$status) ~ 
                       age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction +
                       high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex +
                       smoking,data=df.cp)
summary(mod.extended)
gof.ex0 = cox.zph(mod.extended);gof.ex0
mod.extended1 = coxph(Surv(df.cp$start,df.cp$time,df.cp$status) ~ 
                        age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction +
                        high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex +
                        smoking + ejection_fraction:time,data=df.cp)
summary(mod.extended1)
gof.ex1=cox.zph(mod.extended1);gof.ex1

# Model Extended
# split time
df.cp <- survSplit(df, cut=df$time[df$status==1], 
                   end="time", event = "status", start = "start", id="id")

# extended cox
mod.extended = coxph(Surv(df.cp$start,df.cp$time,df.cp$status) ~ 
                       age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction +
                       high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex +
                       smoking,data=df.cp)
summary(mod.extended)
gof.ex0 = cox.zph(mod.extended);gof.ex0

# extended cox 2
mod.extended1 = coxph(Surv(df.cp$start,df.cp$time,df.cp$status) ~ 
                        age + anaemia + creatinine_phosphokinase + diabetes + ejection_fraction +
                        high_blood_pressure + platelets + serum_creatinine + serum_sodium + sex +
                        smoking + ejection_fraction:time,data=df.cp)
summary(mod.extended1)
gof.ex1=cox.zph(mod.extended1);gof.ex1

# hazard ratio for id=8
p8 = df.cp[df.cp$id==8,c('id','start','time','status','ejection_fraction')]
beta_ef <- -0.02406
beta_ef_time <- -0.0004143
p8$HR <- exp(beta_ef + beta_ef_time*p8$time)
p8

# AIC 
extractAIC(model_stra4)[2]
extractAIC(mod.extended1)[2]
summary(model_stra4)$concordance
summary(mod.extended1)$concordance
