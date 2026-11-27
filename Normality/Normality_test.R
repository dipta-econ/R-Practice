#============================================================
# Shapiro-Wilk Normality Test
# Model: Result (CGPA) = Study hour at class + Study hour at home
# First data will be analyzed by graphical methods and then analyzed by Shapiro-Wilk for Normality Test.
#============================================================


Dataset <- data.frame(
  CGPA = c(3.45, 3.54, 3.25, 3.78, 3.03, 3.52, 3.86, 3.35, 3.62, 3.31, 3.96, 3.23, 3.70, 
           3.41, 3.60, 3.47, 3.33, 3.84, 3.19, 3.68, 3.64, 3.09, 3.50, 4.00),
  Sthcl = c(10, 12, 8, 14, 6, 11, 15, 9, 13, 7, 16, 10, 12, 8, 14, 9, 11, 13, 6, 15, 10, 
            7, 12, 16),
  Sthhm = c(6, 8, 4, 10, 3, 7, 12, 5, 9, 6, 13, 4, 11, 7, 6, 9, 5, 12, 5, 8, 10, 3, 6, 14)
)

View(Dataset)
str(Dataset)


##graphical method
# for CGPA
plot(Dataset$CGPA)
boxplot(Dataset$CGPA)
plot(density(Dataset$CGPA), col = "blue", lwd = 2)
hist(Dataset$CGPA, freq = FALSE)
lines(density(Dataset$CGPA), col = "red", lwd = 2)
qqnorm(Dataset$CGPA, main = "QQ Plot for CGPA"); qqline(Dataset$CGPA, lwd = 2)


#for Study hour at class (Sthcl)
plot(Dataset$Sthcl)
boxplot(Dataset$Sthcl)
plot(density(Dataset$Sthcl), col = "blue", lwd = 2)
hist(Dataset$Sthcl, freq = FALSE)
lines(density(Dataset$Sthcl), col = "red", lwd = 2)
qqnorm(Dataset$Sthcl, main = "QQ Plot for Study hour at class"); qqline(Dataset$Sthcl, lwd = 2)


#for Study hour at home (Sthhm)
plot(Dataset$Sthhm)
boxplot(Dataset$Sthhm)
plot(density(Dataset$Sthhm), col = "blue", lwd = 2)
hist(Dataset$Sthhm, freq = FALSE)
lines(density(Dataset$Sthhm), col = "red", lwd = 2)
qqnorm(Dataset$Sthhm, main = "QQ Plot for Study hour at home"); qqline(Dataset$Sthhm, lwd = 2)



# Shapiro-Wilk Normality Test
# #H0: The data is normally distributed
## H1: The data is not normally distributed

shapiro.test(Dataset$CGPA) #W = 0.98394, p-value = 0.9559
shapiro.test(Dataset$Sthcl) #W = 0.95651, p-value = 0.3724
shapiro.test(Dataset$Sthhm) #W = 0.95104, p-value = 0.2853

# Here W value of CGPA is 0.98394 which is very close to 1(Normal). Since P-Value is 0.9559 > 0.05, we fail to reject null hypothesis. CGPA is normally distributed.
# Also W value of Study hour at class is 0.95651, very close to 1(Normal). Since P-Value is 0.3724 > 0.05, we fail to reject null hypothesis. Study hour at class is normally distributed.
# And W value of Study hour at home is 0.95104 which is very close to 1(Normal). Since P-Value is 0.2853 > 0.05, we fail to reject null hypothesis. Study hour at home is normally distributed.
# Finally we conclude that all the variables are normally distributed in the model.
