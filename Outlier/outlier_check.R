# ============================================================
## PRACTICE; Outlier Detection using IQR Method
## Variables; ID, Age, Income, Savings (n = 20)
## Steps; create dataset -> inspect -> boxplot -> IQR method -> find outliers ->cleaning -> compare    
# ============================================================

## Step 1: Create Dataset
Dataset <- data.frame(
  ID = 1 : 20,
  Age = c(33, 31, 34, 36, 41, 40, 42, 35, 34, 39, 37, 30, 38, 32, 34, 33, 35, 37, 39, 40),
  Income = c(55, 54, 50, 57, 60, 72, 48, 60, 54, 47, 135, 63, 59, 58, 49, 52, 62, 153, 49, 59),
  Savings = c(10, 10, 8, 11, 14, 16, 7, 8, 6, 5, 40, 8, 6, 10, 12, 8, 7, 48, 5, 8)
)

## Step 2: Inspect the data
View(Dataset)
str(Dataset)
colSums(is.na(Dataset))
summary(Dataset)


## Step 3: Boxplot (VISUAL)
png("boxplot_raw.png", width = 800, height = 600)
boxplot(
  Dataset$Income, Dataset$Savings,
  names = c("Income", "Savings"),
  main = "Boxplot of Income and Savings"
)
dev.off()

## Step 4: IQR Method for Income
Q1 <- quantile (Dataset$Income, 0.25)
Q3 <- quantile (Dataset$Income, 0.75)
IQR <- Q3 - Q1

Lower <- Q1 - 1.5 * IQR
Uper <- Q3 + 1.5 * IQR


## IQR method for Savings
Q1_S <- quantile ( Dataset$Savings, 0.25)
Q3_S <- quantile ( Dataset$Savings, 0.75)
IQR_S <- Q3_S - Q1_S

Lower_S <- Q1_S - 1.5 * IQR_S
Uper_S <- Q3_S + 1.5 * IQR_S


## Step 5: Find Outliers
Dataset[Dataset$Income < Lower | Dataset$Income > Uper, ]
Dataset[Dataset$Savings < Lower_S | Dataset$Savings > Uper_S, ] 


## Step 6: Clean the data
Dataset_Clean <- Dataset[
  Dataset$Income >= Lower & Dataset$Income <= Uper &
  Dataset$Savings >= Lower_S & Dataset$Savings <= Uper_S,
]
rownames(Dataset_Clean) <- NULL


## Step 7: Compare
summary(Dataset)
summary(Dataset_Clean)

png("boxplot_clean.png", width = 800, height = 600)
boxplot(
  Dataset_Clean$Income, Dataset_Clean$Savings,
        names = c("Income", "Savings"),
        main = "Clean Data"
)
dev.off
