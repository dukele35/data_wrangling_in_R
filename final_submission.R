####### PLEASE REFER THOSE LINES OF CODES TO THE REPORT FILE THAT EXPLAINS CLEARLY STEP-BY-STEP HOW THE CODE WORKS ###### 
install.packages("tidyverse")
install.packages("readxl")
install.packages('lubridate')

# load required packages
library(tidyverse)
library(readxl)
library(summarytools)
library(tidyr)

# list the excel file's sheets 
excel_sheets('assignment1.xlsx')

# read different sheets of the excel file
df1 <- read_excel('assignment1.xlsx', sheet = 'Sheet1')
df2 <- read_excel('assignment1.xlsx', sheet = 'Sheet2')

# standardise the dataframe
df1 <- as.data.frame(df1)
df2 <- as.data.frame(df2)

View(df1)
View(df2)

str(df1)


# Change columns' names to follow the Naming Convention in R, i.e. there should be no whitespace and other special characters like ()
colnames(df1)
colnames(df2)
names(df1)[names(df1) == 'Education Level'] <- 'EducationLevel'
names(df1)[names(df1) == 'Height(m)'] <- 'Height'
names(df1)[names(df1) == 'Weight(kg)'] <- 'Weight'
names(df1)[names(df1) == 'Surname'] <- 'SurName'
names(df2)[names(df2) == 'Surname'] <- 'SurName'


# removing white spaces in names in df1
df1$FirstName <- str_trim(df1$FirstName, side = c("both", "left", "right"))
df1$SurName <- str_trim(df1$SurName, side = c("both", "left", "right"))

# Removing "’" from Surname column in df1
# install.packages('Hmisc')
library(Hmisc)
df1$SurName <- str_remove(df1$SurName, "’")
df1$SurName <- tolower(df1$SurName)
df1$SurName <- capitalize(df1$SurName)


# checking the number of unique names in df1
nrow(df1)
length(unique(df1$FirstName))
length(unique(df1$SurName))
length(unique(paste(df1$FirstName, df1$Surname)))


# names_genders
fst_df <- read_csv('name_gender.csv')
View(fst_df)
fst_df$name

# Creating gender column
for(i in 1:nrow(df1)){
  for(j in 1:nrow(fst_df)){
    if(df1$FirstName[i] == fst_df$name[j]){
      df1$Gender[i] <- fst_df$gender[j]
    }
  }
}


# factorise gender
df1$Gender <- as.factor(df1$Gender)
table(df1$Gender)
View(df1)


# Inspecting surname's races
sur_df <- read_csv('surnames_races.csv')
View(sur_df)

for(i in 1:nrow(df1)){
  for(j in 1:nrow(sur_df)){
    if(df1$SurName[i] == sur_df$name[j]){
      m = max(sur_df$pctwhite[j], 
              sur_df$pctblack[j], 
              sur_df$pctapi[j], 
              sur_df$pctaian[j], 
              sur_df$pct2prace[j], 
              sur_df$pcthispanic[j])
      if(sur_df$pctwhite[j] == m){
        df1$Race[i] <- 'White'
      }else if(sur_df$pctblack[j] == m){
        df1$Race[i] <- 'Black'
      }else if(sur_df$pctapi[j] == m){
        df1$Race[i] <- 'Asian and Pacific Islander'
      }else if(sur_df$pctaian[j] == m){
        df1$Race[i] <- 'American Indian and Alaskan Native'
      }else if(sur_df$pct2prace[j] == m){
        df1$Race[i] <- 'Two or More Races'
      }else if(sur_df$pcthispanic[j] == m){
        df1$Race[i] <- 'Hispanic'
      }else{
        df1$Race[i] <- NA
      }
    }
  }
}
View(df1)



# Create AgeRange column
descr(df1$Age)

df1 <- df1 %>%
  mutate(AgeRange = case_when(
    between(Age, 0,    17.9) ~ 1,
    between(Age, 18.0, 34.9) ~ 2,
    between(Age, 35.0, 54.9) ~ 3,
    between(Age, 55.0, 74.9) ~ 4,
    between(Age, 75.0,  100) ~ 5,
  ))

# factorise age range
df1$AgeRange <- as.factor(df1$AgeRange)
levels(df1$AgeRange)


# DoB & IDNumber
length(unique(df1$DoB))
length(unique(df1$IDNumber))



# Change 'FALSE' and factorise EducationLevel
df1$EducationLevel[df1$EducationLevel == 'Primary'] <- 'primary'
df1$EducationLevel[df1$EducationLevel == 'FALSE'] <- 'unknown'
df1$EducationLevel <- as.factor(df1$EducationLevel)
str(df1)
table(df1$EducationLevel)
barplot(table(df1$EducationLevel))



# Factorise CriminalRecord column 
df1$CriminalRecord <- as.factor(df1$CriminalRecord)
table(df1$CriminalRecord)
barplot(table(df1$CriminalRecord))
levels(df1$CriminalRecord)
is.factor(df1$CriminalRecord)


# Question 3,4: creating ids
df1 <- df1 %>% 
  mutate(ID = group_indices(., IDNumber))
View(df1[,c('IDNumber', 'FirstName', 'SurName', 'ID')])

df1$ID <- paste0(as.character(df1$ID), 
                 substr(df1$FirstName,0,1), 
                 substr(df1$SurName,0,1))
View(df1[,c('IDNumber', 'FirstName', 'SurName', 'ID')])

for(i in 1:nrow(df1)){
  if(nchar(df1$ID[i]) == 3){
    df1$ID[i] <- paste0('R000',df1$ID[i])
  }else if(nchar(df1$ID[i]) == 4){
    df1$ID[i] <- paste0('R00',df1$ID[i])
  }else if(nchar(df1$ID[i]) == 5){
    df1$ID[i] <- paste0('R0',df1$ID[i])
  }
}
View(df1[,c('IDNumber', 'FirstName', 'SurName', 'ID')])


# checking the uniquess of IDNumber in df2
length(unique(filter(df2, !is.na(df2$IDNumber))$IDNumber))
nrow(filter(df2, is.na(df2$IDNumber)))


# removing white spaces in names in df2
df2$FirstName <- str_trim(df2$FirstName, side = c("both", "left", "right"))
df2$SurName <- str_trim(df2$SurName, side = c("both", "left", "right"))

# Removing "’" from Surname column in df2
df2$SurName <- str_remove(df2$SurName, "’")
df2$SurName <- capitalize(tolower(df2$SurName))


# checking the number of unique names in df2
nrow(df2)
length(unique(df2$FirstName))
length(unique(df2$SurName))
length(unique(paste0(df2$FirstName, df2$Surname)))
a <- filter(df2, is.na(df2$IDNumber))
length(unique(paste0(a$FirstName, a$Surname)))


# checking the set --> whether there are any union, difference or intersect between df1 & df2
setdiff(df1$IDNumber, df2$IDNumber)
length(setdiff(df1$IDNumber, df2$IDNumber))
setdiff(df2$IDNumber, df1$IDNumber)


df1.1 <- df1[c('FirstName','SurName', 'IDNumber')]
df2.1 <- df2[c('FirstName','SurName', 'IDNumber')]

setdiff(df2.1, df1.1)

id_na_with_names <- setdiff(df2.1, df1.1)
id_na_with_names
id_na_with_names[[1]]
id_na_with_names[[2]]

# Question 6
df2$IDNumber[df2$FirstName %in% id_na_with_names[[1]] & df2$SurName %in% id_na_with_names[[2]]] <- df1$IDNumber[df1$FirstName %in% id_na_with_names[[1]] & df1$SurName %in% id_na_with_names[[2]]]

# Question 5
df3 <- df2[c('IDNumber', 'Health')]
View(df3)
df1 <- full_join(df1, df3, by = 'IDNumber')

View(df1)

# factorise health
df1$Health <- as.factor(df1$Health)
str(df1)


##### Questions 8,9,10 #####
par(mfcol = c(2,2))
# 4.1.1. Univariate analysis 
table(df1$CriminalRecord)
prop.table(table(df1$CriminalRecord))
ggplot(df1, aes(x=CriminalRecord)) +
  geom_bar(fill = 'tan2', width = 0.5) +
  labs(title = 'Criminal Record Bar Plot') 


# 4.1.2. Bivariate analysis 
# a. Criminal with other categorical variables
# Criminal vs Gender
table(df1$CriminalRecord, df1$Gender)
prop.table(table(df1$Gender, df1$CriminalRecord), 2)
g1 <- ggplot(df1, aes(x=CriminalRecord, fill=Gender)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_fill_manual(values = c("tan2", "tan4")) +
  labs(title = '1.Criminal Record Bar Plot by Genders') 


# Criminal Record vs Educational Level 
table(df1$EducationLevel, df1$CriminalRecord)
prop.table(table(df1$EducationLevel, df1$CriminalRecord, useNA = 'always'), margin = 2)
g2 <- ggplot(df1, aes(x=CriminalRecord, fill=EducationLevel)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("peachpuff3", "tan1", 'tan2', 'tan3', 'tan4', 'chocolate4')) +
  labs(title = '2.Criminal Record Bar Plot by Education Levels') 


# Criminal vs AgeRange
table(df1$AgeRange, df1$CriminalRecord)
g3 <- ggplot(df1, aes(x=CriminalRecord, fill=AgeRange)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("peachpuff3", "tan1", 'tan2', 'tan3', 'tan4')) +
  labs(title = '3.Criminal Record Bar Plot by Age Ranges') 

# Health vs Criminal
table(df1$Health, df1$CriminalRecord, useNA = 'always')
prop.table(table(df1$CriminalRecord, df1$Health, useNA = 'always'), margin = 2)
g4 <- ggplot(df1, aes(x=CriminalRecord, fill=Health)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = '4.Criminal Record Bar Plot by Health Status') 


library(ggpubr)
ggarrange(g1, g2, g3, g4,
          ncol = 2, nrow = 2)


# b. Criminal with other numeric variables
# Criminal vs Height
y1 <- ggplot(df1, aes(x=Height)) +
  geom_histogram(color ='tan4', fill = 'peachpuff2') +
  facet_wrap(~CriminalRecord) +
  labs(title = '1.Height Histogram grouped by Criminal Record', x = 'Height(m)') 
y2 <- ggplot(df1, aes(x=CriminalRecord, y=Height)) +
  geom_boxplot(fill = c("tan2","tan4"), alpha = 0.8)  +
  labs(title = '2.Height Boxplot grouped by Criminal Record', y = 'Height(m)') 
y2
y3 <- ggplot(df1, aes(x=Height, fill=CriminalRecord)) +
  geom_density(alpha = .7) + 
  scale_fill_manual( values = c("tan2","tan4")) +
  labs(title = '3.Height Density plot grouped by Criminal Record', x = 'Height(m)') 

# CriminalRecord vs Weight 
y4 <- ggplot(df1, aes(x=Weight)) +
  geom_histogram(color ='tan4', fill = 'peachpuff2') +
  facet_wrap(~CriminalRecord) +
  labs(title = '4.Weight Histogram grouped by Criminal Record', x = 'Weight(kg)') 
y5 <- ggplot(df1, aes(x=CriminalRecord, y=Weight)) +
  geom_boxplot(fill = c("tan2","tan4"), alpha = 0.8)  +
  labs(title = '5.Weight Boxplot grouped by Criminal Record', y = 'Weight(kg)') 
y6 <- ggplot(df1, aes(x=Weight, fill=CriminalRecord)) +
  geom_density(alpha = .7) + 
  scale_fill_manual( values = c("tan2","tan4")) +
  labs(title = '6.Weight Density plot grouped by Criminal Record', x = 'Weight(kg)') 

# Salary vs CriminalRecord
y7 <- ggplot(df1, aes(x=Salary)) +
  geom_histogram(color ='tan4', fill = 'peachpuff2') +
  facet_wrap(~CriminalRecord) +
  labs(title = '7.Salary Histogram grouped by Criminal Record') 
y8 <- ggplot(df1, aes(x=CriminalRecord, y=Salary)) +
  geom_boxplot(fill = c("tan2","tan4"), alpha = 0.8) +
  labs(title = '8.Salary Boxplot grouped by Criminal Record') 
y9 <- ggplot(df1, aes(x=Salary, fill=CriminalRecord)) +
  geom_density(alpha = .7) + 
  scale_fill_manual( values = c("tan2","tan4")) +
  labs(title = '9.Salary Density plot grouped by Criminal Record') 


ggarrange(y1, y2, y3, y4, y5, y6, y7, y8, y9,
          ncol = 3, nrow = 3)


# Question 8

crime_age1 <- df1 %>%
  filter(AgeRange == 1)
ca1.1 <- ggplot(crime_age1, aes(x=CriminalRecord, fill=Gender)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_fill_manual(values = c("tan2", "tan4")) +
  labs(title = '1.CriminalRecord Bar Plot by Genders (AgeRange 0-18)') 
ca1.2 <- ggplot(crime_age1, aes(x=CriminalRecord, fill=EducationLevel)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("peachpuff3", "tan1", 'tan2', 'tan3', 'tan4', 'chocolate4')) +
  labs(title = '2.CriminalRecord Bar Plot by EducationLevels (AgeRange 0-18)') 
ca1.3 <- ggplot(crime_age1, aes(x=CriminalRecord, fill=Health)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = '3.CriminalRecord Bar Plot by Health (AgeRange 0-18)') 

crime_age2 <- df1 %>%
  filter(AgeRange == 2)
ca2.1 <- ggplot(crime_age2, aes(x=CriminalRecord, fill=Gender)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_fill_manual(values = c("tan2", "tan4")) +
  labs(title = '4.CriminalRecord Bar Plot by Genders (AgeRange 18-35)') 
ca2.2 <- ggplot(crime_age2, aes(x=CriminalRecord, fill=EducationLevel)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("peachpuff3", "tan1", 'tan2', 'tan3', 'tan4', 'chocolate4')) +
  labs(title = '5.CriminalRecord Bar Plot by EducationLevels (AgeRange 18-35)') 
ca2.3 <- ggplot(crime_age2, aes(x=CriminalRecord, fill=Health)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = '6.CriminalRecord Bar Plot by Health (AgeRange 18-35)') 



crime_age3 <- df1 %>%
  filter(AgeRange == 3)
ca3.1 <- ggplot(crime_age3, aes(x=CriminalRecord, fill=Gender)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_fill_manual(values = c("tan2", "tan4")) +
  labs(title = '7.CriminalRecord Bar Plot by Genders (AgeRange 35-55)') 
ca3.2 <- ggplot(crime_age3, aes(x=CriminalRecord, fill=EducationLevel)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("peachpuff3", "tan1", 'tan2', 'tan3', 'tan4', 'chocolate4')) +
  labs(title = '8.CriminalRecord Bar Plot by EducationLevels (AgeRange 35-55)') 
ca3.3 <- ggplot(crime_age3, aes(x=CriminalRecord, fill=Health)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = '9.CriminalRecord Bar Plot by Health (AgeRange 35-55)') 



crime_age4 <- df1 %>%
  filter(AgeRange == 4)
ca4.1 <- ggplot(crime_age4, aes(x=CriminalRecord, fill=Gender)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_fill_manual(values = c("tan2", "tan4")) +
  labs(title = '10.CriminalRecord Bar Plot by Genders (AgeRange 55-75)') 
ca4.2 <- ggplot(crime_age4, aes(x=CriminalRecord, fill=EducationLevel)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("peachpuff3", "tan1", 'tan2', 'tan3', 'tan4', 'chocolate4')) +
  labs(title = '11.CriminalRecord Bar Plot by EducationLevels (AgeRange 55-75)') 
ca4.3 <- ggplot(crime_age4, aes(x=CriminalRecord, fill=Health)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = '12.CriminalRecord Bar Plot by Health (AgeRange 55-75)') 



crime_age5 <- df1 %>%
  filter(AgeRange == 5)
ca5.1 <- ggplot(crime_age5, aes(x=CriminalRecord, fill=Gender)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_fill_manual(values = c("tan2", "tan4")) +
  labs(title = '13.CriminalRecord Bar Plot by Genders (Age > 75)') 
ca5.2 <- ggplot(crime_age5, aes(x=CriminalRecord, fill=EducationLevel)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("peachpuff3", "tan1", 'tan2', 'tan3', 'tan4', 'chocolate4')) +
  labs(title = '14.CriminalRecord Bar Plot by EducationLevels (Age > 75)') 
ca5.3 <- ggplot(crime_age5, aes(x=CriminalRecord, fill=Health)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = '15.CriminalRecord Bar Plot by Health (Age > 75)') 



ggarrange(ca1.1, ca1.2, ca1.3,
          ca2.1, ca2.2, ca2.3,
          ca3.1, ca3.2, ca3.3,
          ca4.1, ca4.2, ca4.3,
          ca5.1, ca5.2, ca5.3,
          ncol = 3, nrow = 5)



# 4.1.3. Multivariate analysis

ggplot(df1, aes(x = Weight, y = Height, col=CriminalRecord, size = Age)) +
  geom_point() +
  labs(title = '1.Height and Weight Scatterplot grouped by CriminalRecord', 
       x = 'Weight(kg)', 
       y = 'Height(m)') +
  scale_color_manual(values = c("tan2", "tan4")) +
  theme_light()
colors <- c('tan2', 'tan4')
colors <- colors[as.numeric(df1$CriminalRecord)]
scatterplot3d(df1[,c('Height', 'Weight', 'Age')], pch = 16, color = colors)
legend("top", 
       legend = c('CriminalRecord 1', 'CriminalRecord 2'),
       col =  c('tan2', 'tan4'), 
       pch = 16,
       inset = -0.25, xpd = TRUE, horiz = TRUE)



ggplot(df1, aes(x = Weight, y = Height, col=CriminalRecord, size = Salary)) +
  geom_point() +
  labs(title = '1.Height and Weight Scatterplot grouped by CriminalRecord', 
       x = 'Weight(kg)', 
       y = 'Height(m)') +
  scale_color_manual(values = c("tan2", "tan4")) +
  theme_light()
colors <- c('tan2', 'tan4')
colors <- colors[as.numeric(df1$CriminalRecord)]
scatterplot3d(df1[,c('Height', 'Weight', 'Salary')], pch = 16, color = colors)
legend("top", 
       legend = c('CriminalRecord 1', 'CriminalRecord 2'),
       col =  c('tan2', 'tan4'), 
       pch = 16,
       inset = -0.25, xpd = TRUE, horiz = TRUE)




df1.salary1 <- filter(df1, Salary < 15000)
df1.salary2 <- filter(df1, Salary > 15000)

# Question 10 - Salary < 15000
ggplot(df1.salary1, aes(x = Weight, y = Height, col=CriminalRecord, size = Salary)) +
  geom_point() +
  labs(title = '1.Height and Weight Scatterplot grouped by CriminalRecord', 
       x = 'Weight(kg)', 
       y = 'Height(m)') +
  scale_color_manual(values = c("tan2", "tan4")) +
  theme_light()
colors <- c('tan2', 'tan4')
colors <- colors[as.numeric(df1.salary1$CriminalRecord)]
scatterplot3d(df1.salary1[,c('Height', 'Weight', 'Salary')], pch = 16, color = colors)
legend("top", 
       legend = c('CriminalRecord 1', 'CriminalRecord 2'),
       col =  c('tan2', 'tan4'), 
       pch = 16,
       inset = -0.25, xpd = TRUE, horiz = TRUE)

# Question 10 - Salary > 15000
ggplot(df1.salary2, aes(x = Weight, y = Height, col=CriminalRecord, size = Salary)) +
  geom_point() +
  labs(title = '1.Height and Weight Scatterplot grouped by CriminalRecord', 
       x = 'Weight(kg)', 
       y = 'Height(m)') +
  scale_color_manual(values = c("tan4", "tan2")) +
  theme_light()
colors <- c('tan2', 'tan4')
colors <- colors[as.numeric(df1.salary2$CriminalRecord)]
scatterplot3d(df1.salary2[,c('Height', 'Weight', 'Salary')], pch = 16, color = colors)
legend("top", 
       legend = c('CriminalRecord 1', 'CriminalRecord 2'),
       col =  c('tan2', 'tan4'), 
       pch = 16,
       inset = -0.25, xpd = TRUE, horiz = TRUE)




# 4.2. Health analysis
# 4.2.1. Univariate analysis
df_health <- df1[,c('Age', 'Height', 'Weight', 'EducationLevel', 
                    'Salary', 'CriminalRecord', 'Gender', 'AgeRange', 'Health')]


View(df_health)

table(df_health$Health, useNA = 'always')
prop.table(table(df_health$Health))
ggplot(df_health, aes(x=Health)) +
  geom_bar(fill = 'olivedrab4', width = 0.5) +
  labs(title = 'Health Bar Plot') +
  theme_light()

# 4.2.2. Bivariate analysis 
# a. Health with other categorical variables
# Criminal vs Gender
table(df_health$Health, df1$Gender)
prop.table(table(df_health$Gender, df_health$Health), 2)
g1 <- ggplot(df_health, aes(x=Health, fill=Gender)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) + 
  scale_fill_manual(values = c("olivedrab2", "olivedrab4")) +
  labs(title = '1.Health Bar Plot by Genders') 

# Health vs Educational Level 
g2 <- ggplot(df1, aes(x=Health, fill=EducationLevel)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("darkgreen", "forestgreen", 'olivedrab1', 'olivedrab3', 'olivedrab4', 'darkgrey')) +
  labs(title = '2.Heath Bar Plot by Education Levels') 

# Health vs AgeRange
g3 <- ggplot(df1, aes(x=Health, fill=AgeRange)) +
  geom_bar(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 0)) +
  scale_fill_manual(values = c("darkgreen", "forestgreen", 'olivedrab1', 'olivedrab4', 'darkgrey')) +
  labs(title = '3.Health Bar Plot by Age Ranges') 

# Health vs Criminal
g4 <- ggplot(df1, aes(x=Health, fill=CriminalRecord)) +
  geom_bar(position = 'dodge', width = 0.6) +
  theme(axis.text.x = element_text(angle = 0)) +
  labs(title = '4.Health Bar Plot by CriminalRecord') 

ggarrange(g1, g2, g3, g4,
          ncol = 2, nrow = 2)


# b. Health with other numeric variables
# Health vs Height
y1 <- ggplot(df1, aes(x=Height)) +
  geom_histogram(color ='olivedrab4', fill = 'olivedrab1') +
  facet_wrap(~Health) +
  labs(title = '1.Height Histogram grouped by Health', x = 'Height(m)') 
y2 <- ggplot(df1, aes(x=Health, y=Height)) +
  geom_boxplot(fill = c('olivedrab1',"olivedrab4","grey"), alpha = 0.8)  +
  labs(title = '2.Height Boxplot grouped by Health', y = 'Height(m)') 
y3 <- ggplot(df1, aes(x=Height, fill=Health)) +
  geom_density(alpha = .7) + 
  scale_fill_manual( values = c('olivedrab1',"olivedrab4","grey")) +
  labs(title = '3.Height Density plot grouped by Health', x = 'Height(m)') 
y1
y2
y3
# Health vs Weight 
y4 <- ggplot(df1, aes(x=Weight)) +
  geom_histogram(color ='olivedrab4', fill = 'olivedrab1') +
  facet_wrap(~Health) +
  labs(title = '4.Weight Histogram grouped by Health', x = 'Weight(kg)') 
y5 <- ggplot(df1, aes(x=Health, y=Weight)) +
  geom_boxplot(fill = c('olivedrab1',"olivedrab4","grey"), alpha = 0.8)  +
  labs(title = '5.Weight Boxplot grouped by Health', y = 'Weight(kg)') 
y6 <- ggplot(df1, aes(x=Weight, fill=Health)) +
  geom_density(alpha = .7) + 
  scale_fill_manual( values = c('olivedrab1',"olivedrab4","grey")) +
  labs(title = '6.Weight Density plot grouped by Health', x = 'Weight(kg)') 
y4
y5
y6
# Salary vs Health
y7 <- ggplot(df1, aes(x=Salary)) +
  geom_histogram(color ='olivedrab4', fill = 'olivedrab1') +
  facet_wrap(~Health) +
  labs(title = '7.Salary Histogram grouped by Health') 
y8 <- ggplot(df1, aes(x=Health, y=Salary)) +
  geom_boxplot(fill = c('olivedrab1',"olivedrab4","grey"), alpha = 0.8) +
  labs(title = '8.Salary Boxplot grouped by Health') 
y9 <- ggplot(df1, aes(x=Salary, fill=Health)) +
  geom_density(alpha = .7) + 
  scale_fill_manual( values = c('olivedrab1',"olivedrab4","grey")) +
  labs(title = '9.Salary Density plot grouped by Health')
y7
y8
y9


ggarrange(y1, y2, y3, y4, y5, y6, y7, y8, y9,
          ncol = 3, nrow = 3)




# 4.2.3. Multivariate analysis

ggplot(df1, aes(x = Weight, y = Height, col=Health, size = Age)) +
  geom_point() +
  labs(title = '1.Height and Weight Scatterplot grouped by Health', 
       x = 'Weight(kg)', 
       y = 'Height(m)') +
  scale_color_manual(values = c("olivedrab2", "olivedrab4")) +
  theme_light()
colors <- c("olivedrab2", "olivedrab4")
colors <- colors[as.numeric(df1$Health)]
scatterplot3d(df1[,c('Weight', 'Height', 'Age')], pch = 16, color = colors)
legend("top", 
       legend = c('Health 1', 'Health 2'),
       col =  c("olivedrab2", "olivedrab4"), 
       pch = 16,
       inset = -0.25, xpd = TRUE, horiz = TRUE)

xyplot(Height ~ Weight | AgeRange, 
       df1, 
       groups = Health,           
       grid = TRUE,
       col = c("olivedrab2", "olivedrab4"),
       pch = 19,
       auto.key = list(space = 'right', title = 'Health'))


###### 10. regresion models #####
library(caTools)
set.seed(1234)
View(df_health)
df_health
split <- sample.split(df_health, SplitRatio = 0.8)
training <- subset(df_health, split == 'TRUE')
testing <- subset(df_health, split == 'FALSE')

model <- glm(Health ~ Age, training, family = 'binomial')
summary(model)

pro_pred <- predict(model, testing[-9], type = 'response')
health_predict <- ifelse(pro_pred > 0.5, 2, 1)
cm = table(testing[,9], health_predict )
cm
