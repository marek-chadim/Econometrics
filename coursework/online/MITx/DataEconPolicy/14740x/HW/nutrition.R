# Set workspace
setwd("data")

# Read in the data
data <- read.csv("nutrition_csv.csv")
colnames(data)

# Number of obs per pupil
length(data$pupid)
length(unique(data$pupid))

# Percent male
mean(data$sex, na.rm=TRUE)

# Percent assigned to treatment, percent took pill in 1998
vars <- c("treat_sch98", "pill98")
summary(data[,vars],na.rm=TRUE)

# Difference in outcomes, took pill versus didn't take pill (method 1)
diff1 <- lm(totpar98~pill98,data)
summary(diff1)

# Difference in outcomes, took pill versus didn't take pill (method 2)
summary(data[data$pill98==0,"totpar98"],na.rm=TRUE)
summary(data[data$pill98==1,"totpar98"],na.rm=TRUE)

# Difference in outcomes, treatment versus nontreatment (method 1)
diff2 <- lm(totpar98~treat_sch98,data)
summary(diff2)

# Difference in outcomes, treatment versus nontreatment (method 2)
summary(data[data$treat_sch98==0,"totpar98"],na.rm=TRUE)
summary(data[data$treat_sch98==1,"totpar98"],na.rm=TRUE)

# Probability of taking pill (method 1)
diff3 <- lm(pill98~treat_sch98,data)
summary(diff3) 

# Probability of taking pill (method 2)
summary(data[data$treat_sch98==0,"pill98"],na.rm=TRUE)
summary(data[data$treat_sch98==1,"pill98"],na.rm=TRUE)

