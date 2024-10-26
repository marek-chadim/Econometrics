###OBJECTS###

#Creating objects
a<-c(1,2,3,4)
a
b<-1:4 #equivalent to the previous one
a
a<-0.5:3.5
a
a<-c(4,3,2,1)
a
a<-4:1 #equivalent to the previous one
a
b<-seq(1,11,by=2) #creates a sequence of values from 1 to 11 with a step of 2
ls() #calls all existing objects
2*a
a*a
a+1
a+1:2
a+1:3

#Using R as a calculator
log(exp(sqrt(2*(a+3)/7)-1)) #use R as a calculator
min(log(exp(sqrt(2*(a+3)/7)-1)))^2
sum(a)

###FUNCTIONS###

#Example of functions - sampling
die<-1:6 #Die roll possible outcomes
?sample()
sample(die,1) #Roll a die
sample(size=1,x=die) #Specifying arguments
sample(die,6,replace=TRUE) #Roll a die six times

#Example of functions - distributions
pnorm(1.960) #CDF for the given value
pnorm(1.645)
pnorm(1.645,lower.tail=FALSE) #Complementary (inverse) CDF for the given value
qnorm(0.95) #Quantile value for given probability
x<-rnorm(10000,mean=0,sd=1) #Random variable generation (simulation)
mean(x)
sd(x)
help(Distributions)
#?Distributions

###YOUR OWN FUNCTIONS###

#Writing your own function
simulate_model<-function(a,b,n,m_x,sd_x,sd_u){ #simulate a model
  x<-rnorm(n,mean=m_x,sd=sd_x)
  u<-rnorm(n,mean=0,sd=sd_u)
  y<-a+b*x+u
#  plot(x,y)
  return(matrix(c(x,y),ncol=2))
}
model<-simulate_model(1,1,1000,1,0.5,0.2)
#plot(model[,1],model[,2])

###DATA STRUCTURES###

#Types of atomic vectors
y<-c(-1,2,10)
typeof(y)
y<-c(-1L,2L,10L)
typeof(y)
y<-c("IES","rocks")
y<-c('IES','rocks')
typeof(y)
y<-c(T,F,T)
typeof(y)
y<-c(1+1i,2+2i,3+3i)
typeof(y)

#Attributes
y<-1:12
y
dim(y)<-c(4,3) #Matrix
y
dim(y)<-c(2,3,2) #3D Matrix
y

#Matrices
x<-1:9
M<-matrix(x,nrow=3)
M
M<-matrix(x,nrow=3,byrow=TRUE)
M
M[2,2]
M[2:3,1:2]
M[,1]
D<-diag(x) #diagonal matrix
solve(D) #inverse matrix
t(M) #matrix transposition
D[1:3,1:3]*M #element-by-element multiplication
D[1:3,1:3] %*% M #matrix multiplication

#Arrays
x<-1:12
A<-array(x,dim=c(2,2,3))
A
A[1,,]
A[,1,]
A[,,1]

#Factors
gender <- factor(c("male", "female", "female", "male"))
typeof(gender)
attributes(gender)
gender

#Strings
length("IES") #gives the vector length
length(c("IES","is","the","best"))
nchar("IES") #gives the string length
nchar(c("IES","is","the","best"))

paste("IES","is","the","best.") #space is a defaults separator
paste("IES","is","the","best.",sep="-")
paste("IES","is","the","best.",sep="")

soccer<-c("Plzen","Slavia","Sparta")
paste(soccer,"is the best.") #pasting works for vectors as well
paste(soccer,"is the best", collapse=", and ") #returned vectors can be combined using the collapse argument
paste(paste(soccer,"is the best", collapse=", and "),".",sep="") #it can all be combined

substr("Statistics", 1, 4) #extract first 4 characters
substr("Statistics", 7, 10) #extract last 4 characters
substr(soccer,1,4) #works for vectors as well

cities<-c("New York, NY", "Los Angeles, CA", "Peoria, IL")
substr(cities, nchar(cities)-1, nchar(cities)) #arguments can be functions

strsplit("IES is the best.", " ") #splitting the string according to a delimiter

string<-"We should study hard. We can get some sleep."
sub("We","They",string) #replace the first one
gsub("We","They",string) #replace all
sub(" and SAS", "", "For really tough problems, you need R and SAS.") #remove a substring altogether

#highways<-c("D0","D1","D2","D3")
#outer(highways,highways,paste,sep="-") #outer function gives an outer product but can be specified with practically any function

#Dates
Sys.Date()
class(Sys.Date())

as.Date(Sys.Date())
as.POSIXct(Sys.Date())
as.POSIXlt(Sys.Date())

as.Date("2010-12-31") #correct data format (ISO 8601), i.e. yyyy-mm-dd
as.Date("12/31/2010") #does not work
as.Date("12/31/2010", format="%m/%d/%Y") #we need to specify the format; %Y indicates yyyy, %y indicates yy

as.character(Sys.Date()) #convert date into a string
as.character(Sys.Date(),format="%m/%d/%Y") #format can be forced
as.character(Sys.Date(),format="%m/%d/%y")

ISOdate(2012,2,29)
as.Date(ISOdate(2012,2,29)) #converting day,month, year into a date
ISOdate(2013,2,29) #not everything works - leap year

years<-c(2010,2011,2012,2013,2014)
months<-1
days<-c(15,21,20,18,17)
ISOdate(years, months, days) #works with vectors as well
as.Date(ISOdate(years, months, days))

hours<-5
minutes<-10
seconds<-59
ISOdatetime(years,months,days,hours,minutes,seconds)

d<-as.Date("2010-03-15") #how to extract days, months, etc.
p<-as.POSIXlt(d) #convert to POSIXlt object
p$mday
p$mon+1
p$year+1900 #years since 1900

s<-as.Date("2012-01-01")
e<-as.Date("2012-02-01")
seq(from=s,to=e,by=1) #creating a sequence of dates
seq(from=s,by=1,length.out=7) #an alternative by setting the sequence length
seq(from=s,by="month",length.out=12) #first of the month for one year
seq(from=s,by="3 months",length.out=4) #quarterly dates for one year
seq(from=s,by="year",length.out=10) #year-start dates for one decade
seq(as.Date("2010-01-29"),by="month",len=3) #mind the leap years and ends of months

#Lists
?list()
list1 <- list(100:130, "R", list(TRUE, FALSE))
list1
list1[[1]][2]
list1[[2]]

#Data frames
deck <- data.frame(
  face = c("king", "queen", "jack", "ten", "nine", "eight", "seven", "six",
           "five", "four", "three", "two", "ace", "king", "queen", "jack", "ten",
           "nine", "eight", "seven", "six", "five", "four", "three", "two", "ace",
           "king", "queen", "jack", "ten", "nine", "eight", "seven", "six", "five",
           "four", "three", "two", "ace", "king", "queen", "jack", "ten", "nine",
           "eight", "seven", "six", "five", "four", "three", "two", "ace"),
  suit = c("spades", "spades", "spades", "spades", "spades", "spades",
           "spades", "spades", "spades", "spades", "spades", "spades", "spades",
           "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs", "clubs",
           "clubs", "clubs", "clubs", "clubs", "clubs", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "diamonds",
           "diamonds", "diamonds", "diamonds", "diamonds", "diamonds", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts", "hearts", "hearts",
           "hearts", "hearts", "hearts", "hearts", "hearts"),
  value=c(13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11,10,9,8,7,6,5,4,3,2,1,13,12,11, 10,9,8,7,6,5,4,3,2,1)
)
deck
attributes(deck)

####SELECTING VALUES / INDEXING###

#Positive integers
head(deck) #Returns the first few parts of the object
tail(deck) #Returns the last few parts of the object
tail(deck,10)
deck[1,1]
deck[1,c(1, 2, 3)]
deck[c(1,1),c(1,2,3)]

deck[1:2,1:2] #Returns a data frame
deck[1:2,1] #Returns a vector
deck[1:2,1,drop=FALSE] #Returns a data frame
class(deck[1:2,1])

#Negative integers
deck[-(2:52),1:3] 
deck[c(-5, 1),1] #Error

#Zero
deck[1,0]

#Blank spaces
deck[,1,drop=F]

#Logicals
deck[10,c(TRUE,TRUE,FALSE)]
rows<-c(TRUE,
        F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,
        F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,F,
        F,F,F,F,F,F,F,F,F,F,F,F,F)
deck[rows, ]

#Names
names(deck)
deck[5,c("face","value")]
deck[,"value"]

#Dollar notation
deck$value
mean(deck$value)
list1<-list(numbers=c(1,2),logical=TRUE,strings=c("a","b","c"))
list1
class(list1[1]) #This returns a list
class(list1[[1]]) #This returns a vector
sum(list1[1]) #Error
sum(list1[[1]])
list1["numbers"]
list1[["numbers"]]
list1$numbers
sum(list1$numbers)

#Dollar notation for functions
#x<-rnorm(1000,mean=0,sd=1)
#histogram<-hist(x,plot=FALSE)
#summary(histogram)
#help(hist)
#histogram$breaks
#histogram$counts
#plot(histogram)

#Probabilities and distributions

#runif(1,min=-3,max=3) #generate a random draw from the uniform distribution with preset parameters
#rnorm(1) #standard normal
#rnorm(1,mean=100,sd=15) #normal
#
#rnorm(5,mean=0,sd=10) #generate more numbers with fixed parameters
#rnorm(5,mean=1:5,sd=1) #generate more numbers with varying parameters
#
#set.seed(165) #fixing the random generator, needs an integer as an argument
#runif(10)
#set.seed(165)
##set.seed(166)
#runif(10)
#
#sample(1:5,10,replace=TRUE) #sampling ten numbers between 1 and 5, replace needs to be TRUE to have replacement
#sample(1:5,5) #for permutation, i.e. without replacement (default)
#
#sample(c("heads","tails"),10,replace=TRUE) #flipping coin ten times
#unfairCoin<-sample(c("heads","tails"),10,replace=TRUE,prob=c(0.3,0.7)) #unfair coin
#unfairCoin
#c(sum(unfairCoin=="heads"),sum(unfairCoin=="tails")) #try for 10 and 100

###SOME USEFEUL FUNCTIONS###

#if and else
#x<-0
x<-runif(1,-10,10)
if (x>0) {print("Positive number")}

if (x>0) {
  print("Positive number")
  print("+")
} else {
  print("Non-positive number")
  print("-")
}

if (x>0) {
  #print("Positive number")
  #paste("Positive number - ",x)
  print(paste("Positive number -",round(x,2)),quote=FALSE)
} else if (x<0) {
  #print("Negative number")
  #paste("Negative number - ",x)
  print(paste("Negative number -",round(x,2)),quote=FALSE)
} else {
  print("Zero")
}

#Changing values
vec<-c(0,0,0,0,0,0)
vec
vec[1]<-1000
vec
vec[c(1,3,5)]<-c(1,1,1)
vec
vec[4:6]<-vec[4:6]+1
vec
vec[10]<-0 #Adds an element to the vector

deck2<-deck
deck2$new<-1:52 #Adds a new variable to the data frame
head(deck2)
deck2$new<-NULL #And removes it (only for a list or a data frame)
head(deck2)

#Logical subsetting
deck2$face
deck2$face=="ace"
sum(deck2$face=="ace") #TRUE is treated as 1 and FALSE as 0
deck2$value[deck2$face=="ace"]
deck2$value[deck2$face=="ace"]<-100
deck2

#Multiple conditions
deck3<-deck
deck3$face=="queen" & deck3$suit=="spades"
queenOfSpades<-deck3$face=="queen" & deck3$suit=="spades"
queenOfSpades
deck3$value[queenOfSpades]<-111
head(deck3)

# %in% syntax
deck4<-deck
facecard<-deck4$face %in% c("king", "queen", "jack")
facecard
#Equivalently using multiple OR conditions
facecard<-deck4$face=="king" | deck4$face=="queen" | deck4$face=="jack"
deck4[facecard,]
deck4$value[facecard]<-888
head(deck4,13)

#Missing values
c(NA,1:50)
mean(c(NA,1:50))
mean(c(NA,1:50),na.rm=TRUE)
is.na(c(NA,1:50))

#Splitting
class(split(deck$face,deck$suit))
class(unstack(data.frame(deck$face,deck$suit)))

#LOOPS

#replicate "loop"
replicate(100,mean(simulate_model(1,2,1000,0,0.5,0.2)[,2])) #do not plot!
#hist(replicate(100,mean(simulate_model(1,2,1000,0,0.5,0.2)[,2])))

#for loop
for (i in c(1,10,22,100)){
  if(abs(10-i)<10){
    print(i)
  } else {
    print("Condition not met")
  }
}

for (i in seq(1,31,by=2)){
  if(abs(10-i)<10){
    print(i)
  } else {
    print("Condition not met")
  }
}

for (i in c("This", "is", "a", "for", "loop")) {
  print(i)
}

no_rep<-1000
obs<-1000
mu<-1
sd<-1
sims<-rep(NA,no_rep) #E.g. in simulations, rep() functions is ideal for creating an "empty" atomic vector to write into later
for (i in 1:no_rep) { #Is average a good estimator of the mean value?
  sims[i]<-mean(rnorm(obs,mean=mu,sd=sd))
}
mean(sims)
sd(sims)

no_rep<-1000
mu<-1
sd<-1
obs_min<-100
obs_max<-5000
obs_step<-100
SDs<-matrix(c(seq(obs_min,obs_max,by=obs_step),
              NA*seq(obs_min,obs_max,by=obs_step)),ncol=2)
counter_obs<-1
for(i in seq(obs_min,obs_max,by=obs_step)){ #Does SD decrease with increasing number of observations?
  sims<-rep(NA,no_rep)
  for (j in 1:no_rep) { #two loops two counters (i and j)!
    sims[j]<-mean(rnorm(i,mean=mu,sd=sd))
  }
  #mean(sims)
  SDs[counter_obs,2]<-sd(sims)  
  counter_obs<-counter_obs+1
}
SDs
plot(SDs[,1],SDs[,2])
plot(log(SDs[,1]),log(SDs[,2]))
#lm(log(SDs[,2])~log(SDs[,1]))


#while loop
run<-0
money<-10
while (money > 0 && run < 100000) {
  money <- money - 0 + sample(c(-2,-1,0,1.5,2.5),1)
  run <- run +1
}
run

#repeat loop
run<-0
money<-10
repeat {
  money <- money - 0 + sample(c(-2,-1,0,1.5,2.5),1)
  run <- run +1
  if (money <= 0 || run > 1000) {
    break
  }
}
run

#APPLY() & VARIANTS# Load the benchmarkme package
library(benchmarkme)

install.packages("benchmarkme")# Assign the variable ram to the amount of RAM on this machine
ram <- get_ram()
ram


# Assign the variable cpu to the cpu specs
cpu <- get_cpu()
cpu

mat<-matrix(rnorm(10000,mean=1:10),ncol=10,byrow=TRUE) #matrix drawn from the Gaussian with a mean of 1-10 for given 10 columns
apply(mat,2,mean) #find mean for each column
apply(mat,2,sd) #find SD for each column
apply(mat,2,range) #find range for each column, returns a matrix
apply(mat,2,function(x) range(x)-mean(x)) #for more complicated functions
apply(mat,2,function(x) range-mean) #does not work!

tapply(deck$face,deck$suit,function(x) sample(x,1)) #apply by a grouping factor
by(deck,deck$suit,is.na) #apply to each row by grouping factor

###FASTER CODE###

#Vectorized code

abs_loop<-function(vec){ #return a vector of absolute values via for loop
  for (i in 1:length(vec)){
    if(vec[i]<0) {
      vec[i] <- -vec[i]
    } 
  }
  vec
}

abs_sets<-function(vec){ #return a vector of absolute values, vectorized code
  negs <- vec<0
  vec[negs]<-vec[negs]*-1
  vec
}

long<-rep(c(-1, 1),100000000) #1 mil. of -1, 1, -1, 1,...

system.time(abs_loop(long)) #compare execution (elapsed) time
system.time(abs_sets(long)) #compare execution (elapsed) time

#Fast vs. slow loop

fast_loop<-function(n){
  output<-rep(NA,n)
  for(i in 1:n){
    output[i]<-i+1
  }
}

slow_loop<-function(n){
  output<-NA
  for (i in 1:n){
    output[i]<-i+1
  }
}

n<-10000000
system.time(fast_loop(n))
system.time(slow_loop(n))
system.time(slow_loop(n))[3]/system.time(fast_loop(n))[3]
