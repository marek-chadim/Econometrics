library(tableone)
library(Matching)
library(MatchIt)

data(lalonde)
# Q1
xvars = c('age', 'educ', 'race', 'married', 'nodegree',
          're74', 're75')

table1 <- CreateTableOne(vars=xvars, strata='treat', data=lalonde, test=FALSE)
# print the results including the standardized mean differences (SMD)
print("pre matched table 1")
print(table1,smd=TRUE)


# Q2
y_trt <- lalonde$re78[lalonde$treat==1]
y_con <- lalonde$re78[lalonde$treat==0]
print("performing t test ...")
print(mean((y_trt) - mean(y_con)))


# Q3
psmodel <- glm(treat~.-re78,
               family = binomial(), data=lalonde) # binomial informs using logistic regression

print(summary(psmodel))
pscore <-psmodel$fitted.values
summary(pscore)



# Q4
set.seed(931139)
psmatch <- Match(Tr=lalonde$treat, M=1, X=pscore, replace=FALSE)
matched <-lalonde[unlist(psmatch[c("index.treated","index.control")]),]
matchedtab1 <- CreateTableOne(vars=xvars, strata = "treat",
                              data=matched, test=FALSE)
print(matchedtab1, smd=TRUE)

# Q7
y_trt <- matched$re78[matched$treat==1]
y_con <- matched$re78[matched$treat==0]
diffy <- (y_trt) - (y_con)
print(mean(diffy))

print(t.test(diffy))
