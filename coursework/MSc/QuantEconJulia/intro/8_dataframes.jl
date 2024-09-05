
#########################
# this is a jl file.
# run it line-by-line by shift+enter

####################
# set working directory
cd("your/path/to/files/you/want/to/use") # write your path there
pwd() # tells you working directory. this is a function that does not need inputs

##############################
# how to run external script
include("call_this_script.jl") # runs everything in 'call_this_script.jl'
# this assumes that this file is directly in your working folder. Otherwise, you can give the full path as a string as an input of include

myfunction(4) # everything in 'call_this_script.jl' is available here too after running include


##############################
# help
##############################

# help is available if you write ?functionyouareinterestedin in the REPL (i.e. the terminal below) and press enter.
# btw, when typing ?, the prompt 'julia>' should turn into help?>

#############################
#############################
#        DataFrames         #
#############################
#############################

# when working with data in julia, use DataFrames

# guide is here: https://dataframes.juliadata.org/stable/man/getting_started/
# this is a large and sophisticated package, in this course we need a tiny fraction of its functionalities!

using DataFrames
using CSV # to read CSV files

# this is how you would create a brand new data frame from your own data
testdf = DataFrame(a=1:4, b=["M", "F", "F", "M"])
println(testdf)
# dataframe behaves a lot like a matrix, but
# - columns have names
# - columns can have different types 

psid = DataFrame(CSV.File("psid.csv")) # read psid file. if fails check if file is in the your working folder ( i.e. the output of pwd() )

println(first(psid,10)) # show first 10 rows

# names of columns is a vector of strings
names(psid)

#######
# select columns
#######

psid.age
# or
psid."age"
# or
psid[!,"age"]
# or 
psid[:,"age"]

# first two options are only useful to get whole columns (cannot select row indices), and only 1 column at a time. first option doesn't work at all when column name contains a space.

# last two options are more flexible. what is the difference between those two? more subtle than I remembered during class:
# both 
# psid[!,"age"].= 0, or
# psid[:,"age"].= 0 would overwrite the 'age' column of psid with 0s.
# However, if you run
# a = psid[!,"age"] and
# b = psid[:,"age"], then 
# 'a' is actually a column of psid (so modiying 'a' modifies psid), while
# 'b' is just a copy (so modiying 'b' does not affect psid).
# TLDR: If you don't have such expressions (i.e. psid[!,"age"] or psid[:,"age"])
# ALONE on the RIGHT hand side of =, the two things do the same.


# note that this is a vector
psid[!,"age"]
# this is a DataFrame
psid[!,["age"]]

# so when we ask for more columns, we also get a DataFrame
psid[!,["age", "age_spouse"]]

# select all columns with a name starting the word 'age'
psid[:, Cols(x -> startswith(x, "age"))]

# select all columns with a name containing the word 'spouse'
psid[:, Cols(x -> contains(x, "spouse"))]

# select all columns except a few that you list
psid[:, Not(["educ", "state"])]

##########
# select rows
##########

# indices
psid[1:4,:] # rows 1st to 4th
psid[[1,4],:] # 1st and 4th row

# logical indices - very useful for selecting rows!
psid[psid.id_ind.==1002,:] # all records for individual with ID 1002

psid[psid.famid_1968.==12 .&& psid.year .> 1970,:] # all records of household 12, after year 1970


using Statistics # I think this is a core Julia package, most likely doesn't need to be installed
# needed for mean, cor and lot of other functions: std, var, cov, ...

#########
# missing
#########

# 'missing' variables are equivalent to NA in R. they ruin all operations:
mean(psid[:,"age"])

# but we can skip missing values with this simple function:
mean(skipmissing(psid[:,"age"])) # temporaly drops missing values and then computes mean

# we are interested in the correlation of age of household head and spouse
# several steps:
# 1. select columns age and age_spouse
# 2. drop all rows including any 'missing's
# 3. turn the resulting DataFrame into an Array (that's what cor needs as an input)
# 4. cor gives you the correlation matrix. we need to call over rows (dim 1), since rows represent the indviduals.
cor(Array(dropmissing(psid[:,["age", "age_spouse"]])); dims = 1)
# the ones in the main diagonal are correlation of the two variables with themselves (they are trivially 1). we were interested in the off-diagonal elements

# read more about missing and dropmissing in the first half of this guide: https://dataframes.juliadata.org/stable/man/missing/

# we can replace 'missing' with any particular value with function 'coalesce'
coalesce.([missing, 2, 5, missing], 13) # replaces 'missing's with 13

# how do logical indexing interact with missing values? tricky

# select afro-americans
unique(psid[:,["race", "race_label"]]) # first run this to see which label corresponds to what. afro-american is 2.
# (unique throws out all duplicate rows)

psid[psid.race .==2,:] # this throws an error, since race is sometimes missing; missing==2 gives missing instead true or false, and missing is not ok as a logical index
# so more slowly, we have an issue, since
psid.race .==2
# contains missings, not only logical values

psid[coalesce.(psid.race .==2,false),:] # this works: first we check if race is 2, and then we replace all missings in the logical vector by false, and then we use it as index 

## combine with and
# this gives afro-americans in 2001
psid[coalesce.(psid.race .==2,false) .&& psid.year.==2001,:] # works, since year is never missing


###########
# sorting
# sort first by individual id, then by year
sort!(psid, ["id_ind", "year"])
println(first(psid[:,1:8],10))

# one can also sort in the opposite direction by some variables
sort!(psid, ["id_ind", order("year", rev = true)]) # id increasing, year decreasing
println(first(psid[:,1:8],10))

#########
# modify dataset

# drop rows/columns by taking a subset of the original dataframe and write it into a new one
psid_old = psid[coalesce.(psid.age .>=65,false),:]

# we could also overwrite the original dataset like this
# psid = psid[coalesce.(psid.age .>=65,false),:]

# rename columns
rename!(psid, "rel_head" => "relation_to_head", "educ" => "education")
names(psid)

# create column
psid[:,"cohort"] .= psid[:,"year"].-psid[:,"age"] # cohort is the year of birth, i.e. current year - age

println(first(psid[psid.id_ind.==1002,["id_ind","year","age","cohort"]],5)) # cohort is not stable for an individual, since age is observed with an error (maybe interview is not done always on same calendar day)

# initialize a new column with missings, such that it is allowed to contain logical or missing
psid[!,"spendsmin1000"] = missings(Bool, size(psid,1))
psid[coalesce.(psid.expenditure_family.>=1000, false),"spendsmin1000"] .= true # set it to true if a family spends more than 1000 dollars. missings should be ignored
psid[coalesce.(psid.expenditure_family.<1000,false),"spendsmin1000"] .= false # set it to false if a family spends less than 1000 dollars. missings should be ignored again!

# if no missings are involved, this simpler way works too:
psid[!,"yearafter1980"] = fill(false,size(psid,1)) # initialize with falses
psid[psid.year .>=1980,"yearafter1980"] .= true # set it to true for people older tha 65

# how to create first lag of income without fancy functions?
#1. sort by id and year (reverse)
sort!(psid, ["id_ind", order("year", rev = true)]) # id increasing, year decreasing
# 2. initialize new variable
psid[!,"labor_inc_lag"] = missings(Float64, size(psid,1))
# 3.
# for every row (except last one), check if next row actually represents the very same person in the previous year.
# if yes, set labor_inc_lag to labor_inc from next row
# otherwise do nothing
for i in 1:(size(psid,1)-1)
    if psid[i,"id_ind"] == psid[i+1,"id_ind"] && psid[i,"year"] == psid[i+1,"year"] + 1 # should be the same person AND the previous year
        psid[i,"labor_inc_lag"] = psid[i+1,"labor_inc"]
    end
end
# note that this code is useable only up to 1997, since it assumes the data is annual!


