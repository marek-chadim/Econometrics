cd("C:/Users/chadi/OneDrive - Handelshögskolan i Stockholm/SSE/5329/code")
pwd()


using DataFrames
using CSV # to read CSV files
using Statistics

psid = DataFrame(CSV.File("psid.csv"))



###############
# Task 1
###############

# the simplest option is first turning all missing values in "labor_inc_spouse" into 0s (this is simplest to do with the coalesce function),
psid."labor_inc_spouse" .= coalesce.(psid."labor_inc_spouse",0.0)

# after which we can simply take the sum of "labor_inc" and "labor_inc_spouse" to obtain family-level income.
psid."labor_inc_family" = psid."labor_inc" .+ psid."labor_inc_spouse"

# on coalesce: coalesce(x, something) returns x if x is not missing, but returns 'something' instead if 'x' is missing. We need to add the broadcasting . when applying this function on a vector. Therefore coalesce.(psid."labor_inc_spouse",0.0) replaces all missing elements of psid."labor_inc_spouse"
# with 0, but keeps all other values

###############
# Task 2
###############

psid = psid[psid.rel_head.=="head",:]

# psid.rel_head.=="head" returns a vector of logical values, 'true' for rows where "rel_head" variable has value "head", 'false' otherwise

# when giving a logical vector as index, only indices at 'true' are returned. This is exactly what we want, since we only want rows which represent "head"s.

###############
# Task 3
###############

psid = psid[:,["id_ind","year", "family_comp_change", "expenditure_family", "net_wealth_family", "labor_inc_family"]]

###############
# Task 4
###############

# This can be solved with specialized functions from DataFrames package. I won't do that, since for this course it's more instructive to use base Julia.

# Two steps: 
# 1. we need the list of all years that appear in PSID
# 2. for all years we compute the respective quantities

# two ways for 1.

# I. manual: PSID starts from 1968, biannual from 1997, stops in 2021, so 
years = vcat(1968:1997,1999:2:2021)
# not sure I showed 1999:2:2021, this creates a range, but jumping two years at a time.

# II. we take the 'year' column of psid and throw duplicated with unique
years = unique(psid[:,"year"])

# two ways for 2.

# I. we first initialize the first vector, then fill them up in a for loop
labinc_mean = fill(0.0,42)
labinc_var = fill(0.0,42)

exp_mean = fill(0.0,42)
exp_var = fill(0.0,42)

for yi in eachindex(years)
    labinc_mean[yi] = mean(skipmissing(psid[psid.year .== years[yi],"labor_inc_family"]))
    labinc_var[yi] = var(skipmissing(psid[psid.year .== years[yi],"labor_inc_family"]))
    exp_mean[yi] = mean(skipmissing(psid[psid.year .== years[yi],"expenditure_family"]))
    exp_var[yi] = var(skipmissing(psid[psid.year .== years[yi],"expenditure_family"]))
end
# what is happening? yi runs through all indices of years, so from 1 to 42.
# for each index, psid[psid.year .== years[yi],"interesting variable"] takes the "interesting variable" column of psid, but only the rows corresponding to year equaling to the yith element of the years vector. then we skip missing variables and compute whichever statistic we want. then we put this number in the right (yith) position of our prepared vectors.

# II. we don't initialize, but create the vectors directly by comprehensions.

labinc_mean = [mean(skipmissing(psid[psid.year .== years[yi],"labor_inc_family"])) for yi in eachindex(years)]
labinc_var = [var(skipmissing(psid[psid.year .== years[yi],"labor_inc_family"])) for yi in eachindex(years)]
exp_mean = [mean(skipmissing(psid[psid.year .== years[yi],"expenditure_family"])) for yi in eachindex(years)]
exp_var = [var(skipmissing(psid[psid.year .== years[yi],"expenditure_family"])) for yi in eachindex(years)]

###############
# Task 5
###############

using Plots
plot(years,labinc_mean, label = "Mean of labor income", xlabel = "Years", ylabel = "Dollars", title = "Means of income and expenditure", color = :blue, linewidth = 2, legend = :topleft)
plot!(years,exp_mean, label = "Mean of expenditure", color = :red, linewidth = 2)

plot(years,labinc_var, label = "Variance of labor income", xlabel = "Years", ylabel = "Dollars", title = "Variances of income and expenditure", color = :blue, linewidth = 2, linestyle = :dashdot, legend = :topleft)
plot!(years,exp_var, label = "Variance of expenditure", color = :red, linewidth = 2, linestyle = :dashdot)

# I never mentioned this :somename things, as we'll never see them, except in plots. For many keywords of plot, this is how we give possible options, which are already defined. For example, linestyle can take :auto, :solid, :dash, :dot, :dashdot or :dashdotdot. There are a huge number of possible keywords and respective options. You can find them on the documentation of the Plots package. For example, this page lists options only for lines and similar parts of a plot: https://docs.juliaplots.org/latest/generated/attributes_series/ If you in general want to know more about plotting in Julia, it's best to look at this tutorial of Plots: https://docs.juliaplots.org/latest/tutorial/

###############
# Task 6
###############

psid = psid[psid.year.>=1999 .&& psid.family_comp_change.==0,:]
# rows where year is at least 1999 AND (&&) family_comp_change is 0
vscodedisplay(psid)
###############
# Task 7
###############
sort!(psid, ["id_ind", order("year", rev = true)])
# how to create this function? take the code fragment from 8_dataframes.jl and replace
# - psid with some variable name (I used df) 
# - "id_ind" with some variable name (I used id) 
# - "year" with some variable name (I used time) 
# - "labor_inc" with some variable name (I used tolag)

# then need to take care of the new column name: glue together 'tolag' with "_lag_2" using the * operator. Like this the new column name will be determined by the variable that you take the lag of, as intended.
function lagby2!(df::DataFrame,id::String,time::String,tolag::String)
    sort!(df, [id, order(time, rev = true)])
    df[!,tolag*"_lag_2"] = missings(Float64, size(df,1))
    for i in 1:(size(df,1)-1)
        if df[i,id] == df[i+1,id] && df[i,time] == df[i+1,time] + 2 # this +2 here is the change due to working with biannual data. we have to check whether the observation below the current row was !!!2!!! years ago, instead of 1
            df[i,tolag*"_lag_2"] = df[i+1,tolag]
        end
    end
end
# Why is it good to write functions when doing any task that you might do more than once?:
# - shorter than copying and changing little parts all the time (compare with copying the for loop three times and replacing the variable names by hand, separately)
# - less error-prone. easy to make mistakes when copying and replacing small pieces
# - code reuse: this function is easy to adapt to other purposes. if someone gives you a dataset, where data is collected every 5 years (census), you change one character and can apply this function
# - clarity: functions have well-defined inputs and outputs and should have a clear purpose. Complicated codes are easier to understand if you build them from functions. More clear, which part of the code does what.
# + 1 reason in Julia: code inside functions is faster than scripts using global variabes in the 'main scope'.
# TLDR: almost always, when you copy/paste within one file during coding, you should have written a function. This is of course just some general advice on good coding style, not a subject of this course.

###############
# Task 8
###############

# now we can create the lagged column for three variables like this
lagby2!(psid,"id_ind","year","net_wealth_family")
lagby2!(psid,"id_ind","year","expenditure_family")
lagby2!(psid,"id_ind","year","labor_inc_family")

# or even like this in a for loop:
for tolag in ["net_wealth_family", "expenditure_family", "labor_inc_family"]
    lagby2!(psid,"id_ind","year",tolag)
end

# changes are differences of current values with lagged counterparts:
psid."expenditure_family_change" = psid."expenditure_family".-psid."expenditure_family_lag_2"

psid."net_wealth_family_change" = psid."net_wealth_family".-psid."net_wealth_family_lag_2"

psid."labor_inc_family_change" = psid."labor_inc_family".-psid."labor_inc_family_lag_2"

# again, we could write a for loop

for col in ["expenditure_family", "net_wealth_family", "labor_inc_family"]
    psid[:,col*"_change"] = psid[:,col] .- psid[:,col*"_lag_2"]
end
# for this the shorter psid.col syntax wouldn't work, that is only for giving the column name as a ready, manually typed string, not something constructed on the fly.


###############
# Task 9
###############

# this is exactly as line 125-132 in 8_dataframes.jl, see explanation there
cor(Array(dropmissing(psid[:,["expenditure_family_change", "labor_inc_family_change"]])); dims = 1)[2,1]
cor(Array(dropmissing(psid[:,["net_wealth_family_change", "labor_inc_family_change"]])); dims = 1)[2,1]
