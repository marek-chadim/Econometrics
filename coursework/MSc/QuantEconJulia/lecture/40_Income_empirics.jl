using DataFrames
using CSV
using Plots
using Statistics
# using Pkg; Pkg.add.(["FixedEffectModels","CategoricalArrays","LaTeXStrings"])
using FixedEffectModels
using CategoricalArrays
using LaTeXStrings

pwd()

#cd("./julia/classes")

psid =  CSV.read("psid_final.csv", DataFrame)

# hh income is head labor income + spouse labor income + hh transfer income 
psid.income = psid.labor_inc .+ coalesce.(psid.labor_inc_spouse,0.0) .+ coalesce.(psid.transfer_inc,0.0)
unique(psid.marital_status)
psid[coalesce.(psid.marital_status .== "Married or perm. cohabiting",false),"income"] = psid[coalesce.(psid.marital_status .== "Married or perm. cohabiting",false),"income"] ./ 2

psid = psid[psid.rel_head .== "head",:]


# distribution of labor income in 2017

histogram(psid[psid.year.==2017,"income"])

# distribution of log income - nicer!
histogram(log.(psid[psid.year.==2017,"income"]))

psid = psid[:,["id_ind", "year", "age", "income"]]
psid.cohort = psid.year- psid.age

# drop observations with 0 income and missing age
psid = psid[coalesce.(psid.income.!= 0.0,false) .&& .!ismissing(psid.age),:]
# drop very old people
psid = psid[coalesce.(psid.age .< 101,false),:]

psid_income = psid[:,["id_ind", "year", "income"]]
# create wide format panel
# each row is one individual
# not an (individual,year) pair as before
psid_income = unstack(psid_income, "id_ind", "year", "income")
#compare
display(first(psid,5))
display(first(psid_income,5))

which_cohort = 1943
ids_cohort = psid[coalesce.(psid.cohort .== which_cohort,false), "id_ind"] # ids of people belonging to chosen cohort


inc_paths = psid_income[psid_income.id_ind .∈ [ids_cohort[1:20]],Not("id_ind")]
plot(names(inc_paths),Matrix(inc_paths)', label = "", xlabel = "Year", ylabel = "Dollars, real (2015 basis)", title = "Income profiles of some individuals born in $which_cohort")

plot(names(inc_paths),log.(Matrix(inc_paths)'), label = "", xlabel = "Year", ylabel = "Log income", title = "Log income profiles of some individuals born in $which_cohort")

psid.age = categorical(psid.age)

age_reg = reg(psid, @formula(log(income) ~ age + fe(cohort)), save = :residuals)

display(age_reg)

# transform such that mean log for a chosen age (42) is the same as data
age_fe = coef(age_reg) .+ mean(log.(psid[psid.age .== 42,"income"])) .- coef(age_reg)[28]
#CSV.write("./julia/classes/age_fe.csv", DataFrame(age = 17:100, fe = age_fe)) # save FEs

scatter(17:100,age_fe, xlabel = "Age", ylabel = "Average log income", label = "", title = "Age fixed effects")

psid."income_residual" = residuals(age_reg)

psid_residual = psid[:,["id_ind", "year", "income_residual"]]
psid_residual = unstack(psid_residual, "id_ind", "year", "income_residual")
# wide format

covs_w_lag = [cov(Array(dropmissing(DataFrame(a = psid_residual[:,"1970"], b = psid_residual[:,string(y)]))); dims = 1)[1,2] for y in 1970:1989]

scatter(0:19,covs_w_lag, ylims = [0,0.45], title = "\nCovariance of "*L"u_t"*" with "*L"u_{t+j}", label = "", xlabel = "j")

vars_age = [var(psid[psid.age .== a, "income_residual"]) for a in 22:75]

# compare with thick line in Fig 1 of STY
scatter(22:75, vars_age, xlabel = "Age", title = "Variance of log income residual", label = "")

