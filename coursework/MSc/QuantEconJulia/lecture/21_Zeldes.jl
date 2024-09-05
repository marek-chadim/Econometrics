####################################
# loading all packages here
###############
# if any of them is not installed on your current device, run
# using Pkg
# and then install like
# Pkg.add("Distributions")
# Pkg.add("GLM")
using Plots
using DataFrames
using GLM

cd("C:/Users/chadi/OneDrive - Handelshögskolan i Stockholm/SSE/5329/code")
pwd()
include("21_borrowinglims_solve.jl")

# set economic parameters
ep = EconPars(bl=0) # empty brackets means leaving defaults
# if you want you can overwrite any defaults: Look at the definintion of Econpars in 21_borrowinglims_solve.jl to find possible field names. then just give the values you'd like to the fields you want to change relative to defaults
# Examples 
# ep = EconPars(r = 0.01) # or
# ep = EconPars(T = 40, r = 0.01)

# set numerical parameters for solving optimization problem
np = NumPars()

# solve optimization problem
@time sol = solve(ep,np)

# check how solution looks for age = 3 agents
t = 3

# this produces the relevant cash-on-hand grid for agents of age t. don't worry about understanding this line, specific command for the Interpolations package
grid = collect(knots(sol.vf[t]))

# value function
plot(grid,sol.vf[t].(grid), ylims = [-20,20], label = "value function", xlabel = "Cash-on-hand")

# optimal consumption
plot(grid,sol.cp[t].(grid), label = "optimal consumption")
plot!(0:0.1:1.2,0:0.1:1.2 .- ep.bl, label = "45° line - consume everything", linestyle = :dash, xlabel = "Cash-on-hand")

# optimal savings
plot(grid,sol.sp[t].(grid), label = "optimal saving", xlabel = "Cash-on-hand")


# number of individuals to simulate
N = 1000

# simulate cash-on-hand and income paths for N individuals
(coh_sim, y_sim) = simulate(ep,sol,N)
# we obtained two N*T matrices. 
# - The first one contains cash-on-hand simulation
# - The second one contains income simulation
# every row represents an individual, every column represents an age group
# coh_sim[i,t] shows cash-on-hand of individual i at time/age t

# to compute corresponding consumption, we can use optimal consumption policies
cons_sim = hcat([ sol.cp[t].(coh_sim[:,t]) for t in 1:ep.T ]...)
#what's happening? 
# - sol.cp[t](coh_sim[i,t]) would give us optimal consumption of agent i at time t knowing that she has coh_sim[i,t] cash-on-hand at that time. Note that we apply the optimal consumption policy appropriate for age t individuals, namely sol.cp[t].
# - sol.cp[t].(coh_sim[:,t]) performs this computations for each individuals of age t at once. this gives us a column vector
# - [ sol.cp[t].(coh_sim[:,t]) for t in 1:ep.T ] gives us a collection of column vectors, containing the above values in a separate column for each age group
# hcat(   ...) turns this into a matrix, since we want our simulated consumption have the same format as previous simulated quantities.

# to compute corresponding savings = end-of -period wealth, we can use the optimal saving policies
save_sim = hcat([ sol.sp[t].(coh_sim[:,t]) for t in 1:ep.T ]...)
# same as for consumption, but we use saving policy

# let's see how a simulated lifecycle looks like!
i = 1 # choose an individual to plot: any number from 1 to N
plot(cons_sim[i,:], label = "consumption of individual $i over time")
plot!(y_sim[i,:], label = "income of individual $i over time")
plot!(save_sim[i,:], label = "savings of individual $i over time")
# you should see that consumption fluctuations are smoother than income


# check if sufficiently lower than maximal coh grid point, IGNORE NOW
maximum(save_sim)

# we will need a definition of 'low wealth' to replicate Zeldes' analysis in our simulated dataset. We pick some level a bit above the borrowing limit
low_wealth_limit = -ep.bl + 0.05

# cross sectional averages for each age:
# mean(cons_sim, dims = 1) gives the column-wise means of matrix cons_sim (so one mean for each age group). this will be given as a row vector, so we have to transpose it (by ' ) before giving it to plot
plot(mean(cons_sim, dims = 1)', color = :blue, linewidth=2, label = "consumption", ylims = [minimum(save_sim),maximum(cons_sim)])
plot!(mean(save_sim, dims = 1)', color = :red, linewidth=2, label = "savings")
plot!(mean(y_sim, dims = 1)', color = :green, linewidth=2, label = "income")
plot!(mean(save_sim.<low_wealth_limit, dims = 1)', color = :red, linewidth=2, linestyle = :dashdot, label = "savings below $low_wealth_limit") # share of households with savings below low wealth limit
# save_sim.<low_wealth_limit gives a matrix of logical values. taking mean column-wise gives you the share of 'true's for each age group. Remember that 'true' and 'false' are treated as 1 and 0 in such contexts.

# for age T agents future consumption is missing, for the rest it is simply the simulated values shifted by 1 time period
future_cons_sim = hcat(cons_sim[:,2:end],fill(missing,N))

# gross rate of consumption growth is the ratio of future and current consumption 
cons_growth_sim = future_cons_sim./cons_sim

# ages of all agent
ages_sim = [t for i in 1:N, t in 1:ep.T]

# let's put our simulated data in a dataframe
data_sim = DataFrame(age = ages_sim[:], consumption_growth = cons_growth_sim[:], income = y_sim[:], wealth = save_sim[:])
# we need to pass vectors as variables. how did we do that?
# if A is an array, then we know that A[4:8] gives a part of A as a vector. we also know that : represents 'all indices'. So A[:] gives back all values in a vector form!
# alternatively, there exists a function: vec(A) would do the same

# lets create sample 1 and 2 of Zeldes
# we can simply use wealth = savings to tell who is constrained

# I don't do == 0 to test if constrained, but < low_wealth_limit, since numerical errors might make true 0s look like small positive numbers

constrained_sample = data_sim[data_sim.wealth .< low_wealth_limit,:] # consists of observations, for which wealth is less than the limit we defined above

unconstrained_sample = data_sim[data_sim.wealth .> low_wealth_limit,:]

 println(size(constrained_sample,1)/size(data_sim,1))
# test (i)

# how to run regressions in Julia: everything looks a bit like R, but we have to write the formula inside the @formula() magic. This is not something you are supposed to understand

test_i_constrained = lm(@formula(log(consumption_growth) ~ age + log(income)), constrained_sample)
# Zeldes says income should be significantly negative in constrained sample
println(test_i_constrained)

test_i_unconstrained = lm(@formula(log(consumption_growth) ~ age + log(income)), unconstrained_sample)
# but should be 0 in unconstrained sample
println(test_i_unconstrained)


# some useful commands for the Assignment:

# this is how to extract regression coefficients as a vector:
coef(test_i_unconstrained)

# if you want to test if the mean of a particular vector is 0, you should just regress the vector on a constant and interpret the result approprately. 
# e.g.
boring_data = DataFrame(x = rand(100))
whatsmean = lm(@formula(x ~ 1 ), boring_data)
println(whatsmean)