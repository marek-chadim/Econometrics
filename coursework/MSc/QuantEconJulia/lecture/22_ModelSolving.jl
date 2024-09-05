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

cd("julia/classes")
include("21_borrowinglims_solve.jl")

#################
#### CRRA utility
#################

# utility values:
crra_utility(-2.1,1)

crra_utility(2.1,1)

crra_utility(2.1,1.01)

# crra_utility_function(γ) returns a FUNCTION!

crra_utility_function(1.01) # this is a function

crra_utility_function(1.01)(2.1) # this is the utility
# we could:

u = crra_utility_function(1.01)
u(2.1)

cgrid = range(10^-7, 4, length = 1000)

plot(cgrid, crra_utility_function(0.5))


gammas  = [0.0,0.5,1.0,1.5]

reshape(["γ = $gamma" for gamma in gammas],(1,4))

plot(cgrid, [crra_utility_function(gamma) for gamma in gammas], labels = permutedims(["γ = $gamma" for gamma in gammas]), ylims = [-10,4], linewidth = 1.5, title = "CRRA utility functions for different parameter values", xlabel = "Consumption", ylabel = "Utility")

# set economic parameters
ep = EconPars() # empty brackets means leaving defaults
# if you want you can overwrite any defaults: Look at the definintion of Econpars in 21_borrowinglims_solve.jl to find possible field names. then just give the values you'd like to the fields you want to change relative to defaults
# Examples 
# ep = EconPars(r = 0.01) # or
# ep = EconPars(T = 40, r = 0.01)

#############################
#### Natural borrowing limits
#############################

nbl = natural_borrowing_limits(0.02,[0.9,1.1],60)
plot(nbl)

rs = [0.0,0.01,0.02,0.05,0.1]

plot([natural_borrowing_limits(r,[1.0],60) for r in rs], labels = permutedims(["r = $r" for r in rs]), linewidth = 1.5, title = "Natural borrowing limits for different interest rates (r)", ylabel = "Maximum allowed debt", xlabel = "Age")

#############################
#### Interpolation (again)
#############################

xgrid_sparse = range(-10,10,length = 15)

scatter(xgrid_sparse,sin, label="approximate based on these points")

xgrid_dense = range(-10,10,length = 1000)

plot!(xgrid_dense,sin,label = "exact values")

approx_linear = linear_interpolation(xgrid_sparse,sin.(xgrid_sparse))

plot!(xgrid_dense,approx_linear.(xgrid_dense),label = "linear approximation")

# interpolated functions behave like functions

approx_linear(-1.2)
sin(-1.2)

# we cannot evaluate outside of original grid

approx_linear(-15) # error since -15 is outside of original grid

# we have to tell what to do when evaluated at points outside the grid
# called 'extrapolation'
# we used linear extrapolation in code
approx_linear_with_extpr = linear_interpolation(xgrid_sparse,sin.(xgrid_sparse), extrapolation_bc = Linear())

approx_linear_with_extpr(-15)


xgrid_wide = range(-15,15,length = 1000)
scatter(xgrid_sparse,sin, label="approximate based on these points")
plot!(xgrid_wide,sin,label = "exact values")
plot!(xgrid_wide,approx_linear_with_extpr.(xgrid_wide),label = "linear approximation with linear extrapolation")

# use extrapolation only if you know what you are doing!!

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

# cross sectional averages for each age:
# mean(cons_sim, dims = 1) gives the column-wise means of matrix cons_sim (so one mean for each age group). this will be given as a row vector, so we have to transpose it (by ' ) before giving it to plot
plot(mean(cons_sim, dims = 1)', color = :blue, linewidth=2, label = "consumption", ylims = [minimum(save_sim),maximum(cons_sim)])
plot!(mean(save_sim, dims = 1)', color = :red, linewidth=2, label = "savings")
plot!(mean(y_sim, dims = 1)', color = :green, linewidth=2, label = "income")