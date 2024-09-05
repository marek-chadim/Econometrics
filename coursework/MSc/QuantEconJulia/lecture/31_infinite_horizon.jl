####################################
# loading all packages here
###############
# if any of them is not installed on your current device, run
# using Pkg
# and then install like
# Pkg.add("Distributions")

using Distributions
using Parameters
using Interpolations
using Roots
using LinearAlgebra

function crra_utility(c,γ)
    if c <= 0 
        throw(DomainError("consumption has to be positive with CRRA utility"))
    else
        if γ < 0
            throw(DomainError("γ has to be positive"))
        elseif γ == 1 # when γ is 1 and hence the usual formula is not well-defined, CRRA simplifies to log utility. Can prove using the Hopital-rule.
            return log(c)
        else
            return (c^(1-γ)-1)/(1 - γ) # usual formula for CRRA
        end
    end    
end

function crra_utility_function(γ)
    return c -> crra_utility(c,γ) # returns a function, which to any consumption level, assigns the corresponding CRRA utility under risk aversion equal to γ
end

"""
This structure stores all parameters describing preferences and the economic environment
"""
@with_kw struct EconPars
    "interest rate on savings"
    r = 0.02 # default means: 2%
    "vector containing possible income realizations"
    ys = [0.5, 0.8, 1.0 ,1.2 ,1.5] # default: 5 possible income levels, worst is 0.5, best is 1.5
    "vector containing probabilities corresponding to possible income levels"
    pys = [0.05, 0.3, 0.3 ,0.3 ,0.05] # default: extreme values are less likely, middle values more likely
    "discount factor"
    β = 0.97
    "risk aversion of used CRRA utility function"
    γ = 2.0
    "maximal amount of debt"
    bl = 0.0 # default means: no borrowing allowed. a positive value means debt is allowed!
end


"""
This structure stores all numerical parameters needed to solve the model

We use an evenly spaced grid for savings (not cash-on-hand as before).
"""
@with_kw struct NumPars
    "maximum of savings grid"
    max_save = 10.0
    "number of points on savings grid"
    N_save = 100
end

"""
This structure stores the solution. Each field is an interpolated function. For example, cp is the optimal consumption policy function, and cp(0.8) would give optimal consumption of agents who have 0.8 units of cash.
"""
struct Solution
    "value functions for all ages"
    vf
    "optimal consumption policies for all ages"
    cp
    "optimal saving policies for all ages"
    sp
end

"""
Solves the infinite horizon optimal consumption-saving problem from lecture in the presence of  
 - borrowing limits
 - i.i.d. income shocks
# Arguments:
 - economic parameters
 - numerical parameters
# Return a Solution structure with the value function and optimal consumption and saving policies.

# Note: This is still not fastest way to solve the model, but a faster code would be harder to read.
"""
function solve(ep::EconPars, np::NumPars; conv_tol = 10^-5, maxiter = 500)
    @unpack_EconPars ep
    @unpack_NumPars np

    ϵ = 10^-7 # very small number that we sometimes add to lowest grid point, to avoid getting -infinities.

    u = crra_utility_function(γ) # this code works for crra utility only

    nat_bls =  minimum(ys)/r
    minsave = max(-bl, -nat_bls)

    saves = range(minsave, max_save, length=N_save) # evenly spaced grid for end-of-period savings (wealth) values.

    # setting initial guesses for policies and the value function. In this case, we choose the policies of eating everything and saving nothing as a starting point. Therefore our guess for the value function will equal the utility from eating everything
    grid_for_guess = range(0.0, max_save, length=N_save)

    cp_guess = linear_interpolation(grid_for_guess, grid_for_guess, extrapolation_bc = Linear()) # interpolate identity function: for any cash-on-hand level, you eat it all.
    sp_guess = linear_interpolation(grid_for_guess, fill(0.0, N_save), extrapolation_bc = Linear()) # interpolate 0: for any cash-on-hand level, you save nothing.
    vf_guess = linear_interpolation(grid_for_guess.+ϵ, u.(grid_for_guess.+ϵ), extrapolation_bc = Linear()) # since you eat everything, value = u

    # the extrapolation_bc = Linear() term sets that if you feed in a value to these interpolated functions which is not over the grid, it can still evaluate the linear approximation outside the grid.


    # initialize the new policies. it doesn't matter what they are, I just copied the guesses
    cp_new = deepcopy(cp_guess)
    sp_new = deepcopy(sp_guess)
    vf_new = deepcopy(vf_guess)
    # 'deepcopy' is like 'copy', but works even for complicated objects, like interpolated functions.


    dif = 100.0
    iter = 1

    while dif > conv_tol && iter < maxiter # run the block below if dif is too big and haven't run more than maxiter times already

        #initialize vectors that will hold values and policies corresponding to each grid point. we will overwrite these 0s in the for loop below
        cs = fill(0.0, N_save) # consumption
        as = fill(0.0, N_save) # end of asset
        vs = fill(0.0, N_save) # value
        cohs = fill(0.0, N_save) # cash-on-hand

        function value(c,a) # define a function that to a given level of current consumption and savings, computes value, assuming that from the next period vf_guess applies. This comes from two sources
            value_now = u(c) # utility from eating c now
            value_future = 0.0
            for yi in eachindex(ys) # add expected value from next period for each possible value of y
                coh_future = (1 + r) * a + ys[yi]
                value_future = value_future + pys[yi] * vf_guess(coh_future)
            end
            return value_now + β * value_future
        end

        for i in eachindex(saves) # i runs through all indices of the possible savings values

            # compute RHS of Euler equation for a given savings grid point. we apply cp_guess in the next period
            RHS_of_euler = 0.0
            for yi in eachindex(ys) # add expected value from next period for each possible value of y
                coh_future = (1 + r) * saves[i] + ys[yi]
                RHS_of_euler = RHS_of_euler + pys[yi] * (cp_guess(coh_future))^(-γ)
            end
            RHS_of_euler = β * (1+r) * RHS_of_euler # RHS of Euler-equation is computed

            c = RHS_of_euler^(-1/γ) # compute current consumption from Euler-equation

            cohs[i] = c + saves[i] # cash-on-hand is used for consumption and savings today, so it must be the sum of the two.

            cs[i] = c
            as[i] = saves[i]
            vs[i] = value(c,saves[i]) # we just evaluate the above defined function to get the value from the c today and corresponding saving.
        end

        if cohs[1] > minsave + ϵ # if the coh level corresponding to minimal saving is too high,
            # then we addone more grid point to capture the eat-everything policy of very poor agents
            pushfirst!(cohs, minsave + ϵ) # first coh point should be minsave + ϵ
            pushfirst!(as, minsave) # save the minimum allowed
            pushfirst!(cs, ϵ) # and eat the rest            
            pushfirst!(vs, value(ϵ,minsave))
        end

        # and now we can interpolate the value and policy functions for age t
        cp_new = linear_interpolation(cohs, cs, extrapolation_bc = Linear())
        sp_new = linear_interpolation(cohs, as, extrapolation_bc = Linear())
        vf_new = linear_interpolation(cohs, vs, extrapolation_bc = Linear())

        dif = norm(cp_new.(cohs)./cp_guess.(cohs).-1,Inf) + norm(vf_new.(cohs)./vf_guess.(cohs).-1,Inf) # we want to stop when both the consumption policy and the value function converged
        cp_guess = deepcopy(cp_new)
        sp_guess = deepcopy(sp_new)
        vf_guess = deepcopy(vf_new)
        #println("done with iteration $iter, difference was $dif")
        iter += 1
    end
    if dif > conv_tol
        println("didn't converge in $maxiter iterations")
    end
    return Solution(vf_new, cp_new, sp_new)
end

"""
Simulates life-cycle paths of N agents.

It is assumed that everyone starts with 0 wealth

Returns two matrices:
 - first contains the simulated cash-on-hand values
 - second contains simualted income series
Coh and income determines everything else.

Every row is an individual, every column is an age group
"""
function simulate(ep::EconPars, sol::Solution, N::Integer, T::Integer)
    cohs = fill(0.0,N,T)
    incomes = fill(0.0,N,T)
    y_dist = DiscreteNonParametric(ep.ys,ep.pys) # creates a discrete probability dstribution, where possible values are ys and corresponding probabilities are pys
    for n in 1:N
        y = rand(y_dist) # simulate one draw from the distribution we just defined
        cohs[n,1] = y # at age = 1, coh = y, since there is no initial wealth
        incomes[n,1] = y
    end
    for t in 2:T
        for n in 1:N
            y = rand(y_dist)
            cohs[n,t] = sol.sp(cohs[n,t-1])*(1+ep.r) + y # cash on hand today is after-return savings from previous time period, + income
            incomes[n,t] = y
        end        
    end
    return (cohs,incomes)
end