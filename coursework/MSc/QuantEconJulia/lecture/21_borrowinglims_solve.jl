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
using Optim

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

function quadratic_utility_function(cbar)
    return c -> -(c-cbar)^2 # returns a function, which to any consumption level, assigns the corresponding quadratic utility if bliss point is cbar
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
    "utility function"
    u = crra_utility_function(2) # default means: crra utility with risk aversion (γ) parameter being 2 
    "number of time periods"
    T = 60
    "maximal amount of debt"
    bl = 0.0 # default means: no borrowing allowed. a positive value means debt is allowed!
end


"""
This structure stores all numerical parameters needed to solve the model

At the moment our grid for cash-on-hand is an evenly spaced range from min_coh to max_coh, consisting of N_coh numbers. (This is not ideal, but simplest at this point).
"""
@with_kw struct NumPars
    "minimum of cash-on-hand grid (relative to borrowing limit)"
    min_coh = 10^(-8) # small number. shouldn't compute utility with c = 0 exactly (to avoid badly defined utility), but we should do it for some small value
    "maximum of cash-on-hand grid (relative to borrowing limit)"
    max_coh = 6.0
    "number of points on cash-on-hand grid"
    N_coh = 500 # number of grid points
end

"""
This structure stores the solution. Each field is a vector of interpolated functions, corresponding to age. For example, cp[5] is the optimal consumption policy function of agents with age = 5. cp[5](0.8) would give optimal consumption of agents of age 5 who have 0.8 units of cash
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
natural borrowing limit is the highest debt level that could be payed back until time T (without negative consumption ever) even if the worst income path realizes for the rest of life. One can show:
nat_bls_t = sum_s min(y_s)/(1+r)^(s-t)
where s runs from t+1 to T, and min(y_s) denotes the worst possible income realization in time s.

This function computes this for all ts and puts them in a vector of length T.
"""
function natural_borrowing_limits(r,ys,T)
    nat_bls = fill(0.0,T)
    minimal_income = minimum(ys)
    for t in (T-1):-1:1
        nat_bls[t] = (nat_bls[t+1]+minimal_income)/(1+r)
    end
    return nat_bls
end

"""
Solves T-period optimal consumption-saving problem from lecture in the presence of  
 - borrowing limits
 - i.i.d. income shocks
# Arguments:
 - economic parameters
 - numerical parameters
# Return a Solution structure with the value function and optimal consumption and saving policies for all ages.

# Note: This is not fastest way to solve the model, but a faster code would be harder to read.
"""
function solve(ep::EconPars, np::NumPars)
    @unpack_EconPars ep
    @unpack_NumPars np

    nat_bls = natural_borrowing_limits(r, ys, T)

    # initialize containers of value and policy functions with empty containers of T undefined objects. these will be overwritten
    cp = Vector{Any}(undef, T)
    sp = Vector{Any}(undef, T)
    vf = Vector{Any}(undef, T)

    cohs = range(min_coh, max_coh, length=N_coh) # evenly spaced grid for cash-on-hand values, over which we interpolate value and policy functions

    # setting policies and value in the last period. In this case, it is optimal to eat everything and save nothing. Therefore the value function equals utility from eating everything

    cp[T] = linear_interpolation(cohs, cohs, extrapolation_bc = Linear()) # interpolate identity function: for any cash-on-hand level, you eat it all.
    sp[T] = linear_interpolation(cohs, fill(0.0, N_coh), extrapolation_bc = Linear()) # interpolate 0: for any cash-on-hand level, you save nothing.
    vf[T] = linear_interpolation(cohs, u.(cohs), extrapolation_bc = Linear()) # since you eat everything, value = u

    # the extrapolation_bc = Linear() term sets that if you feed in a value to these interpolated functions which is not over the grid, it can still evaluate the linear approximation outside the grid.

    for t in (T-1):-1:1 # t goes from T-1 to 1 !BACKWARDS!

        #initialize vectors that will hold values and policies corresponding to each cash-and-hand grid point. we will overwrite these 0s in the for loop below
        cs = fill(0.0, N_coh) # consumption
        as = fill(0.0, N_coh) # end of asset
        vs = fill(0.0, N_coh) # value

        # we don't want to only interpolate over a grid that starts at 0. Since borrowing might be allowed, we want to compute stuff over negative cash-on-hand levels as well if that cash-on-hand ensures a well-defined solution. A certain level of coh is an ok as starting point in period t, if
        # 1. it ensures that borrowing limit is not violated in the end of period if conmsumption is small enough (but still positive). a_t + c_t = coh_t (cash-on-hand), and a_t>= -bl, so this means coh_t should be at least -bl and then both the borrowing limit and the budget constraint can be satisfied with non-negative consumption.
        # 2. ensures that debt is possible to repay over the rest of life even if the worst income path realizes (i.e. coh is at least -nat_bls_t), if sufficiently small (almost 0) consumption is chosen. This means end-of-period saving has to be at least the natural borrowing limit.

        # so we shift the coh grid by the minimum amount of cash-on-hand that results in a sensible solution:

        mincoh = max(-bl, -nat_bls[t]) # mincoh as savings (happens when c = 0) should be both (1) allowed, and (2) possible to repay without going into negative consumption.
        cohs_at_t = cohs .+ mincoh # we have an age specific coh grid, since the amount of repayable debt varies with age!

        for i in eachindex(cohs_at_t) # i runs through all indices of the possible cash-on-hand values


            function value_if_save_a(a) # define a function that to all candidate for end-of-period savings assigns the corresponding utility. This is the RHS of the Bellman equation assuming we save 'a'. This comes from two sources
                value_now = u(cohs_at_t[i] - a) # utility from eating everything you don't save
                value_future = 0.0
                for yi in eachindex(ys) # add expected value from next period for each possible value of y
                    value_future = value_future + pys[yi] * vf[t+1]((1 + r) * a + ys[yi])
                end
                return value_now + β * value_future
            end

            tominimize(a) = -value_if_save_a(a) # multiply by -1, since optimize minimizes functions
            opt_a = optimize(tominimize, mincoh, cohs_at_t[i]).minimizer
            # minimize function 'tomin'
            # - such that savings is above the minimum amount of end-of-period savings
            # - savings is less than cash-on-hand, enabling non-negative consumption
            #   why is this so? a = coh-c, so a<coh is equivalent to c>0. 
            cs[i] = cohs_at_t[i] - opt_a # consumption is coh - savings
            as[i] = opt_a
            vs[i] = value_if_save_a(opt_a) # we just evaluate the above defined function to get the value corresponding to optimal saving
        end
        # and now we can interpolate the value and policy functions for age t
        cp[t] = linear_interpolation(cohs_at_t, cs, extrapolation_bc = Linear())
        sp[t] = linear_interpolation(cohs_at_t, as, extrapolation_bc = Linear())
        vf[t] = linear_interpolation(cohs_at_t, vs, extrapolation_bc = Linear())
    end
    return Solution(vf, cp, sp)
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
function simulate(ep::EconPars, sol::Solution, N::Integer)
    cohs = fill(0.0,N,ep.T)
    incomes = fill(0.0,N,ep.T)
    y_dist = DiscreteNonParametric(ep.ys,ep.pys) # creates a discrete probability dstribution, where possible values are ys and corresponding probabilities are pys
    for n in 1:N
        y = rand(y_dist) # simulate one draw from the distribution we just defined
        cohs[n,1] = y # at age = 1, coh = y, since there is no initial wealth
        incomes[n,1] = y
    end
    for t in 2:ep.T
        for n in 1:N
            y = rand(y_dist)
            cohs[n,t] = sol.sp[t-1](cohs[n,t-1])*(1+ep.r) + y # cash on hand today is after-return savings from previous time period, + income
            incomes[n,t] = y
        end        
    end
    return (cohs,incomes)
end