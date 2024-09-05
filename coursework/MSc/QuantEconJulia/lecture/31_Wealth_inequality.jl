
####
# restart julia here!
include("31_infinite_horizon.jl")
ep = EconPars()
np = NumPars()

sol = solve(ep,np)
sol.sp(1.0)
N = 10000
T = 200

(coh_sim, y_sim) = simulate(ep, sol, N, T)

cons_sim = [sol.cp(coh_sim[i, t]) for i in 1:N, t in 1:T]
save_sim = [sol.sp(coh_sim[i, t]) for i in 1:N, t in 1:T]

using Plots

# mean savings
plot(mean(save_sim,dims = 1)', label = "mean savings")

# savings of first individual
plot(save_sim[1,:], label = "savings of individual 1")

# savings of first 10 individual
plot(save_sim[1:10,:]', label = "")

# low wealth in the fisrt few periods
plot(save_sim[1:10,1:50]', label = "")

# wealth distribution in 1st, 2nd, 50th period of simulation

histogram(save_sim[:,1], label = "", xlabel = "wealth", normalize = true)
histogram(save_sim[:,2], label = "", xlabel = "wealth", normalize = true)
histogram(save_sim[:,50], label = "", xlabel = "wealth", normalize = true)


# have to do 'normalize' to avoid weird changes in histogram shape from varying number of column  numbers, etc.
h = histogram(save_sim[:,1], label = "", alpha = 0.2, xlims = [0,3], xlabel = "wealth", normalize = true)

for i in 2:10:200
    h = histogram!(save_sim[:,i], label = "simul $i", alpha = 0.2, normalize = true)
    display(h)
    sleep(0.3)
end
# stable distribution after a burn-in period!


# why? 

gr = range(0,10,length = 500)

plot(gr, sol.sp(gr)*(1+ep.r) .+ 1, label = "expected cash-on-hand", xlabel = "cash-on-hand")
plot!(gr,gr, label = "45° line")

plot(gr, sol.sp(gr)*(1+ep.r) .+ 1 .- gr, label = "expected change in cash-on-hand", xlabel = "cash-on-hand")

wstar = find_zero(x-> sol.sp(x)*(1+ep.r) + 1-x, (0,2.5))
vline!([wstar], label = "w*")
# w* is target wealth


# measures of inequality

function gini(values::AbstractVector)
    n = length(values)
    A = 0.0
    for i in 1:n
        for j in 1:n
            A += abs(values[i]-values[j])
        end
    end
    B = sum(values)
    return 0.5*A/B/n
end

gini(save_sim[:,1])
gini(save_sim[:,100])

function share_top(values;perc = 0.01)
    ws = sort(values, rev = true)
    len = length(ws)
    topperc_len = round(Int,len*perc)
    if sum(ws) == 0
        throw(error("Nobody saves in this economy"))
    end
    return sum(ws[1:topperc_len])/sum(ws)
end

share_top(save_sim[:,1])
share_top(save_sim[:,100])

###############################################################
# what if beta is too high? everyone would have a positive expected change in coh - > no stable distribution

ep = EconPars(β = 0.99)
sol = solve(ep,np, maxiter = 4000)

(coh_sim, y_sim) = simulate(ep, sol, N, T)

cons_sim = [sol.cp(coh_sim[i, t]) for i in 1:N, t in 1:T]
save_sim = [sol.sp(coh_sim[i, t]) for i in 1:N, t in 1:T]

h = histogram(save_sim[:,1], label = "", alpha = 0.2, xlims = [0,50], xlabel = "wealth", normalize = true)
for i in 2:5:200
    h = histogram!(save_sim[:,i], label = "simul $i", alpha = 0.2, normalize = true)
    display(h)
    sleep(0.3)
end

plot(gr, sol.sp(gr)*(1+ep.r) .+ 1 .- gr, label = "expected change in cash-on-hand", xlabel = "cash-on-hand")
# expected growth in coh is positive for everybody -> no stable distribution


#########################
# no uncertainty

# no uncertainty -> no precautionary saving. As people are impatient, they are fine being pressed to the borrowing limit a = 0 and eating 1 every period.
ep = EconPars(ys = [1.0], pys = [1.0])
sol = solve(ep,np)

(coh_sim, y_sim) = simulate(ep, sol, N, T)

cons_sim = [sol.cp(coh_sim[i, t]) for i in 1:N, t in 1:T]
save_sim = [sol.sp(coh_sim[i, t]) for i in 1:N, t in 1:T]

h = histogram(save_sim[:,1], label = "", alpha = 0.2, xlims = [0,50], xlabel = "wealth", normalize = true)
for i in 2:5:200
    h = histogram!(save_sim[:,i], label = "simul $i", alpha = 0.2, normalize = true)
    display(h)
    sleep(0.3)
end

plot(gr, sol.sp(gr)*(1+ep.r) .+ 1 .- gr, label = "expected change in cash-on-hand", xlabel = "cash-on-hand")
vline!([1.0], label = "minimal coh = 1")
# in this economy cash-on-cannot be less than 1 (certain income = 1). and for people with more coh than 1, expected change in coh is negative!! so in steady state, noone has wealth.

# what happens when simulating starting with a high wealth level?
# change one line in 'simulate':

function simulate_withstartw(ep::EconPars, sol::Solution, N::Integer, T::Integer, w::Real)
    cohs = fill(0.0,N,T)
    incomes = fill(0.0,N,T)
    y_dist = DiscreteNonParametric(ep.ys,ep.pys) # creates a discrete probability dstribution, where possible values are ys and corresponding probabilities are pys
    for n in 1:N
        y = rand(y_dist) # simulate one draw from the distribution we just defined
        cohs[n,1] = y + w # coh = y + w, where w is initial wealth
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

(coh_sim, y_sim) = simulate_withstartw(ep, sol, N, T, 40)

cons_sim = [sol.cp(coh_sim[i, t]) for i in 1:N, t in 1:T]
save_sim = [sol.sp(coh_sim[i, t]) for i in 1:N, t in 1:T]

h = histogram(save_sim[:,1], label = "", alpha = 0.2, xlims = [0,50], xlabel = "wealth", normalize = true)
for i in 2:5:200
    h = histogram!(save_sim[:,i], label = "simul $i", alpha = 0.2, normalize = true)
    display(h)
    sleep(0.3)
end