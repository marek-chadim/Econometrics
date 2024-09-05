using Plots
using Statistics

pwd()

#cd("./julia/classes")

include("41_inheritence_solve.jl")
# experiment

# numerical parameters
np = NumPars(N_z = 11, N_α = 11, N_ϵ = 11)

# benchmark: no link between generations - fully redistributed accidental bequests, no inheritence of abilities 
ep_nolink = EconPars(β = 0.9645, τ_b = 0.9999, Φ_1 = -10^-9, ρ_α = 0.0)

# if interested in how much lifecycle income profile matters, run with flat income profile
# ep_nolink = EconPars(β = 0.9665, τ_b = 0.9999, Φ_1 = -10^-9, ρ_α = 0.0,ks = fill(0.0,79))

#τ_nolink = find_τ_l(ep_nolink, np, N = 10000, M = 20) 
τ_nolink = 0.1567

# solve
sol_nolink = solve(ep_nolink,np, τ_nolink)

# simulate
(cohs_nolink,incomes_nolink,saves_nolink,conss_nolink,survives_nolink,death_now_nolink) = simulate(ep_nolink, np, sol_nolink, τ_nolink, M = 20, N = 10000)

# wealth to income ratio - should be close to 3
# vary β to achieve this
sum(saves_nolink[:,:,end][survives_nolink[:,:,end]])/sum(incomes_nolink[:,:,end][survives_nolink[:,:,end]])

# income variance has been scaled down to match income inequality in data. compare with Table 2 in De Nardi, US.
incs = (incomes_nolink[:,1:44,end][survives_nolink[:,1:44,end]])[:]

share_top(incs; perc = 0.01)
share_top(incs; perc = 0.05)
share_top(incs; perc = 0.2)
gini(sample(incs,10000))


# wealth distribution
wealthd = (saves_nolink[:,:,end][survives_nolink[:,:,end]])[:]

# somewhat more equal than should
# compare with Table 1 in De Nardi
share_top(wealthd; perc = 0.01)
share_top(wealthd; perc = 0.05)
gini(sample(wealthd,10000))

# share of agents with 0
share_at0(wealthd)

# what is the problem?

# much of the iniquality is driven by differences across age groups. Let's check this by looking at within age group measures of inequality
inc_ginis = [gini(sample(incomes_nolink[:,t,end][survives_nolink[:,t,end]],10000)) for t in axes(incomes_nolink,2)]
wealth_ginis = [gini(sample(saves_nolink[:,t,end][survives_nolink[:,t,end]],10000)) for t in axes(saves_nolink,2)]

plot(22:100,inc_ginis, color=:blue, linewidth=2, label="Income", xlabel = "Age", title = "Gini coefficients within age groups")
plot!(22:100,wealth_ginis, color=:red, linewidth=2, label="Wealth", xlabel = "Age", title = "Gini coefficients within age groups")
hline!([gini(sample(incs,10000))], color=:blue, linewidth=2, linestyle = :dash, label = "Income - pooled, working age sample")
hline!([gini(sample(wealthd,10000))], color=:red, linewidth=2, linestyle = :dash, label = "Wealth - pooled full sample")


################
# with bequest motive

ep_bequest = EconPars(β = 0.9349, ρ_α = 0.0, Φ_1 = -55.0)
#τ_bequest = find_τ_l(ep_bequest, np, N = 10000, M = 20) 
τ_bequest = 0.1606

sol_bequest = solve(ep_bequest,np, τ_bequest)

(cohs_bequest,incomes_bequest,saves_bequest,conss_bequest,survives_bequest,death_now_bequest) = simulate(ep_bequest, np, sol_bequest, τ_bequest, M = 20, N = 10000)

sum(saves_bequest[:,:,end][survives_bequest[:,:,end]])/sum(incomes_bequest[:,:,end][survives_bequest[:,:,end]])

# wealth to income ratio - should be close to 3
# vary β to achieve this


# wealth distribution
wealthd_bequest = (saves_bequest[:,:,end][survives_bequest[:,:,end]])[:]

share_top(wealthd_bequest; perc = 0.01)
share_top(wealthd_bequest; perc = 0.05)
gini(sample(wealthd_bequest,10000))

share_at0(wealthd_bequest)

# how do bequests affect saving?

t = 70
αi = 6
zi = 6
gr = 0.0002:0.01:10

plot(gr,sol_nolink.cp[αi,t,zi].(gr), title = "Optimal consumption", label = "no bequest motive", xlabel = "Cash-on-hand (multiple of average yearly income)")
plot!(gr,sol_bequest.cp[αi,t,zi].(gr), label = "bequest motive")

plot(gr,sol_nolink.sp[αi,t,zi].(gr), title = "Optimal saving", label = "no bequest motive", xlabel = "Cash-on-hand (multiple of average yearly income)")
plot!(gr,sol_bequest.sp[αi,t,zi].(gr), label = "bequest motive")

# average bequest in bequest economy
mean(death_now_bequest[:,:,end] .* cohs_bequest[:,:,end])/mean(death_now_bequest[:,:,end])
# average bequest in nolink economy
mean(death_now_nolink[:,:,end] .* cohs_nolink[:,:,end])/mean(death_now_nolink[:,:,end])


plot(22:100,mean(conss_bequest[:,:,end], dims=1)', color=:blue, linewidth=2, label="consumption - bequest", xlabel = "Age", title = "Effect of bequest motive on means by age group", ylabel = "Multiple of yearly average income", legend = :topleft)
plot!(22:100,mean(saves_bequest[:,:,end], dims=1)', color=:red, linewidth=2, label="wealth - bequest", xlabel = "Age")
plot!(22:100,mean(conss_nolink[:,:,end], dims=1)', color=:blue, linewidth=2, linestyle = :dash, label="consumption - nolink", xlabel = "Age")
plot!(22:100,mean(saves_nolink[:,:,end], dims=1)', color=:red, linewidth=2, linestyle = :dash, label="wealth - nolink", xlabel = "Age")

# both bequest channel and ability inheritence is active

ep_both = EconPars(β = 0.9313, ρ_α = 0.6, Φ_1 = -55.0)
#τ_both = find_τ_l(ep_both, np, N = 10000, M = 20) 
τ_both = 0.1430

sol_both = solve(ep_both,np, τ_both)

(cohs_both,incomes_both,saves_both,conss_both,survives_both,death_now_both) = simulate(ep_both, np, sol_both, τ_both, M = 20, N = 10000)

sum(saves_both[:,:,end][survives_both[:,:,end]])/sum(incomes_both[:,:,end][survives_both[:,:,end]])
# wealth to income ratio - should be close to 3
# vary β to achieve this


# wealth distribution
wealthd_both = (saves_both[:,:,end][survives_both[:,:,end]])[:]

share_top(wealthd_both; perc = 0.01)
share_top(wealthd_both; perc = 0.05)
gini(sample(wealthd_both,10000))
share_at0(wealthd_both)

share_top(wealthd_both[wealthd_both.>0]; perc = 0.01)
share_top(wealthd_both[wealthd_both.>0]; perc = 0.05)
gini(sample(wealthd_both[wealthd_both.>0],10000))


# within age group inequalities
wealth_ginis_bequest = [gini(sample(saves_bequest[:,t,end][survives_bequest[:,t,end]],10000)) for t in axes(saves_bequest,2)]
wealth_ginis_both = [gini(sample(saves_both[:,t,end][survives_both[:,t,end]],10000)) for t in axes(saves_both,2)]

plot(22:100,wealth_ginis, color=:blue, linewidth=2, label="nolink", xlabel = "Age", title = "Gini coefficients of wealth within age groups")
plot!(22:100,wealth_ginis_bequest, color=:red, linestyle = :dash, linewidth=2, label="bequest")
plot!(22:100,wealth_ginis_both, color=:green, linestyle = :dot, linewidth=2, label="both")