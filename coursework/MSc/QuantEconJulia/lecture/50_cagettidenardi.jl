using Plots
using Statistics
using NLsolve

pwd()

# cd("./julia/classes")

include("50_cagettidenardi_solve.jl")
# experiment


np = NumPars(max_asset = 50000.0, N_asset = 300)
 
ep = EconPars()
r = 0.065
β = 0.92
τ_l = 0.15

sol = solve(ep, np, r, β, τ_l)

ag = 30 .* range(10^-7,1,length=np.N_asset) .^ np.grid_curvature

plot(ag,sol.vfR.(ag), color=:blue, linewidth=2, label="Retired", xlabel = "Assets", title = "Value functions")
plot!(ag,sol.vfO[1].(ag), color=:red, linewidth=2, label="Old - low theta")
plot!(ag,sol.vfO[2].(ag), color=:green, linewidth=2, label="Old - high theta")

plot(ag,sol.vfY[1,3].(ag), color=:blue, linewidth=2, label="Young - low theta - mid y", xlabel = "Assets", title = "Value functions")
plot!(ag,sol.vfY[2,3].(ag), color=:red, linewidth=2, label="Young - high theta - mid y")
plot!(ag,sol.vfY[1,5].(ag), color=:green, linewidth=2, label="Young - low theta - high y")

plot(ag,sol.cpR.(ag), color=:blue, linewidth=2, label="Retired", xlabel = "Assets", title = "Optimal consumption")
plot!(ag,sol.cpO[1].(ag), color=:red, linewidth=2, label="Old - low theta")
plot!(ag,sol.cpO[2].(ag), color=:green, linewidth=2, label="Old - high theta")
plot!(ylims = [-0.1,ylims()[2]])

plot!(ag,sol.kpO[2].(ag), color=:green, linestyle = :dash, linewidth=2, label="k: Old - high theta")

(olds, yis, θis, ks, cs, as) = simulate(ep, np, sol)

# far from GE
aggregate_quantities(ep,olds, yis, θis, ks, as)


plot(olds[1:5,1:100]', label = "")

# burn in
plot(mean(olds,dims = 1)')
plot(mean(ks,dims = 1)')
plot(mean(as,dims = 1)')

aslast = as[:,end]
gini(aslast)
share_at0(aslast)
share_top(aslast)

kslast = ks[ks[:,end].>0,end]

gini(kslast)
mean(kslast)
share_top(kslast)
histogram(kslast)


# this is how to solve for GE
calibpars = nlsolve(x -> GE_difference(x,ep,np),  [0.065,0.95,0.1] )

