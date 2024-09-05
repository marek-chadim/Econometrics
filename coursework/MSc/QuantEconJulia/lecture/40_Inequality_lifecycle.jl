using Plots
using Statistics

pwd()

#cd("./julia/classes")

include("40_lifecycle_solve.jl")

# numerical parameters
np = NumPars()

# benchmark
ep = EconPars()

# no lifecycle risk
ep_nolcrisk = EconPars(σ_η = 10^-7, σ_ϵ = 10^-7)

# no risk in fixed effects
ep_noalpharisk = EconPars(σ_α = 10^-7)

# low pension
ep_lowB = EconPars(B = 0.01) 

# low wealth
ep_loww = EconPars(β = 0.933) 

sol = solve(ep,np)
sol_nolcrisk = solve(ep_nolcrisk,np)
sol_noalpharisk = solve(ep_noalpharisk,np)
sol_lowB = solve(ep_lowB,np)
sol_loww = solve(ep_loww,np)

t = 1
αi = 4
zi = 9
gr = 0.02:0.01:10

plot(gr,sol.cp[αi,t,zi].(gr), title = "Optimal consumption", label = "benchmark", xlabel = "Cash-on-hand (multiple of average yearly income)")
plot!(gr,sol_nolcrisk.cp[αi,t,zi].(gr), label = "no life-cycle risk")


plot(gr,sol.vf[αi,t,zi].(gr), title = "Value function", label = "benchmark", xlabel = "Cash-on-hand (multiple of average yearly income)")
plot!(gr,sol_nolcrisk.vf[αi,t,zi].(gr), label = "no life-cycle risk")
plot!(xlim = xlims(), ylim = ylims())

# more grid points, where there is more action
knot = collect(knots(sol.vf[αi,t,zi]))
scatter!(knot,sol.vf[αi,t,zi].(knot), label = "grid points used for interpolation")
knot_nolcrisk = collect(knots(sol_nolcrisk.vf[αi,t,zi]))
scatter!(knot_nolcrisk,sol_nolcrisk.vf[αi,t,zi].(knot_nolcrisk), label = "grid points used for interpolation")

(cohs,incomes,saves,conss,survives) = simulate(ep, np, sol, 10000)

(cohs_lowB,incomes_lowB,saves_lowB,conss_lowB,survives_lowB) = simulate(ep_lowB, np, sol_lowB, 10000)

(cohs_loww,incomes_loww,saves_loww,conss_loww,survives_loww) = simulate(ep_loww, np, sol_loww, 10000)

plot(22:100,mean(conss, dims=1)', color=:blue, linewidth=2, label="consumption", xlabel = "Age")
plot!(22:100,mean(saves, dims=1)', color=:red, linewidth=2, label="wealth")
plot!(22:100,mean(incomes, dims=1)', color=:green, linewidth=2, label="income")

# compare with Fig 5. is STY
plot(22:100,var(log.(incomes), dims = 1)', color=:green, linewidth=2, label="income", title = "Variance of logs")
plot!(22:100,var(log.(conss), dims = 1)', color=:blue, linewidth=2, label="consumption")
plot!(22:100,var(log.(conss_lowB), dims = 1)', color=:blue, linewidth=2, linestyle = :dash, label="consumption - low pension")
plot!(22:100,var(log.(conss_loww), dims = 1)', color=:blue, linewidth=2, linestyle = :dot, label="consumption - low wealth")

# compute expected value of someone starting with no wealth, z = 0. draw alpha and epsilon. this is what happens in Section 5 of STY
function expval(ep::EconPars,np::NumPars,sol::Solution)
    @unpack_EconPars ep
    @unpack_NumPars np
    Ybar = mean_income(ep)
    ϵ_grid, ϵ_ps = discretize_normal(0, σ_ϵ, N_ϵ)
    α_grid, α_ps = discretize_normal(0, σ_α, N_α)
    midindz = round(Int64, (N_z+1)/2)
    expv = 0.0
    for αi in 1:N_α
        for ϵi in 1:N_ϵ
            coh = exp(α_grid[αi] + g + ks[1] + ϵ_grid[ϵi])/Ybar
            expv = expv + ϵ_ps[ϵi] * α_ps[αi] * sol.vf[αi,1,midindz](coh)
        end
    end
    return expv
end

# 2 is risk aversion in both models
(expval(ep_nolcrisk,np,sol_nolcrisk)/expval(ep,np,sol))^(1/(1-2)) # eliminating risk over the life-cycle is equivalent to increasing consumption by 23.3%

(expval(ep_noalpharisk,np,sol_nolcrisk)/expval(ep,np,sol))^(1/(1-2)) # eliminating risk at the start is equivalent to increasing consumption by 26.6%

