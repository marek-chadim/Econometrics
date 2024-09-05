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

#cd("julia/classes")
include("21_borrowinglims_solve.jl")

function solve_simul(ep::EconPars, np::NumPars; N=10000)
    sol = solve(ep, np)

    (coh_sim, y_sim) = simulate(ep, sol, N)
    cons_sim = hcat([sol.cp[t].(coh_sim[:, t]) for t in 1:ep.T]...)
    save_sim = hcat([sol.sp[t].(coh_sim[:, t]) for t in 1:ep.T]...)

    plot(mean(cons_sim, dims=1)', color=:blue, linewidth=2, label="consumption")
    plot!(mean(save_sim, dims=1)', color=:red, linewidth=2, label="wealth")
    plot!(mean(y_sim, dims=1)', color=:green, linewidth=2, label="income")
end

np = NumPars(max_coh = 10.0, N_coh = 500)

# no uncertainty

# no uncertainty and (1+r)β = 1 --> flat consumption profile, no savings 
solve_simul(
    EconPars(ys = [1.0], pys = [1.0], bl = 3.0, β = 1/(1+0.02)),
    np)

# no uncertainty and (1+r)β > 1 --> postpone consumption, savings is + 
solve_simul(
    EconPars(ys = [1.0], pys = [1.0], bl = 3.0, β = 0.99),
    np)

# no uncertainty and (1+r)β < 1 --> move consumption forward, savings is - 
solve_simul(
    EconPars(ys = [1.0], pys = [1.0], bl = 3.0, β = 0.97),
    np)

# even lower β --> same, but hitting borrowing limit    
solve_simul(
    EconPars(ys = [1.0], pys = [1.0], bl = 3.0, β = 0.9),
    np)


# quadratic utility

np = NumPars(max_coh = 20.0, N_coh = 1000)

quad_determ = EconPars(u = quadratic_utility_function(50.0),ys = [1.0], pys = [1.0], bl = 15.0, β = 0.98)

quad_stoch = EconPars(u = quadratic_utility_function(50.0),ys = [0.8, 1.2], pys = [0.5, 0.5], bl = 15.0, β = 0.98)

solve_simul(quad_determ,np)

solve_simul(quad_stoch,np)


function solve_simul2(ep::EconPars, ep2::EconPars, np::NumPars; N=10000)
    sol = solve(ep, np)
    sol2 = solve(ep2, np)

    (coh_sim, y_sim) = simulate(ep, sol, N)
    cons_sim = hcat([sol.cp[t].(coh_sim[:, t]) for t in 1:ep.T]...)
    save_sim = hcat([sol.sp[t].(coh_sim[:, t]) for t in 1:ep.T]...)

    (coh_sim2, y_sim2) = simulate(ep2, sol2, N)
    cons_sim2 = hcat([sol2.cp[t].(coh_sim2[:, t]) for t in 1:ep2.T]...)
    save_sim2 = hcat([sol2.sp[t].(coh_sim2[:, t]) for t in 1:ep2.T]...)

    f1 = plot(mean(cons_sim, dims=1)', color=:blue, linewidth=2, label="consumption in model 1")
    f1 = plot!(mean(cons_sim2, dims=1)', color=:blue, linestyle = :dash, linewidth=2, label="consumption in model 2")
    f1 = plot!(mean(y_sim, dims=1)', color=:green, linewidth=2, label="income in model 1")
    f1 = plot!(mean(y_sim2, dims=1)', color=:green, linestyle = :dash,linewidth=2, label="income in model 2")
    display(f1)

    f2 = plot(mean(save_sim, dims=1)', color=:red, linewidth=2, label="wealth in model 1")
    f2 = plot!(mean(save_sim2, dims=1)', color=:red, linestyle = :dash, linewidth=2, label="wealth in model 2")
    display(f2)
end

solve_simul2(quad_determ,quad_stoch,np)
# different only due to numerical errors and not allowing negative consumption!



crra_determ = EconPars(u = crra_utility_function(5.0),ys = [1.0], pys = [1.0], bl = 10.0, β = 0.93)

crra_stoch = EconPars(u = crra_utility_function(5.0),ys = [0.8, 1.2], pys = [0.5, 0.5], bl = 10.0, β = 0.93)

# with CRRA utility, saving is higher and consumption is lower in uncertain case
solve_simul2(crra_determ,crra_stoch,np)

sol_determ = solve(crra_determ, np)
sol_stoch = solve(crra_stoch, np)

t = 1
grid = -10:0.01:-5

plot(grid,sol_determ.cp[t].(grid), label = "optimal consumption in deterministic model")
plot!(grid,sol_stoch.cp[t].(grid), label = "optimal consumption under uncertainty")
plot!((0:0.1:1.2)  .- crra_determ.bl,0:0.1:1.2, label = "45° line - consume everything", linestyle = :dash, xlabel = "Cash-on-hand")

plot(grid,sol_determ.sp[t].(grid), label = "optimal saving in deterministic model")
plot!(grid,sol_stoch.sp[t].(grid), label = "optimal saving under uncertainty")


plot(grid,sol_determ.sp[t].(grid)*(1+crra_determ.r) .+ 1 .- grid, label = "E_t[coh_t+1] - coh_t in deterministic model")
plot!(grid,sol_stoch.sp[t].(grid)*(1+crra_stoch.r) .+ 1 .- grid, label = "E_t[coh_t+1] - coh_t under uncertainty")

(coh_sim, y_sim) = simulate(crra_stoch, sol_stoch, 10000)
cons_sim = hcat([sol_stoch.cp[t].(coh_sim[:, t]) for t in 1:crra_stoch.T]...)
save_sim = hcat([sol_stoch.sp[t].(coh_sim[:, t]) for t in 1:crra_stoch.T]...)

plot(var(cons_sim,dims = 1)', label = "Cross sectional variance of consumption")
plot!(var(y_sim,dims = 1)', label = "Cross sectional variance of income")
