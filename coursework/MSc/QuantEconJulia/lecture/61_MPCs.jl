using Plots

include("31_infinite_horizon.jl")

ep = EconPars(β = 1/1.02)
np = NumPars()

sol = solve(ep,np)

gr = range(0,10,length = 5000)
plot(gr, sol.cp(gr), label = "Optimal consumption", xlabel = "cash-on-hand", linewidth = 2, title = "Consumption")

# average income
sum(ep.ys .* ep.pys)

1/ep.r
#
futinc = 1/(ep.r * (1+ep.r))

plot!(gr, coh -> ep.r * (futinc+coh), label = "Optimal consumption - no uncertainty", xlabel = "cash-on-hand", linewidth = 2)


tiny = 10^-7
plot(gr, coh -> (sol.cp(coh+tiny) - sol.cp(coh))/tiny, label = "MPC", xlabel = "cash-on-hand", linewidth = 2, title = "MPC")

hline!([ep.r], label = "MPC - no uncertainty", linewidth = 2)

