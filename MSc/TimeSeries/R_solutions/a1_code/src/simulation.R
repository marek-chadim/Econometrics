#%%%%%% Simulating AR(2) %%%%%%%%

require(reshape2)

simulate <- function(a_0, a_2, y_init, niter) {
  
  A = diag(, nrow = 1, ncol = 2)
  A = rbind(c(0., a_2), A)
  B = c(a0, 0.)
  
  n = 25
  Y = matrix(, nrow = 2, ncol = niter);
  Y[,1] = y_init
  for (i in 1:(niter - 1)) {
    Y[ ,i + 1] = A %*% Y[ ,i] + B
  }
  
  return(Y)
}


niter = 20
Y_0s = simulate(a0,a2,c(y00,y00),niter)
Y_1s = simulate(a0,a2,c(y11,y01),niter)
Y_0l = simulate(a0,a22,c(y00,y00),niter)
Y_1l = simulate(a0,a22,c(y11,y01),niter)
simres = data.frame(iter = 1:niter, 
                     sim0s = Y_0s[1, ], 
                     sim1s = Y_1s[1, ],
                     sim0l = Y_0l[1, ],
                     sim1l = Y_1l[1, ])
simres = melt(simres, id = "iter")
grp1 = rep(c(rep(as.character(y00),niter),rep("3",niter)), 2)
grp2 = c(rep(as.character(a2),niter * 2), rep(as.character(a22), niter * 2))
simres$grp1 = grp1
simres$grp2 = grp2

# Export some numbers for tex integration
mu0 = a0 / (1 - a2)
digit_to_dat(mu0,"./numbers/mu0")
mu1 = a0 / (1 - a22)
digit_to_dat(mu1,"./numbers/mu1")


gplot = ggplot(simres, aes(color=grp2, linetype = grp1, x=iter, y=value)) + 
  geom_line(stat = "identity") + geom_point(stat = "identity") +
  geom_hline(yintercept = a0 / (1 - a2), linetype = "dashed") +
  geom_hline(yintercept = a0 / (1 - a22), linetype = "dashed") +
  graph_settings +
  labs(x = TeX(r"(Time $t$)"), 
       y = TeX(r"($y_t$)")) +
  labs(color = TeX(r"($a_2$)"), linetype = TeX(r"($y_0$)"))

if (export) {
ggsave("./figures/A1_01_simulation.pdf",  plot = gplot, dpi = 300,
                                height = h, width = h * aspect_ratio)
}