####################################################
#
# Geburtstagsproblem
#
# file: prob_birthday.r
#
####################################################

# Tage im Jahr
no_of_days <- 365
days <- seq(1,no_of_days,1)

# Berchnung von P(B_n) und P(D_n)
p_dn <- cumprod(1-(days -1) / no_of_days)
p_bn <- 1-p_dn

# Bestimmung des Medians
q2 <- min(which(p_bn >= 0.5))

# Schaubild
par(mfrow=c(1,1))
plot(days,p_bn,type="l", xlim=c(0,100), xlab="n", ylab="P(B_n)",
     main="Birthday Problem", cex.sub=0.9,
     sub="Probability that in a class with n students at least two studente have the same birthday")
abline(h=0)
abline(v=0)
segments(0,p_bn[q2],q2,p_bn[q2])
segments(q2,p_bn[q2],q2,0)

# eps-file erzeugen
dev.copy2eps(file="../R_eps_files/birthday.eps")

# Ausgabetabelle für ein paar Werte von n
n <- c(2,10,20,q2,30,50,70,365)
tab <- cbind(n,p_dn[n],p_bn[n])
colnames(tab) <- c("n","P(D_n)","P(B_n)")

# tex-Tabelle erzeugen
library(xtable)
tex_table <- xtable(tab)
digits(tex_table) <- c(0,0,5,5)
align(tex_table) <- c("ll|l|l")
print(tex_table, floating=FALSE, include.rownames=FALSE, hline.after=c(0),
      file="../R_tex_files/birthday.tex")
