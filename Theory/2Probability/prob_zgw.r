####################################################
#
# Veranschaulichung des ZGW
#
# file: prob_zgw.r
#
####################################################

# Augenzahlen von n Würfeln
n <- 50

# Die Matrix Y enthält in der Zeile i die rekursiv bestimmten
# Wahrscheinlichkeitsdichte von Y_i
Y <- matrix(rep(0,n*n*6),nrow=n, ncol=n*6)
Y[1,c(1:6)] <- rep(1,6) / 6
for (j in 2:n) {
    for (i in j:(6*j)) {
        Y[j,i] <- sum(Y[j-1,c(max(i-6,1):(i-1))]) / 6
    }
}

# Vektor der Summe Augenzahlen
X <- seq(1,6*n)

# Graphik der Dichten von Y_n
par(mfrow=c(2,3))
plot(c(0,X),c(0,Y[1,]), type="s", xlab="sum of numbers",
     ylab="probability",  main="Throwing 1 fair die",xlim=c(0,10),
     ylim=c(0,0.18), tck=0.01)
for ( i in c(2,3,10,15,20)) {
    plot(X,Y[i,], type="s", xlab="sum of numbers", ylab="probability",
         main=paste("Throwing ",i," fair dice"),xlim=c(0,min(6*i+10,100)),
         ylim=c(0,0.18), tck=0.01, lab=c(20,20,12))
}

# eps-file erzeugen
dev.copy2eps(file="../R_eps_files/zgw_1.eps")

# Erwartungswert und Varianz von Y_n
EWERT <- seq(1,n,1) * 3.5
VAR <- seq(1,n,1) * 35 / 12

# Dichte der N(0,1)-Verteilung
f <- function(x) {
    return(exp(-0.5 * x^2) / sqrt(2*pi))
}

# Standardisierung von Y_n
par(mfrow=c(2,3))
plot((c(0,X) - EWERT[1])/sqrt(VAR[1]),c(0,Y[1,]),
     type="s", xlab="sum of numbers",
     ylab="probability",  main="Throwing 1 fair die",
     xlim=c(-4,4), ylim=c(0,0.42))
plot(f,-4,4, add=TRUE, col="red")
for ( i in c(2,3,10,20,30)) {
    plot((X-EWERT[i])/sqrt(VAR[i]),Y[i,] * sqrt(VAR[i]),
         type="s", xlab="sum of numbers", ylab="probability",
         main=paste("Throwing ",i," fair dice"),
         xlim=c(-4,4), ylim=c(0,0.42))
    plot(f,-4,4, add=TRUE, col="red")
}

# eps-file erzeugen
dev.copy2eps(file="../R_eps_files/zgw_2.eps")

# Stetigkeitskorrektur
par(mfrow=c(2,1))
plot((X-EWERT[30])/sqrt(VAR[30]),Y[30,] * sqrt(VAR[30]),
     type="s", xlab="sum of numbers", ylab="probability",
     main=paste("Throwing ",30," fair dice"),
     sub="without continuity correction",
     xlim=c(-4,4), ylim=c(0,0.42))
plot(f,-4,4, add=TRUE, col="red")
plot((X-0.5-EWERT[30])/sqrt(VAR[30]),Y[30,] * sqrt(VAR[30]),
     type="s", xlab="sum of numbers", ylab="probability",
     main=paste("Throwing ",30," fair dice"),
     sub="with continuity correction",
     xlim=c(-4,4), ylim=c(0,0.42))
plot(f,-4,4, add=TRUE, xol="red")

# eps-file erzeugen
dev.copy2eps(file="../R_eps_files/zgw_3.eps")

# Berechnung von exakten und approx. (mit und ohne Korrektur)
# Wahrscheinlichkeiten P(a < Y_n <= b)

a <- floor(EWERT - sqrt(VAR))   # n?chst kleinere ganze Zahl
b <- ceiling(EWERT + sqrt(VAR)) # n?chst gr??ere ganze Zahl

# Abspeicherung der Werte in einer Tabelle
res_tab <- c()
for (i in c(1,2,3,10,20,30,50)) {
    # exakte Werte
    pex <- sum(Y[i,(a[i]+1):b[i]])
    papp <- pnorm((b[i]-EWERT[i])/sqrt(VAR[i]),0,1) -
        pnorm((a[i]-EWERT[i])/sqrt(VAR[i]),0,1)
    papp0 <- pnorm((b[i]+0.5-EWERT[i])/sqrt(VAR[i]),0,1) -
        pnorm((a[i]+0.5-EWERT[i])/sqrt(VAR[i]),0,1)
#    cat("\n \n i: ",i," a=",a[i]," b=",b[i]," ex: ", pex," papp: ",
#        papp," papp0: ", papp0)
    res_tab <-  rbind(res_tab,c(i,a[i],b[i],pex,papp,papp0))
}

# Spaltennamen setzen
colnames(res_tab) <- c("n","a","b","exact","appr. without corr.",
                       "appr. wiht corr.")

# tex-Tabelle erzeugen
library(xtable)
tex_tab <- xtable(res_tab)
digits(tex_tab) <- c(0,0,0,0,5,5,5)
align(tex_tab) <- c("ll|l|l|c|c|c")
print(tex_tab, floating=FALSE, hline.after=c(0), include.rownames=FALSE,
      file="../R_tex_files/zgw_dice.tex")
