########################################################
#
# Veranschaulichung des Gesetzes der gro?en Zahlen durch
# Werfen einer fairen Münze und Darstellung der Stabili-
# sierung der relativen Häufigkeiten
#
# file: prob_law_of_large_numbers.r
#
########################################################

# Anzahl Wiederholungen
n_of_rep <- 500

# Es werden 4 Simulationen ausgeführt und graphische dargestellt
par(mfrow=c(2,2))
for (i in 1:4) {
    # Zufallszahlen erzeugen
    data <- rbinom(n_of_rep,1,0.5)
    # x = Anzahl Wiederholungen
    x <- seq(1,n_of_rep,1)
    # rel. H?ufigkeit von Kopf bei n Wiederholungen
    y <- cumsum(data) / x
    plot(x,y,xlab="n",ylab="relative frequency of head",ylim=c(0,1))
    # theoretischer Wert anzeigen
    abline(h=0.5)
}

# eps-file erzeugen
dev.copy2eps(file="../R_eps_files/tossing_a_coin.eps")
