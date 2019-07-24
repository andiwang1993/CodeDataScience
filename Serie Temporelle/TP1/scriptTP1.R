#script du TP n��1, Series Temporelles, Majeure Science des Donnees 2017-18
#A completer

# Partie 1 : etude d'un MA(1)
# Modele X[t] = mu + Z[t] + theta*Z[t-1]
# Z[t] bruit blanc gaussien N(0,varZ)

mu <- 0	# moyenne du processus X[t]
theta <- 10   # parametre MA(1)
sigZ <- 1	# ecart-type du bruit Z[t]


# simulation d'un MA(1) de taille n
n <- 200

x <- rep(0,n) # initialisation de la serie x[t]

zinit <- sigZ*rnorm(1)  # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation de Z[1], ... , Z[n]

x[1] <- mu + z[1] + theta*zinit
for (t in 2:n) {

	x[t] <- mu + z[t] + theta*z[t-1]
}

# chronogramme de la serie simulee
plot(x, type='o', xlab="Temps t", main = "MA(1) simule")
abline(mu, 0, col="red", lwd=2)

# visualisation des auto-correcations de la serie simulee

k <- 4
m <- matrix(nr=n+k-1, nc=k)
colnames(m) <- c("x[t]", "x[t-1]", "x[t-2]","x[t-3]")
for (i in 1:k) {
  m[,i] <- c(rep(NA,i-1), x, rep(NA, k-i))
}

op <- par(mex=0.8)
pairs(m, gap=0, cex.labels=1.2, lower.panel=NULL, main="Visualisation des auto-corr�lations")
par(op)

# analyse graphique (chronogramme, acf)

op <- par(mfrow=c(2,1))
plot(x,type='l',xlab='time',ylab='')
abline(mu,0,col='red')
ro <- acf(x,10,main="Fonction d'autocorrelation empirique",ylim=c(-1,1))
par(op)

# Partie 2 : etude d'un AR(1)
# Modele X(t) - mu = phi*(X(t-1) - mu) + Z(t)
# Z[t] bruit blanc gaussien N(0,varZ)

mu <- 0                 # moyenne du processus X[t]
phi <- 1.1 	            # parametre AR(1) : phi = rho(1)

sigZ <-10			      # ecart-type du bruit Z[t]
zinit <- sigZ*rnorm(1)  # simulation de Z[0]
z <- sigZ*rnorm(n)      # simulation de Z[1], ... , Z[n]

sigX = sigZ/sqrt(1 - phi^2) # ecart-type de X[t]

# simulation AR(1) de taille n
n <- 500

x <- rep(0,n)
x[1] <- mu + sigX*rnorm(1)
for (t in 2:n) x[t] <- phi*(x[t-1]-mu)+z[t]

# chronogramme de la s�rie simulee
plot(x, type='o', xlab="Temps t", main = "AR(1) simul?")
abline(mu, 0, col="red", lwd=2)

# visualisation des auto-corr�lations de la s�rie simulee

k <- 4
m <- matrix(nr=n+k-1, nc=k)
colnames(m) <- c("x[t]", "x[t-1]", "x[t-2]","x[t-3]")
for (i in 1:k) {
  m[,i] <- c(rep(NA,i-1), x, rep(NA, k-i))
}

op <- par(mex=0.8)
pairs(m, gap=0, cex.labels=1.2, lower.panel=NULL, main="Visualisation des auto-corr�lations")
par(op)

# analyse graphique (chronogramme, acf)

op <- par(mfrow=c(2,1))
plot(x,type='l',xlab='time',ylab='')
abline(mu,0,col='red')
ro <- acf(x,10,main="Fonction d'autocorr�lation empirique",ylim=c(-1,1))
par(op)

# Partie 3 : �tude d'un AR(2)
# Mod�le X(t) - mu =  phi1*(X(t-1) - mu) + phi2*(X(t-2) - mu) + Z(t)
# Z[t] bruit blanc gaussien N(0,varZ)
# simulation d'un AR(1) par une phase initiale de stationarisation

# On travaille avec les inverses des racines du polyn�me P(z) = 1 - phi1*z - phi2*Z^2
# situees dans le disque unit? du plan complexe

# cas de deux racines inverses reelles dans ]-1, 1[
r1 <- 0.9
r2 <- 0.9
phi1 <- r1 + r2
phi2 <- - r1*r2			# parametres AR(2)

# cas de deux racines inverses complexes conjuguees de module r < 1
r <- 0.9
angle <- 60 			# en degres dans [0, 180]
phi1 <- 2*r*cos(angle*pi/180)
phi2 <- - r*r			# parametres AR(2)

mu <- 0				  # moyenne du processus X[t]
sigZ <- 1				# ecart-type du bruit Z[t]
