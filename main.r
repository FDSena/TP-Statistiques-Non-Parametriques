# Crée le dossier "img" s'il n'existe pas
if (!dir.exists("img")) dir.create("img")

# Ouvre un fichier PNG dans le dossier img
png("img/exo1_q1_repartition.png", width = 800, height = 600)

# ----- Code du graphique -----
set.seed(123)
echantillon1 <- rexp(n = 20, rate = 1)

plot(ecdf(echantillon1),
     main = "Fonction de répartition : Empirique vs Théorique",
     xlab = "x", ylab = "F(x)",
     col = "blue", lwd = 2)

curve(pexp(x, rate = 1), col = "red", lwd = 2, add = TRUE)

legend("bottomright", legend = c("Empirique", "Théorique"),
       col = c("blue", "red"), lwd = 2)

# ----- Fin du graphique -----

# Ferme le fichier PNG
dev.off()

#----------------------------------------

# Exercice 1 - Question 2 : Densité estimée vs densité théorique

# Crée le dossier "img" s'il n'existe pas
if (!dir.exists("img")) dir.create("img")

# Ouvre un fichier PNG pour sauvegarder le graphique
png("img/exo1_q2_densite.png", width = 800, height = 600)

# Génère l’échantillon (le même que précédemment)
set.seed(123)
echantillon1 <- rexp(n = 20, rate = 1)

# Trace la densité empirique
plot(density(echantillon1),
     main = "Densité estimée vs Densité théorique",
     col = "blue", lwd = 2,
     xlab = "x", ylab = "Densité")

# Ajoute la densité théorique (de la loi exponentielle)
curve(dexp(x, rate = 1), col = "red", lwd = 2, add = TRUE)

# Légende
legend("topright", legend = c("Densité estimée", "Densité théorique"),
       col = c("blue", "red"), lwd = 2)

# Ferme le fichier PNG
dev.off()

#----------------------------------------
# Exercice 1 - Question 3 : Cet échantillon est-il gaussien ?

# Crée le dossier "img" s'il n'existe pas
if (!dir.exists("img")) dir.create("img")

# Ouvre un fichier PNG pour sauvegarder le graphique
png("img/exo1_q3_gaussianite.png", width = 800, height = 600)

# Définir le nouvel échantillon
echantillon2 <- c(1.86, 1.99, 0.43, 1.95, 2.09,
                  1.81, 0.54, 0.62, 0.10, 0.13,
                  0.79, 1.89, 0.13, 0.79, 0.04,
                  0.04, 0.82, 2.00, 0.34, 1.10)

# Histogramme + densité estimée
hist(echantillon2, probability = TRUE,
     main = "Histogramme de l'échantillon 2",
     col = "lightgray", xlab = "x")
lines(density(echantillon2), col = "blue", lwd = 2)

# Courbe normale théorique (même moyenne/écart-type que l’échantillon)
curve(dnorm(x, mean=mean(echantillon2), sd=sd(echantillon2)),
      col = "red", lwd = 2, add = TRUE)

legend("topright", legend = c("Densité estimée", "Normale théorique"),
       col = c("blue", "red"), lwd = 2)

# Fermer le fichier PNG
dev.off()

# Afficher les résultats des tests de normalité

# Test de Shapiro-Wilk
shapiro_result <- shapiro.test(echantillon2)
print(shapiro_result)

# Test d’Anderson-Darling (nécessite le package nortest)
if (!require(nortest)) install.packages("nortest")
library(nortest)
ad_result <- ad.test(echantillon2)
print(ad_result)

#----------------------------------------
# Question 4 : Comparer les deux échantillons (1 et 2) avec un test non paramétrique

# Test de Kolmogorov-Smirnov (non paramétrique)
ks.test(echantillon1, echantillon2)

# Test de Wilcoxon (pour comparer les médianes)
wilcox.test(echantillon1, echantillon2)

#----------------------------------------
# Question 5 : Créer Z = min(X, Y) et le comparer à sa loi théorique

if (!dir.exists("img")) dir.create("img")

z <- pmin(echantillon1, echantillon2)

# Sauvegarder le graphique
png("img/exo1_q5_min_density.png", width = 800, height = 600)

# Densité empirique vs théorique (exp(2))
plot(density(z), col = "blue", lwd = 2,
     main = "Densité de Z = min(X, Y) vs Exp(2)",
     xlab = "z", ylab = "Densité")
curve(dexp(x, rate = 2), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Densité de Z", "Exp(2) théorique"),
       col = c("blue", "red"), lwd = 2)

dev.off()

# Test de Kolmogorov-Smirnov entre z et Exp(2)
ks.test(z, "pexp", rate = 2)

#----------------------------------------
#Question 6 : Comparer la 1ère ligne et la 2e ligne

ligne1 <- c(1.86, 1.99, 0.43, 1.95, 2.09,
            1.81, 0.54, 0.62, 0.10, 0.13)

ligne2 <- c(0.79, 1.89, 0.13, 0.79, 0.04,
            0.04, 0.82, 2.00, 0.34, 1.10)

# Test de Kolmogorov-Smirnov
ks.test(ligne1, ligne2)

# Test de Wilcoxon
wilcox.test(ligne1, ligne2)

#-----------------------------------------
# Exercice 2 : Une densité approchée

#Question 1 : Visualisation brute de la densité

# Crée le dossier "img" s'il n'existe pas
if (!dir.exists("img")) dir.create("img")

# Sauvegarde du graphique
png("img/exo2_q1_histogramme.png", width = 800, height = 600)
hist(echantillon2, probability = TRUE, col = "lightgray",
     main = "Visualisation brute de la densité",
     xlab = "x", ylab = "Densité")
dev.off()

#Question 2 – Noyau Epanechnikov avec deux valeurs de bw
# Noyau Epanechnikov sans préciser bw
png("img/exo2_q2_epanechnikov_default.png", width = 800, height = 600)
hist(echantillon2, probability = TRUE, col = "lightgray",
     main = "Densité avec noyau Epanechnikov (par défaut vs bw = 0.2)",
     xlab = "x", ylab = "Densité")

lines(density(echantillon2, kernel = "epanechnikov"), col = "blue", lwd = 2)
lines(density(echantillon2, kernel = "epanechnikov", bw = 0.2), col = "red", lwd = 2)

legend("topright", legend = c("bw par défaut", "bw = 0.2"),
       col = c("blue", "red"), lwd = 2)
dev.off()

#Question 3 – Même graphique avec noyau gaussien
png("img/exo2_q3_gaussian_bw02.png", width = 800, height = 600)
hist(echantillon2, probability = TRUE, col = "lightgray",
     main = "Densité avec noyau Gaussien vs Epanechnikov (bw = 0.2)",
     xlab = "x", ylab = "Densité")

lines(density(echantillon2, kernel = "epanechnikov", bw = 0.2), col = "red", lwd = 2)
lines(density(echantillon2, kernel = "gaussian", bw = 0.2), col = "blue", lwd = 2)

legend("topright", legend = c("Epanechnikov (bw=0.2)", "Gaussien (bw=0.2)"),
       col = c("red", "blue"), lwd = 2)
dev.off()


# Exercice 3 - Densité
# Question 1 : Afficher un histogramme repr´esentant bien l’´echantillon (histogramme en fr´equence relative).
# Crée le dossier "img" s'il n'existe pas
if (!dir.exists("img")) dir.create("img")

# Échantillon de l’exercice 3
echantillon3 <- c(-2.26,  2.08, -0.01, -2.04, -3.07,
                  -4.52,  0.65, -0.67, -0.86, -0.22,
                   0.52,  0.63,  2.69,  0.27, -0.55,
                   0.66, -0.06, -4.39,  0.15, -0.84)

png("img/exo3_q1_histogramme.png", width = 800, height = 600)
hist(echantillon3, probability = TRUE, col = "lightgray",
     main = "Histogramme de l'échantillon 3",
     xlab = "x", ylab = "Densité")
dev.off()

# Question 2 :Sur le même graphique afficher la densit´e calcul´ee par la commande ”density()”.

png("img/exo3_q2_hist_density.png", width = 800, height = 600)
hist(echantillon3, probability = TRUE, col = "lightgray",
     main = "Histogramme + densité estimée (échantillon 3)",
     xlab = "x", ylab = "Densité")
lines(density(echantillon3), col = "blue", lwd = 2)
legend("topright", legend = "Densité estimée", col = "blue", lwd = 2)
dev.off()

# Question 3 : Densité estimée avec noyau Epanechnikov et bw = 0.2
# Shapiro-Wilk
shapiro.test(echantillon3)

# Anderson-Darling
if (!require(nortest)) install.packages("nortest")
library(nortest)
ad.test(echantillon3)

#Question 4 : # Générer un échantillon normal centrée réduite
set.seed(123)
gaussienne <- rnorm(20, mean = 0, sd = 1)

# Somme terme à terme
Z <- echantillon3 + gaussienne

# Histogramme + densité
png("img/exo3_q4_somme_normale.png", width = 800, height = 600)
hist(Z, probability = TRUE, col = "lightgray",
     main = "Z = X + N(0,1) : Histogramme + densité",
     xlab = "z", ylab = "Densité")
lines(density(Z), col = "blue", lwd = 2)
curve(dnorm(x, mean = mean(Z), sd = sd(Z)), col = "red", lwd = 2, add = TRUE)
legend("topright", legend = c("Densité estimée", "Normale théorique"),
       col = c("blue", "red"), lwd = 2)
dev.off()

# Question 5 : Teste de normalité

# Shapiro-Wilk
shapiro.test(Z)

# Anderson-Darling
ad.test(Z)