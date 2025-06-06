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
