# Aufgabe: 
# 1. Installieren Sie das "faraway"-Package und laden Sie den Datensatz "diabetes"!
# 2. Wählen Sie 5 metrische Variablen aus und führen Sie mit den oben vorgestellten Methoden eine
#    deskriptive Analyse durch!

rm(list = ls())
library(rgl)
if(!require(faraway)) install.packages("faraway"); library(faraway)
if(!require(corrplot)) install.packages("corrplot"); library(corrplot)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(MVA)) {install.packages("MVA"); library(MVA)}
if(!require(mvtnorm)) {install.packages("mvtnorm"); library(mvtnorm)}
if(!require(KernSmooth)) {install.packages("KernSmooth"); library(KernSmooth)}
if(!require(scatterplot3d)) {install.packages("scatterplot3d"); library(scatterplot3d)}
if(!require(aplpack)) {install.packages("aplpack"); library(aplpack)}
if(!require(heplots)) {install.packages("heplots"); library(heplots)}

help(diabetes) # Alles über 7 glyhb ist diabetes
data <- diabetes
data <- data[complete.cases(data["glyhb"]), ]
data <-`row.names<-`(data[-1], data$id)

# 13 African Americans haben keinen glyhb Wert

attach(data)
Boxplot(~ glyhb) # Boxplot zeigt sehr schön, dass alles über 7 ausreißer sind, dekt sich mit alles über 7 ist diabetes
outliers = boxplot(data$glyhb, plot=FALSE)$out
(nrow(data[data$glyhb %in% outliers,]) / nrow(data)) * 100 # womöglich 14% der getesteten Leute haben diabetes
outliers<-data[data$glyhb %in% outliers,]
outliers[order(outliers$glyhb, decreasing = TRUE), ]


# Körper zu Glycosolated hemoglobin höher weight heißt gleich Diabetes zb.
# Kovarianzmatrix:
#cov(data[, c("height", "weight", "glyhb", "age", "waist")], use="pairwise.complete.obs")

# Korrelationsmatrix
cor(data[, c("height", "weight", "glyhb", "age", "waist")], use="pairwise.complete.obs")

# Distanzmatrix:
#dist(scale(data[, c("height", "weight", "glyhb", "age", "waist")], center = FALSE))

#Körpermaße dürfte kein Indikator für Diabetes sein.

# Körpermaße
# Ich dachte hip in Inches ist die Höhe darum Korrelation mit height aber es die hip breite daher die Korrelation mit waist and weight
# Kovarianzmatrix:
#cov(data[, c("height", "weight", "hip", "age", "waist")], use="pairwise.complete.obs")

# Korrelationsmatrix
cor(data[, c("height", "weight", "hip", "age", "waist")], use="pairwise.complete.obs")

# Distanzmatrix:
#dist(scale(data[, c("height", "weight", "hip", "age", "waist")], center = FALSE))


# Blutdruck zu Glycosolated
# Kovarianzmatrix:
#cov(data[, c("bp.1s", "bp.1d", "glyhb", "bp.2s", "bp.2d")], use="pairwise.complete.obs")

# Korrelationsmatrix
cor(data[, c("bp.1s", "bp.1d", "glyhb", "bp.2s", "bp.2d")], use="pairwise.complete.obs")

# Distanzmatrix:
#dist(scale(data[, c("bp.1s", "bp.1d", "glyhb", "bp.2s", "bp.2d")], center = FALSE))

#Restliche Werte zu Glycosolated hemoglobin
# Kovarianzmatrix:
#cov(data[, c("chol", "stab.glu", "glyhb", "hdl", "ratio")], use="pairwise.complete.obs")

# Korrelationsmatrix
cor(diabetes[, c("chol", "stab.glu", "glyhb", "hdl", "ratio")], use="pairwise.complete.obs")

# Distanzmatrix:
#dist(scale(data[, c("chol", "stab.glu", "glyhb", "hdl", "ratio")], center = FALSE))

#stab.glu könnte mit Glycosolated Hemoglobin und damit mit Diabetes zusammenhängen.

scatterplot(stab.glu ~ glyhb)

# Waist könnte mit Gewicht und Hüfte zusammenhängen
scatterplot(waist ~ weight)
scatterplot(waist ~ hip)

# Weight zeigt keine Tendenz zu einer Zusammenhang zu Glycosolated Hemoglobin
scatterplot(weight ~ glyhb)

# Grafische Darstellung von waist und hip
ggplot(data, aes(x = waist, y = hip)) + # Scatterplot (ggplot2-Package-Version)
  geom_point(shape = 1) +    # Kreise verwenden
  geom_smooth(method = lm)
# Hip größe hängt auch mit waist größe zusammen.

# Grafische Darstellung von weight zu Glycosolated Hemoglobin
ggplot(data, aes(x = weight, y = glyhb)) +
  geom_point(shape = 1) +  
  geom_smooth()
# Gewicht zeigt keine Korrelation mit Glycosolated Hemoglobin

# Grafische Darstellung ob das Alter Diabetes fördert
ggplot(data, aes(x = age, y = glyhb)) +
  geom_point(shape = 1) +  
  geom_smooth()
# Kein signifikanter zusammenhang zwischen Alter und Diabetes

# Zusammenhang zwischen Glycosolated Hemoglobin und Stabilized Glucose
scatterplot(glyhb ~ stab.glu)
# Alles unter etwa 110 und 6 dürfte "Normal" sein.
# Nochmals als Matrix
scatterplotMatrix(~ glyhb + stab.glu)
# Nochmals als smooth
smoothScatter(glyhb, stab.glu)

# MIt einer Einkreisung 
#as.matrix(diabetes)
x <- as.data.frame(data)
x <- x[complete.cases(data), c("stab.glu", "glyhb")]
outdiabetes <- match(lab <- rownames(outliers), rownames(x))
bvbox(x, mtitle = "", xlab = "Stabilized Glucose", ylab = "Glycosolated Hemoglobin")
text(x$stab.glu[outdiabetes], x$glyhb[outdiabetes], labels = lab,
     cex = 0.7, pos = c(2, 2, 4, 2, 2))


# Eine Scatterplott Matrix zwischen Körpergrößen und Alter
scatterplotMatrix(~ height + weight + hip + age + waist)
# Das Alter dürfte keinen großen EInfluss haben auf Körpergrößen
# Gewicht Taille und Hip hängen zusammen


#Grafiken die den ZUsammenhang zwischen stab.glu und glyhb, bzw. "hip", "weight", "waist" zeigen
cor.diabetes <- cor(data[, c("stab.glu", "glyhb", "hip", "weight", "waist")], use="pairwise.complete.obs")
corrplot(cor.diabetes, method = "ellipse")
corrplot(cor.diabetes, method = "number", type = "lower")

# Verstehe ich noch nicht ganz...
with(data, chiplot(stab.glu, glyhb))
