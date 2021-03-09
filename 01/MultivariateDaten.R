library(faraway)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scatterplot3d)
library(car)
data <- diabetes %>% as_tibble() %>% drop_na() %>%  select(id,stab.glu, chol, glyhb,height,weight ) %>% 
  mutate(stab.glu= as.double(stab.glu),chol=as.double(chol),glyhb=as.double(glyhb),height=as.double(height),weight=as.double(weight))
head(data)

#Kovarianzmatrix
covarianceMatrix <- cov(data[,c("stab.glu","chol","glyhb","height","weight")])
covarianceMatrix
heatmap(covarianceMatrix)

melted_covarianceMatrix <- melt(covarianceMatrix)
ggplot(data = melted_covarianceMatrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="black")


#Korelationsmatrix
corelationMatrix<-cor(data[,c("stab.glu","chol","glyhb","height","weight")])
corelationMatrix
heatmap(corelationMatrix)

melted_corelationMatrix <- melt(corelationMatrix)
ggplot(data = melted_corelationMatrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="black")


#Distanzmatrix 
dist(scale(data[,c("stab.glu","chol","glyhb","height","weight")]))
     

#Ausreißer
ggplot(stack(data), aes(x = ind, y = values)) +
  geom_boxplot()

ggplot(data, aes( y = data$glyhb)) +
  geom_boxplot()

boxplot(data$glyhb)


ggplot(data, aes(glyhb, stab.glu)) +
  geom_point() +
  geom_smooth(method = "lm")

attach(data)
scatterplot(stab.glu ~ glyhb)
scatterplotMatrix(data)
