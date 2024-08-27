library(dplyr)
library(esquisse)
library(caret)
glimpse(titanic2)
titanic2$Sobreviveu <- factor(titanic2$Sobreviveu)
glimpse(titanic2)

titanic2$clase <- factor(titanic2$Classe)

#analise da associação entre sexo e sobrevivencia 
esquisser(titanic2)

#analise da associação entre tarifa sobrevivencia 
titanic2 %>% group_by(Sobreviveu) %>% summarise(media = mean(Tarifa))

#analise a associação entre idade esobrevivencia
titanic2 %>% group_by(Sobreviveu) %>% summarise(media = mean(Idade))

#analise a associação entre classe e sobrevivencia
esquisser(titanic2)

########################################################

indices <- sample(1:891, 712, replace = FALSE)

treino <- titanic2[indices,]
teste <- titanic2[-indices,]

# eu morreria? 
eu <- data.frame(Classe = "3", Sexo = "famle", Idade = 19, IrmEsp= 1,PaiFilho= "0", Tarifa="15", Embarque= "C")
modelo <- glm(formula = Sobreviveu ~., family = "binomial", data = treino)
summary(modelo)
modelo_2 <- glm(formula= Sobreviveu ~ Classe + Sexo, Idade+ IrmEsp, family = "binomial", data=treino)
summary(modelo_2)

#prediçoes na amostra teste

predicoes <- predict(modelo_2, newdata = teste, type = 'response')
predicoes <- ifelse(predicoes> 0.5, 1,0)

confusionMatrix(factor(predicoes), factor(teste$Sobreviveu))

# eu morreria? 
eu <- data.frame(Classe = 3, Sexo = "female", Idade = 19, IrmEsp= 1,PaiFilho= 0, Tarifa=15, Embarque= "C")
eu_morreria <- predict(modelo_2, newdata = eu, type = 'response')
ifelse(eu_morreria >0.5,1,0)










