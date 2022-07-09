library(readxl)
library(tidyverse)
library(corrplot)

################### selecao de dados ###################

# importar dados do excel
dados <- read_excel("~/Documents/Files from c/Mestrado_Gabriel/R/Dados_Brutos_Celulas_20211115.xlsx")

# estatisticas e correlacoes
summary(dados)
cor(dados)
corrplot(cor(dados),method = "number")
corrplot(cor(dados),method = "circle")

# filtragem e correlacao dados de infraestrutura
infra <- dados %>% select(LOG_002,ILU_002,PAV_002,CAL_002,MFI_002,BUE_002,ESG_002,LIX_002,B046_002,B049_002,CLX_002,CLX_005,ABS_002,B020_B021)
summary(infra)
cor(infra)
corrplot(cor(infra),method = "number")

# analise outliers
boxplot(dados$MBA_001, range = 3)
boxplot.stats(dados$MBA_001, coef = 3)

# filtragem dados selecionados para o indice
dados_indice <- dados %>% 
  mutate(B020_B021 = B020_001+B021_001,
         B024_002i = (B024_002*-1)+1, 
         PAV_CAL_MFI = (PAV_002+CAL_002+MFI_002)/3,
         ac_edu = (ac_edu_publi-min(ac_edu_publi))/(max(ac_edu_publi)-min(ac_edu_publi)),
         ac_saude = (ac_saude_publi-min(ac_saude_publi))/(max(ac_saude_publi)-min(ac_saude_publi)),
         ac_emp = (ac_emp_publi-min(ac_emp_publi))/(max(ac_emp_publi)-min(ac_emp_publi)),
         ACESSO = (((ac_emp+ac_saude+ac_edu)/3)*-1)+1) %>%
  dplyr::select(ID_CEL,
                S001_002,B024_002i,Adeq_UHCT,
                MBA_001,
                APs_DHABC,N_EDIFI_01,
                Hidro15m_01,Slope30_01,APRM_01,INUND_01,MOV_MASSA_01,
                ILU_002,PAV_CAL_MFI,ESG_002,B046_002,CLX_002,ABS_002,B020_B021,
                ACESSO,VIAS_01) %>% 
  rename("DOMIMP" = S001_002, 
         "DOMBAN" = B024_002i,
         "INSAL" = Adeq_UHCT,
         "MPORBAN" = MBA_001,
         "AP" = APs_DHABC,
         "ANE" = N_EDIFI_01,
         "CURSO15" = Hidro15m_01,
         "DECLI30" = Slope30_01,
         "APRM" = APRM_01,
         "INUND" = INUND_01,
         "MASSA" = MOV_MASSA_01,
         "ILUPUB" = ILU_002,
         "INFRA" = PAV_CAL_MFI,
         "ESGAB" = ESG_002,
         "ENERGIA" = B046_002,
         "CLIXO" = CLX_002,
         "ABSAGUA" = ABS_002,
         "ESGINAD" = B020_B021,
         "ACESSO" = ACESSO,
         "VIAS50" = VIAS_01)

# transforma o ID em coluna
dados_indice <- dados_indice %>% transform("ID_CEL" = as.numeric(ID_CEL))
rownames(dados_indice) = dados_indice$ID_CEL
dados_indice <- dados_indice %>% dplyr::select(2:21)

# estatisticas e correlacoes
summary(dados_indice)
cor(dados_indice)
corrplot(cor(dados_indice),method = "number")
corrplot(cor(dados_indice),method = "circle")


################### transformacao de variaveis ###################

# escalonamento linear
dados_indice_el = apply(dados_indice,2,function(x) (x-min(x))/(max(x)-min(x)))
summary(dados_indice_el)

# escore z
dados_indice_z = apply(dados_indice,2,function(x) (x-mean(x))/sd(x))
apply(dados_indice_z,2,var)

# log + 0.0001
dados_indice_log0 <- log(dados_indice + 0.0001) + 9.21034 # foi somado o valor de 9.21 de forma que os valores do log ficassem todos positivos

# log + 1.0001
dados_indice_log1 <- log(dados_indice + 1.0001)

# rank
dados_indice_rank = dados_indice
i = 1
while (i <= 20) {
  dados_indice_rank[i] <- rank(dados_indice_rank[i], ties.method = "average")
  i = i + 1
}

################### indice hierarquico ###################

# IMPH-v1: hierarquico com escalonamento linear
IMPH.v01 <- as.data.frame(dados_indice_el)

# calculo indices dimensoes
IMPH.v01 <- IMPH.v01 %>% mutate(IAFE = (DOMIMP+DOMBAN+INSAL)/3, # Índice de Adequacao Fisica da Edificacao
                                IAEF = MPORBAN, # Índice de Adequacao da Edificacao a Familia
                                ISJ = (AP+ANE)/2, # Índice de Seguranca Juridica 
                                ISA = (CURSO15+DECLI30+APRM+INUND+MASSA)/5, #Índice de Seguranca Ambiental
                                IISP = (ILUPUB+INFRA+ESGAB+ENERGIA+CLIXO+ABSAGUA+ESGINAD)/7, #Índice de Infraestrutura e Servicos Publicos
                                ILA = (ACESSO+VIAS50)/2) #Índice de Localizacao e Acessibilidade

# calculo indice final
IMPH.v01 <- as.data.frame(IMPH.v01)
IMPH.v01 <- IMPH.v01 %>% mutate(IMPH = (IAFE+IAEF+ISJ+ISA+IISP+ILA)/6)

IMPH.v01 = apply(IMPH.v01,2,function(x) (x-min(x))/(max(x)-min(x))) # escalonamento do indice final
summary(IMPH.v01)
IMPH.v01 <- as.data.frame(IMPH.v01)
boxplot(IMPH.v01[21:27], range = 1.5)

# Customizing the output
png("Mestrado_Gabriel/R/saida/boxplot_v1.png", width = 600, height = 350)
boxplot(IMPH.v01[21:27], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v01, file = "Mestrado_Gabriel/R/saida/IMPH-v1.csv", row.names = T)


# IMPH-v2: hierarquico com log 0.0001
IMPH.v02 <- as.data.frame(dados_indice_log0)

# calculo indices dimensoes
IMPH.v02 <- IMPH.v02 %>% mutate(IAFE = (DOMIMP+DOMBAN+INSAL)/3, # Índice de Adequacao Fisica da Edificacao
                                IAEF = MPORBAN, # Índice de Adequacao da Edificacao a Familia
                                ISJ = (AP+ANE)/2, # Índice de Seguranca Juridica 
                                ISA = (CURSO15+DECLI30+APRM+INUND+MASSA)/5, #Índice de Seguranca Ambiental
                                IISP = (ILUPUB+INFRA+ESGAB+ENERGIA+CLIXO+ABSAGUA+ESGINAD)/7, #Índice de Infraestrutura e Servicos Publicos
                                ILA = (ACESSO+VIAS50)/2) #Índice de Localizacao e Acessibilidade

# calculo indice final
IMPH.v02 <- as.data.frame(IMPH.v02)
IMPH.v02 <- IMPH.v02 %>% mutate(IMPH = (IAFE+IAEF+ISJ+ISA+IISP+ILA)/6)

IMPH.v02 = apply(IMPH.v02,2,function(x) (x-min(x))/(max(x)-min(x))) # escalonamento do indice final
summary(IMPH.v02)
IMPH.v02 <- as.data.frame(IMPH.v02)
boxplot(IMPH.v02[21:27], range = 1.5)

png("Mestrado_Gabriel/R/saida/boxplot_v2.png", width = 600, height = 350)
boxplot(IMPH.v02[21:27], range = 1.5)
dev.off() 


# exportar resultado
write.csv(IMPH.v02, file = "Mestrado_Gabriel/R/saida/IMPH-v2.csv", row.names = T)


# IMPH-v3: hierarquico com log 1.0001
IMPH.v03 <- as.data.frame(dados_indice_log1)

# calculo indices dimensoes
IMPH.v03 <- IMPH.v03 %>% mutate(IAFE = (DOMIMP+DOMBAN+INSAL)/3, # Índice de Adequacao Fisica da Edificacao
                                IAEF = MPORBAN, # Índice de Adequacao da Edificacao a Familia
                                ISJ = (AP+ANE)/2, # Índice de Seguranca Juridica 
                                ISA = (CURSO15+DECLI30+APRM+INUND+MASSA)/5, #Índice de Seguranca Ambiental
                                IISP = (ILUPUB+INFRA+ESGAB+ENERGIA+CLIXO+ABSAGUA+ESGINAD)/7, #Índice de Infraestrutura e Servicos Publicos
                                ILA = (ACESSO+VIAS50)/2) #Índice de Localizacao e Acessibilidade

# calculo indice final
IMPH.v03 <- as.data.frame(IMPH.v03)
IMPH.v03<- IMPH.v03 %>% mutate(IMPH = (IAFE+IAEF+ISJ+ISA+IISP+ILA)/6)

IMPH.v03 = apply(IMPH.v03,2,function(x) (x-min(x))/(max(x)-min(x))) # escalonamento do indice final
summary(IMPH.v03)
IMPH.v03 <- as.data.frame(IMPH.v03)
boxplot(IMPH.v03[21:27], range = 1.5)

png("Mestrado_Gabriel/R/saida/boxplot_v3.png", width = 600, height = 350)
boxplot(IMPH.v03[21:27], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v03, file = "Mestrado_Gabriel/R/saida/IMPH-v3.csv", row.names = T)


# IMPH-v4: hierarquico com rank
IMPH.v04 <- as.data.frame(dados_indice_rank)

# calculo indices dimensoes
IMPH.v04 <- IMPH.v04 %>% mutate(IAFE = (DOMIMP+DOMBAN+INSAL)/3, # Índice de Adequacao Fisica da Edificacao
                                IAEF = MPORBAN, # Índice de Adequacao da Edificacao a Familia
                                ISJ = (AP+ANE)/2, # Índice de Seguranca Juridica 
                                ISA = (CURSO15+DECLI30+APRM+INUND+MASSA)/5, #Índice de Seguranca Ambiental
                                IISP = (ILUPUB+INFRA+ESGAB+ENERGIA+CLIXO+ABSAGUA+ESGINAD)/7, #Índice de Infraestrutura e Servicos Publicos
                                ILA = (ACESSO+VIAS50)/2) #Índice de Localizacao e Acessibilidade

# calculo indice final
IMPH.v04 <- as.data.frame(IMPH.v04)
IMPH.v04 <- IMPH.v04 %>% mutate(IMPH = (IAFE+IAEF+ISJ+ISA+IISP+ILA)/6)

IMPH.v04 = apply(IMPH.v04,2,function(x) (x-min(x))/(max(x)-min(x))) # escalonamento do indice final
summary(IMPH.v04)
IMPH.v04 <- as.data.frame(IMPH.v04)
boxplot(IMPH.v04[21:27], range = 1.5)

png("Mestrado_Gabriel/R/saida/boxplot_v4.png", width = 600, height = 350)
boxplot(IMPH.v04[21:27], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v04, file = "Mestrado_Gabriel/R/saida/IMPH-v4.csv", row.names = T)


################### indice indutivo (ACP) ###################

library(factoextra)
library(psych)

# Determinar o numero de componentes retidos (analise grafica)
ifelse(!require(nFactors),install.packages('nFactors', dependencies=TRUE),1) # instalar pacote se necessario
require(nFactors)
ev <- eigen(cor(dados_indice_z)) # eigenvalues
ap <- parallel(subject=nrow(dados_indice_z),var=ncol(dados_indice_z), rep=100, cent = .05, quantile =  .05, model = 'components') # model = "factors", "components"
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea, cor = TRUE, model = 'components', criteria = 1)
plotnScree(nS)

# calculo PCA
PCA = princomp(dados_indice_z)

# variancia de cada componente
var = PCA$sdev^2
100*var/sum(var)
cumsum(100*var/sum(var))
variance = (PCA$sdev^2/sum(PCA$sdev^2)) * 100

fviz_eig(PCA, addlabels = T) # grafico scree plot

fviz_pca_var(PCA,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = "YlOrRd",
             repel = TRUE     # Avoid text overlapping
)

# eigenvectors (autovetores)
cor(dados_indice_z,PCA$scores)

# eigenvalues (autovalores)
eig.val <- get_eigenvalue(PCA)
eig.val

# resultados das variaveis
res.var <- get_pca_var(PCA)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 

# resultados individuais
res.ind <- get_pca_ind(PCA)
res.ind$coord          # Coordinates = scores
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

# loadings e scores individuais
PCA$loadings
PCA$scores

# grafico de correlacao variaveis x componentes
corrplot(res.var$cos2, is.corr = F)
corrplot(res.var$contrib, is.corr = F)

# teste KMO
library(Rcmdr)
matcor <- cor(dados_indice_z)
matcorp <- partial.cor(dados_indice_z)
p <- ncol(dados_indice_z)

idiag <- seq(1, by = p + 1, length = p)
somar2 <- sum((as.numeric(matcor)[-idiag])^2)

# resultado do KMO:
cat("\n KMO = ",somar2/(somar2 + sum((as.numeric(matcorp$R)[-idiag])^2)))

# IMPH.v05: kaiser com peso nas componentes (Hewton, 2021)
IMPH.v05 <- cbind(dados_indice_z,PCA$scores[,c(1,2,3,4,5,6,7)])
IMPH.v05 <- as.data.frame(IMPH.v05) %>% mutate(Comp.1 = Comp.1*-1)

IMPH.v05.01 <- IMPH.v05[21:27]
IMPH.v05.01 = apply(IMPH.v05.01,2,function(x) (x-min(x))/(max(x)-min(x)))
IMPH.v05.01 <- as.data.frame(IMPH.v05.01)

IMPH.v05.01 <- IMPH.v05.01 %>% rename(CP1 = Comp.1, CP2 = Comp.2, CP3 = Comp.3, CP4 = Comp.4, CP5 = Comp.5, CP6 = Comp.6, CP7 = Comp.7) %>% 
  mutate(IMPH = (CP1*eig.val$variance.percent[1])+(CP2*eig.val$variance.percent[2])+(CP3*eig.val$variance.percent[3])+(CP4*eig.val$variance.percent[4])+(CP5*eig.val$variance.percent[5])+(CP6*eig.val$variance.percent[6])+(CP7*eig.val$variance.percent[7]))
IMPH.v05.01 = apply(IMPH.v05.01,2,function(x) (x-min(x))/(max(x)-min(x)))

IMPH.v05 <- cbind(IMPH.v05,IMPH.v05.01)

boxplot(IMPH.v05[28:35])
summary(IMPH.v05$IMPH)

png("~/Documents/Files from c/Mestrado_Gabriel/R/saida/boxplot_v5.png", width = 600, height = 350)
boxplot(IMPH.v05[28:35], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v05, file = "Mestrado_Gabriel/R/saida/IMPH-v5.csv", row.names = T)


# IMPH.v06: kaiser sem peso nas componentes
IMPH.v06 <- cbind(dados_indice_z,PCA$scores[,c(1,2,3,4,5,6,7)])
IMPH.v06 <- as.data.frame(IMPH.v06) %>% mutate(Comp.1 = Comp.1*-1)

IMPH.v06.01 <- IMPH.v06[21:27]
IMPH.v06.01 = apply(IMPH.v06.01,2,function(x) (x-min(x))/(max(x)-min(x)))
IMPH.v06.01 <- as.data.frame(IMPH.v06.01)
IMPH.v06.01 <- IMPH.v06.01 %>% rename(CP1 = Comp.1, CP2 = Comp.2, CP3 = Comp.3, CP4 = Comp.4, CP5 = Comp.5, CP6 = Comp.6, CP7 = Comp.7) %>% 
  mutate(IMPH = (CP1+CP2+CP3+CP4+CP5+CP6+CP7)/7)
IMPH.v06.01 = apply(IMPH.v06.01,2,function(x) (x-min(x))/(max(x)-min(x)))

IMPH.v06 <- cbind(IMPH.v06,IMPH.v06.01)

boxplot(IMPH.v06[28:35])
summary(IMPH.v06$IMPH)

png("~/Documents/Files from c/Mestrado_Gabriel/R/saida/boxplot_v6.png", width = 600, height = 350)
boxplot(IMPH.v06[28:35], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v06, file = "Mestrado_Gabriel/R/saida/IMPH-v6.csv", row.names = T)


# IMPH.v07: optimal coordinates com peso nas componentes
IMPH.v07 <- cbind(dados_indice_z,PCA$scores[,c(1,2,3,4,5,6)])
IMPH.v07 <- as.data.frame(IMPH.v07) %>% mutate(Comp.1 = Comp.1*-1)

IMPH.v07.01 <- IMPH.v07[21:26]
IMPH.v07.01 = apply(IMPH.v07.01,2,function(x) (x-min(x))/(max(x)-min(x)))
IMPH.v07.01 <- as.data.frame(IMPH.v07.01)

IMPH.v07.01 <- IMPH.v07.01 %>% rename(CP1 = Comp.1, CP2 = Comp.2, CP3 = Comp.3, CP4 = Comp.4, CP5 = Comp.5, CP6 = Comp.6) %>% 
  mutate(IMPH = (CP1*eig.val$variance.percent[1])+(CP2*eig.val$variance.percent[2])+(CP3*eig.val$variance.percent[3])+(CP4*eig.val$variance.percent[4])+(CP5*eig.val$variance.percent[5])+(CP6*eig.val$variance.percent[6]))
IMPH.v07.01 = apply(IMPH.v07.01,2,function(x) (x-min(x))/(max(x)-min(x)))

IMPH.v07 <- cbind(IMPH.v07,IMPH.v07.01)

boxplot(IMPH.v07[27:33])
summary(IMPH.v07$IMPH)

png("~/Documents/Files from c/Mestrado_Gabriel/R/saida/boxplot_v7.png", width = 600, height = 350)
boxplot(IMPH.v07[27:33], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v07, file = "Mestrado_Gabriel/R/saida/IMPH-v7.csv", row.names = T)


# IMPH.v08: optimal coordinates sem peso nas componentes
IMPH.v08 <- cbind(dados_indice_z,PCA$scores[,c(1,2,3,4,5,6)])
IMPH.v08 <- as.data.frame(IMPH.v08) %>% mutate(Comp.1 = Comp.1*-1)

IMPH.v08.01 <- IMPH.v08[21:26]
IMPH.v08.01 = apply(IMPH.v08.01,2,function(x) (x-min(x))/(max(x)-min(x)))
IMPH.v08.01 <- as.data.frame(IMPH.v08.01)

IMPH.v08.01 <- IMPH.v08.01 %>% rename(CP1 = Comp.1, CP2 = Comp.2, CP3 = Comp.3, CP4 = Comp.4, CP5 = Comp.5, CP6 = Comp.6) %>% 
  mutate(IMPH = (CP1+CP2+CP3+CP4+CP5+CP6)/6)
IMPH.v08.01 = apply(IMPH.v08.01,2,function(x) (x-min(x))/(max(x)-min(x)))

IMPH.v08 <- cbind(IMPH.v08,IMPH.v08.01)

boxplot(IMPH.v08[27:33])
summary(IMPH.v08$IMPH)

png("~/Documents/Files from c/Mestrado_Gabriel/R/saida/boxplot_v8.png", width = 600, height = 350)
boxplot(IMPH.v08[27:33], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v08, file = "Mestrado_Gabriel/R/saida/IMPH-v8.csv", row.names = T)


# IMPH.v09: escolha com peso nas componentes
IMPH.v09 <- cbind(dados_indice_z,PCA$scores[,c(1,2,3)])
IMPH.v09 <- as.data.frame(IMPH.v09) %>% mutate(Comp.1 = Comp.1*-1)

IMPH.v09.01 <- IMPH.v09[21:23]
IMPH.v09.01 = apply(IMPH.v09.01,2,function(x) (x-min(x))/(max(x)-min(x)))
IMPH.v09.01 <- as.data.frame(IMPH.v09.01)

IMPH.v09.01 <- IMPH.v09.01 %>% rename(CP1 = Comp.1, CP2 = Comp.2, CP3 = Comp.3) %>% 
  mutate(IMPH = (CP1*eig.val$variance.percent[1])+(CP2*eig.val$variance.percent[2])+(CP3*eig.val$variance.percent[3]))
IMPH.v09.01 = apply(IMPH.v09.01,2,function(x) (x-min(x))/(max(x)-min(x)))

IMPH.v09 <- cbind(IMPH.v09,IMPH.v09.01)

boxplot(IMPH.v09[24:27])
summary(IMPH.v09$IMPH)

png("~/Documents/Files from c/Mestrado_Gabriel/R/saida/boxplot_v9.png", width = 600, height = 350)
boxplot(IMPH.v09[24:27], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v09, file = "Mestrado_Gabriel/R/saida/IMPH-v9.csv", row.names = T)


# IMPH.v10: escolha sem peso nas componentes
IMPH.v10 <- cbind(dados_indice_z,PCA$scores[,c(1,2,3)])
IMPH.v10 <- as.data.frame(IMPH.v10) %>% mutate(Comp.1 = Comp.1*-1)

IMPH.v10.01 <- IMPH.v10[21:23]
IMPH.v10.01 = apply(IMPH.v10.01,2,function(x) (x-min(x))/(max(x)-min(x)))
IMPH.v10.01 <- as.data.frame(IMPH.v10.01)

IMPH.v10.01 <- IMPH.v10.01 %>% rename(CP1 = Comp.1, CP2 = Comp.2, CP3 = Comp.3) %>% 
  mutate(IMPH = (CP1+CP2+CP3)/3)
IMPH.v10.01 = apply(IMPH.v10.01,2,function(x) (x-min(x))/(max(x)-min(x)))

IMPH.v10 <- cbind(IMPH.v10,IMPH.v10.01)

boxplot(IMPH.v10[24:27])
summary(IMPH.v10$IMPH)

png("~/Documents/Files from c/Mestrado_Gabriel/R/saida/boxplot_v10.png", width = 600, height = 350)
boxplot(IMPH.v10[24:27], range = 1.5)
dev.off() 

# exportar resultado
write.csv(IMPH.v10, file = "Mestrado_Gabriel/R/saida/IMPH-v10.csv", row.names = T)


################### graficos e imagens ###################

corrplot(cor(dados_indice), method = "color", type = "upper", tl.col = "black", tl.cex = 0.8, sig.level = 0.05)

# histograma distribuicao indices 
cel_tipologia <- read_excel("~/Documents/Files from c/Mestrado_Gabriel/R/Dados_Celulas_Tipologias_APs.xlsx")
cel_tipologia$PRECARIO[cel_tipologia$PRECARIO == "NÃO PRECÁRIO"] <- "FORA DE APs"
cel_tipologia$PRECARIO[cel_tipologia$PRECARIO == "PRECÁRIO"] <- "APs"
cel_tipologia$TIPOLOGIA[cel_tipologia$TIPOLOGIA == "NÃO PRECÁRIO"] <- "FORA DE APs"


resultados <- IMPH.v01[27]
resultados <- cbind(resultados, IMPH.v02$IMPH, IMPH.v03$IMPH, IMPH.v04$IMPH, IMPH.v05$IMPH, IMPH.v06$IMPH, IMPH.v07$IMPH, IMPH.v08$IMPH, IMPH.v09$IMPH, IMPH.v10$IMPH, cel_tipologia[2:5])
resultados <- resultados %>% rename(IMPH.v01 = IMPH, IMPH.v02 = `IMPH.v02$IMPH`, IMPH.v03 = `IMPH.v03$IMPH`, IMPH.v04 = `IMPH.v04$IMPH`, IMPH.v05 = `IMPH.v05$IMPH`, IMPH.v06 = `IMPH.v06$IMPH`, IMPH.v07 = `IMPH.v07$IMPH`, IMPH.v08 = `IMPH.v08$IMPH`, IMPH.v09 = `IMPH.v09$IMPH`, IMPH.v10 = `IMPH.v10$IMPH`) %>% 
  pivot_longer(cols = starts_with("IMPH"), names_to = "indice", values_to = "valor")
resultados %>% ggplot() + geom_histogram(aes(x=valor), binwidth = 0.05, colour = "white") + 
  facet_wrap(~indice, ncol = 2) + labs(x = "Valor do Índice", y = "Contagem de Células") -> var_aux

resultados_media <- resultados %>% group_by(indice) %>% summarise(mean = mean(valor))
var_aux + geom_vline(data = resultados_media, aes(xintercept = mean), colour = "red", linetype = "dashed") 

# correlacao resultados
corrplot(cor(resultados[,c(1:10)]), method = "number", type = "upper", tl.col = "black", tl.cex = 0.8, sig.level = 0.05)

# boxplot resultados por tipologia
resultados %>% filter(indice == "IMPH.v01") %>% group_by(ID_CEL) %>% ggplot() + geom_boxplot(aes(x = TIPOLOGIA, y = valor)) + 
  labs(x = "Tipologias", y = "Valor do Índice") #boxplot resultados IMPH.v01 por tipologia

resultados %>% group_by(ID_CEL) %>% ggplot() + geom_boxplot(aes(x = TIPOLOGIA, y = valor, colour = TIPOLOGIA), outlier.size = 0.2) + 
  scale_color_manual(values = c("dodgerblue3","gold2","darkorange1","firebrick4")) + theme(legend.position = "none") + 
  facet_wrap(~indice, ncol = 2) + labs(x = "Tipologias", y = "Valor do Índice") #boxplot resultados por tipologia

x <- resultados %>% filter(MUNICÍPIO != "SÃO CAETANO DO SUL") %>% group_by(ID_CEL) %>% ggplot() + geom_boxplot(aes(x = indice, y = valor, colour = TIPOLOGIA), outlier.size = 0.2) + 
  facet_wrap(~MUNICÍPIO, ncol = 2) + labs(x = "Índices", y = "Valor do Índice", color = "Tipologia") #boxplot resultados IMPH.v01 a v3 por indice e municipio
x + scale_color_manual(values = c("dodgerblue3","gold2","darkorange1","firebrick4")) + theme(legend.position = "bottom")

resultados_media_mun <- resultados %>% group_by(indice, MUNICÍPIO) %>% summarise(mean = mean(valor))
resultados_media_mun %>% ggplot(aes(x = MUNICÍPIO, y = mean, fill = MUNICÍPIO), outlier.size = 0.2) + geom_col() + 
  scale_fill_manual(name = "MUNICÍPIO", values =  c("#c7e9b4","#7fcdbb","#41b6c4","#1d91c0","#225ea8","#253494","#081d58")) + theme(legend.position = "bottom", axis.text.x = element_blank()) + 
  facet_wrap(~indice, ncol = 2) + labs(x = "Municípios", y = "Valor do Índice") #barras resultados por município

# graficos de radar indices por tipologia
devtools::install_github("ricardo-bion/ggradar", dependencies=TRUE)
library(ggradar)

suppressPackageStartupMessages(library(dplyr))
library(scales)
library(tibble)

resultados.v1 <- IMPH.v01[21:27]
resultados.v1 <- cbind(resultados.v1, cel_tipologia[2:5]) 
resultados.v1 <- resultados.v1 %>% pivot_longer(cols = c(1:7), names_to = "indice", values_to = "valor")
var_aux <- resultados.v1 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v1 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v2 <- IMPH.v02[21:27]
resultados.v2 <- cbind(resultados.v2, cel_tipologia[2:5]) 
resultados.v2 <- resultados.v2 %>% pivot_longer(cols = c(1:7), names_to = "indice", values_to = "valor")
var_aux <- resultados.v2 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v2 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.5","1"), gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.5","1"), gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v3 <- IMPH.v03[21:27]
resultados.v3 <- cbind(resultados.v3, cel_tipologia[2:5]) 
resultados.v3 <- resultados.v3 %>% pivot_longer(cols = c(1:7), names_to = "indice", values_to = "valor")
var_aux <- resultados.v3 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v3 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v4 <- IMPH.v04[21:27]
resultados.v4 <- cbind(resultados.v4, cel_tipologia[2:5]) 
resultados.v4 <- resultados.v4 %>% pivot_longer(cols = c(1:7), names_to = "indice", values_to = "valor")
var_aux <- resultados.v4 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v4 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v5 <- IMPH.v05[28:35]
resultados.v5 <- cbind(resultados.v5, cel_tipologia[2:5]) 
resultados.v5 <- resultados.v5 %>% pivot_longer(cols = c(1:8), names_to = "indice", values_to = "valor")
var_aux <- resultados.v5 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v5 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v6 <- IMPH.v06[28:35]
resultados.v6 <- cbind(resultados.v6, cel_tipologia[2:5]) 
resultados.v6 <- resultados.v6 %>% pivot_longer(cols = c(1:8), names_to = "indice", values_to = "valor")
var_aux <- resultados.v6 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v6 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v7 <- IMPH.v07[27:33]
resultados.v7 <- cbind(resultados.v7, cel_tipologia[2:5]) 
resultados.v7 <- resultados.v7 %>% pivot_longer(cols = c(1:7), names_to = "indice", values_to = "valor")
var_aux <- resultados.v7 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v7 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v8 <- IMPH.v08[27:33]
resultados.v8 <- cbind(resultados.v8, cel_tipologia[2:5]) 
resultados.v8 <- resultados.v8 %>% pivot_longer(cols = c(1:7), names_to = "indice", values_to = "valor")
var_aux <- resultados.v8 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v8 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.3","0.6"), grid.mid = 0.3, grid.max = 0.6, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v9 <- IMPH.v09[24:27]
resultados.v9 <- cbind(resultados.v9, cel_tipologia[2:5])
resultados.v9 <- resultados.v9 %>% pivot_longer(cols = c(1:4), names_to = "indice", values_to = "valor")
var_aux <- resultados.v9 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v9 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.2","0.4"), grid.mid = 0.2, grid.max = 0.4, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.2","0.4"), grid.mid = 0.2, grid.max = 0.4, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")

resultados.v10 <- IMPH.v10[24:27]
resultados.v10 <- cbind(resultados.v10, cel_tipologia[2:5])
resultados.v10 <- resultados.v10 %>% pivot_longer(cols = c(1:4), names_to = "indice", values_to = "valor")
var_aux <- resultados.v10 %>% group_by(TIPOLOGIA, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
var_aux1 <- resultados.v10 %>% group_by(PRECARIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar ="Arial", var_aux, base.size = 5, values.radar = c("0","0.2","0.4"), grid.mid = 0.2, grid.max = 0.4, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("dodgerblue3","gold2","darkorange1","firebrick4"),legend.text.size = 10, legend.position = "bottom")
ggradar(font.radar ="Arial", var_aux1, base.size = 5, values.radar = c("0","0.2","0.4"), grid.mid = 0.2, grid.max = 0.4, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = c("red","dodgerblue3"),legend.text.size = 10, legend.position = "bottom")


# graficos de radar indices por municipio
library(RColorBrewer)

var_mun <- resultados.v1 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.5","1"), gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v2 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.5","1"), gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v3 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.5","1"), grid.mid = 0.5, grid.max = 1, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v4 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.5","1"), grid.mid = 0.5, grid.max = 1, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v5 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v6 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v7 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v8 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v9 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")

var_mun <- resultados.v10 %>% group_by(MUNICÍPIO, indice) %>% summarise(mean(valor)) %>% pivot_wider(names_from = "indice", values_from = "mean(valor)", values_fill = 0) 
ggradar(font.radar = "Arial", var_mun, base.size = 5, values.radar = c("0","0.4","0.8"), grid.mid = 0.4, grid.max = 0.8, gridline.min.colour = "black", gridline.mid.colour = "black", gridline.max.colour = "black", gridline.max.linetype = "solid",
        grid.label.size = 5, axis.label.size = 5, axis.line.colour = "black", group.line.width = 0.8, group.point.size = 0.8, group.colours = brewer.pal(7,"Paired"),legend.text.size = 10, legend.position = "right")
