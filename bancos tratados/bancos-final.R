library(readxl)
library(tidyverse)
processo <- read_excel("uniao.xlsx",sheet="processo")
aprovado <- read_excel("uniao.xlsx",sheet="aprovados")
edital <- read_excel("uniao.xlsx",sheet="edital")

names(aprovado) <- c("Logradouro","Bairro","ref1","ref2","ref3","area_util_m2","condominio","vagas","quartos","banheiros")
names(processo) <- c("Logradouro","Bairro","ref1","ref2","ref3","area_util_m2","condominio","vagas","quartos","banheiros")
names(edital) <- c("Endereço","Bairro","ref1","ref2","ref3","area_util_m2","condominio","vagas","quartos","banheiros","precouniao")

########### APROVADO #############

uniao_aprovado <- aprovado
uniao_aprovado$estimacao <- NA
uniao_aprovado$sugerido <- NA
uniao_aprovado$aluguel <- NA
uniao_aprovado$cap <- NA
uniao_aprovado = rename(uniao_aprovado,
                  "Logradouro" = "Logradouro",
                  "Bairro" = "Bairro",
                  "Preço de reforma (R$420/m²)" = "ref1",
                  "Preço de reforma (R$550/m²)" = "ref2",
                  "Preço de reforma (R$650/m²)" = "ref3",
                  "Área útil (m²)" = "area_util_m2",
                  "Condomínio" = "condominio",
                  "Vagas" = "vagas",
                  "Quartos" = "quartos",
                  "Banheiros" = "banheiros",
                  "Preço Estimado" = "estimacao",
                  "Aluguel Estimado" = "aluguel",
                  "Cap Rate" = "cap",
                  "Preço Sugerido" = "sugerido")

# venda
k=1

for (k in 1:nrow(uniao_aprovado)){
  if (uniao_aprovado$Bairro[k] == "Asa Sul"){
    est_sul1 <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_aprovado$`Área útil (m²)`[k]+
    modelo_venda_apt[[1]][[3]]*uniao_aprovado$Vagas[k] + modelo_venda_apt[[1]][[4]]*uniao_aprovado$Quartos[k]+
    modelo_venda_apt[[1]][[5]]*uniao_aprovado$Banheiros[k] + modelo_venda_apt[[1]][[6]]
    uniao_aprovado$`Preço Estimado`[k] <- round(((-0.05*est_sul1)+1)^(1/-0.05),2)
  } else {
    est_norte1 <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_aprovado$`Área útil (m²)`[k]+
    modelo_venda_apt[[1]][[3]]*uniao_aprovado$Vagas[k] + modelo_venda_apt[[1]][[4]]*uniao_aprovado$Quartos[k]+
    modelo_venda_apt[[1]][[5]]*uniao_aprovado$Banheiros[k]
    uniao_aprovado$`Preço Estimado`[k] <- round(((-0.05*est_norte1)+1)^(1/-0.05),2)
  }
}

# aluguel
m=1

for (m in 1:nrow(uniao_aprovado)){
  if (uniao_aprovado$Bairro[m] == "Asa Sul"){
    if (uniao_aprovado$Condomínio[m] > 0){
      aluguel_sul_a <- modelo_aluguel_asa_sul[[1]][[1]] + modelo_aluguel_asa_sul[[1]][[2]]*uniao_aprovado$Quartos[m]+
      modelo_aluguel_asa_sul[[1]][[3]]*uniao_aprovado$Condomínio[m]
      uniao_aprovado$`Aluguel Estimado`[m] <- round(exp(aluguel_sul_a),2)
    } else {
      uniao_aprovado$`Aluguel Estimado`[m] <- "Não se aplica"
    }
  } else {
    if (uniao_aprovado$Condomínio[m] > 0){
      aluguel_norte_a <- modelo_aluguel_asa_norte[[1]][[1]] + modelo_aluguel_asa_norte[[1]][[2]]*uniao_aprovado$`Área útil (m²)`[m]+
      modelo_aluguel_asa_norte[[1]][[3]]*uniao_aprovado$Condomínio[m] + modelo_aluguel_asa_norte[[1]][[4]]*uniao_aprovado$Vagas[m]
      uniao_aprovado$`Aluguel Estimado`[m] <- round(((0.3*aluguel_norte_a)+1)^(1/0.3),2)
    } else {
      uniao_aprovado$`Aluguel Estimado`[m] <- "Não se aplica"
    }
  }
}

########### PROCESSO #############

uniao_processo <- processo
uniao_processo$estimacao <- NA
uniao_processo$sugerido <- NA
uniao_processo$aluguel <- NA
uniao_processo$cap <- NA
uniao_processo = rename(uniao_processo,
                        "Logradouro" = "Logradouro",
                        "Bairro" = "Bairro",
                        "Preço de reforma (R$420/m²)" = "ref1",
                        "Preço de reforma (R$550/m²)" = "ref2",
                        "Preço de reforma (R$650/m²)" = "ref3",
                        "Área útil (m²)" = "area_util_m2",
                        "Condomínio" = "condominio",
                        "Vagas" = "vagas",
                        "Quartos" = "quartos",
                        "Banheiros" = "banheiros",
                        "Preço Estimado" = "estimacao",
                        "Preço Sugerido" = "sugerido",
                        "Aluguel Estimado" = "aluguel",
                        "Cap Rate" = "cap")

# venda
j=1

for (j in 1:nrow(uniao_processo)){
  if (uniao_processo$Bairro[j] == "Asa Sul"){
    venda_sul_p <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_processo$`Área útil (m²)`[j]+
    modelo_venda_apt[[1]][[3]]*uniao_processo$Vagas[j] + modelo_venda_apt[[1]][[4]]*uniao_processo$Quartos[j]+
    modelo_venda_apt[[1]][[5]]*uniao_processo$Banheiros[j] + modelo_venda_apt[[1]][[6]]
    uniao_processo$`Preço Estimado`[j] <- round(((-0.05*venda_sul_p)+1)^(1/-0.05),2)
  } else {
    venda_norte_p <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_processo$`Área útil (m²)`[j]+
    modelo_venda_apt[[1]][[3]]*uniao_processo$Vagas[j] + modelo_venda_apt[[1]][[4]]*uniao_processo$Quartos[j]+
    modelo_venda_apt[[1]][[5]]*uniao_processo$Banheiros[j]
    uniao_processo$`Preço Estimado`[j] <- round(((-0.05*venda_norte_p)+1)^(1/-0.05),2)
  }
}


# aluguel
n=1

for (n in 1:nrow(uniao_processo)){
  if (uniao_processo$Bairro[n] == "Asa Sul"){
      aluguel_sul_p <- modelo_aluguel_asa_sul[[1]][[1]] + modelo_aluguel_asa_sul[[1]][[2]]*uniao_processo$Quartos[n]+
      modelo_aluguel_asa_sul[[1]][[3]]*uniao_processo$Condomínio[n]
      uniao_processo$`Aluguel Estimado`[n] <- round(exp(aluguel_sul_p),2)
  } else {
      aluguel_norte_p <- modelo_aluguel_asa_norte[[1]][[1]] + modelo_aluguel_asa_norte[[1]][[2]]*uniao_processo$`Área útil (m²)`[n]+
      modelo_aluguel_asa_norte[[1]][[3]]*uniao_processo$Condomínio[n] + modelo_aluguel_asa_norte[[1]][[4]]*uniao_processo$Vagas[n]
      uniao_processo$`Aluguel Estimado`[n] <- round(((0.3*aluguel_norte_p)+1)^(1/0.3),2)
  }
}


########### EDITAL #############

uniao_edital <- edital
uniao_edital$estimacao <- NA
uniao_edital$sugerido <- NA
uniao_edital$comp_valor <- NA
uniao_edital$comp_perc <- NA
uniao_edital$aluguel <- NA
uniao_edital$cap <- NA
uniao_edital = rename(uniao_edital,
                        "Endereço" = "Endereço",
                        "Bairro" = "Bairro",
                        "Preço de reforma (R$420/m²)" = "ref1",
                        "Preço de reforma (R$550/m²)" = "ref2",
                        "Preço de reforma (R$650/m²)" = "ref3",
                        "Área útil (m²)" = "area_util_m2",
                        "Condomínio" = "condominio",
                        "Vagas" = "vagas",
                        "Quartos" = "quartos",
                        "Banheiros" = "banheiros",
                        "Preço União" = "precouniao",
                        "Preço Estimado" = "estimacao",
                        "Preço Sugerido" = "sugerido",
                        "Comparação (R$)" = "comp_valor",
                        "Comparação (%)" = "comp_perc",
                        "Aluguel Estimado" = "aluguel",
                        "Cap Rate" = "cap")

# venda
i=1

for (i in 1:nrow(uniao_edital)){
  if (uniao_edital$Bairro[i] == "Asa Sul"){
    est_apt <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_edital$`Área útil (m²)`[i]+
    modelo_venda_apt[[1]][[3]]*uniao_edital$Vagas[i] + modelo_venda_apt[[1]][[4]]*uniao_edital$Quartos[i]+
    modelo_venda_apt[[1]][[5]]*uniao_edital$Banheiros[i] + modelo_venda_apt[[1]][[6]]
    uniao_edital$`Preço Estimado`[i] <- round(((-0.05*est_apt)+1)^(1/-0.05),2)
  } else {
    est_casa <- modelo_venda_casa[[1]][[1]] + modelo_venda_casa[[1]][[2]]*uniao_edital$`Área útil (m²)`[i]+
    modelo_venda_casa[[1]][[3]]*uniao_edital$Quartos[i] + modelo_venda_casa[[1]][[4]]*uniao_edital$Banheiros[i] 
    uniao_edital$`Preço Estimado`[i] <- round(exp(est_casa),2)
  }
}

# aluguel
l=1

for (l in 1:nrow(uniao_edital)){
  if (uniao_edital$Bairro[l] == "Asa Sul"){
      aluguel_sul_e <- modelo_aluguel_asa_sul[[1]][[1]] + modelo_aluguel_asa_sul[[1]][[2]]*uniao_edital$Quartos[l]+
      modelo_aluguel_asa_sul[[1]][[3]]*uniao_edital$Condomínio[l]
      uniao_edital$`Aluguel Estimado`[l] <- round(exp(aluguel_sul_e),2)
  } else {
      aluguel_lago_e <- modelo_aluguel_lago_sul[[1]][[1]] + modelo_aluguel_lago_sul[[1]][[2]]*uniao_edital$`Área útil (m²)`[l]+
      modelo_aluguel_lago_sul[[1]][[3]]*uniao_edital$Quartos[l] + modelo_aluguel_lago_sul[[1]][[4]]*uniao_edital$Banheiros[l]
      uniao_edital$`Aluguel Estimado`[l] <- round(aluguel_lago_e,2)
  }
}






saveRDS(uniao_aprovado, 'bancos tratados/uniao_aprovado.RDS')
saveRDS(uniao_edital, 'bancos tratados/uniao_edital.RDS')
saveRDS(uniao_processo, 'bancos tratados/uniao_processo.RDS')











