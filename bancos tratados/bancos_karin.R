library(readxl)

processo <- read_excel("uniao.xlsx",sheet="processo")
aprovado <- read_excel("uniao.xlsx",sheet="aprovados")
edital <- read_excel("uniao.xlsx",sheet="edital")

names(aprovado) <- c("Logradouro","Bairro","ref1","ref2","ref3","area_util_m2","condominio","vagas","quartos","banheiros")
names(processo) <- c("Logradouro","Bairro","ref1","ref2","ref3","area_util_m2","condominio","vagas","quartos","banheiros")
names(edital) <- c("Endereço","Bairro","ref1","ref2","ref3","area_util_m2","condominio","vagas","quartos","banheiros","precouniao")

uniao_aprovado <- aprovado
uniao_aprovado$estimacao <- NA
uniao_aprovado$sugerido <- NA
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
                  "Preço Sugerido" = "sugerido")

k=1

for (k in 1:nrow(uniao_aprovado)){
  if (uniao_aprovado$Bairro[k] == "Asa Sul"){
    est_sul1 <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_aprovado$`Área útil (m²)`[k]
    + modelo_venda_apt[[1]][[3]]*uniao_aprovado$Vagas[k] + modelo_venda_apt[[1]][[4]]*uniao_aprovado$Quartos[k]
    + modelo_venda_apt[[1]][[5]]*uniao_aprovado$Banheiros[k] + modelo_venda_apt[[1]][[6]]
    uniao_aprovado$`Preço Estimado`[k] <- round((-0.05*est_sul1+1)^(1/-0.05),2)
  } else {
    est_norte1 <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_aprovado$`Área útil (m²)`[k]
    + modelo_venda_apt[[1]][[3]]*uniao_aprovado$Vagas[k] + modelo_venda_apt[[1]][[4]]*uniao_aprovado$Quartos[k]
    + modelo_venda_apt[[1]][[5]]*uniao_aprovado$Banheiros[k]
    uniao_aprovado$`Preço Estimado`[k] <- round((-0.05*est_norte1+1)^(1/-0.05),2)
  }
}

uniao_processo <- processo
uniao_processo$estimacao <- NA
uniao_processo$sugerido <- NA
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
                        "Preço Sugerido" = "sugerido",
                        "Preço Estimado" = "estimacao")

j=1

for (j in 1:nrow(uniao_processo)){
  if (uniao_processo$Bairro[j] == "Asa Sul"){
    est_sul <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_processo$`Área útil (m²)`[j]
    + modelo_venda_apt[[1]][[3]]*uniao_processo$Vagas[j] + modelo_venda_apt[[1]][[4]]*uniao_processo$Quartos[j]
    + modelo_venda_apt[[1]][[5]]*uniao_processo$Banheiros[j] + modelo_venda_apt[[1]][[6]]
    uniao_processo$`Preço Estimado`[j] <- round((-0.05*est_sul+1)^(1/-0.05),2)
  } else {
    est_norte <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_processo$`Área útil (m²)`[j]
    + modelo_venda_apt[[1]][[3]]*uniao_processo$Vagas[j] + modelo_venda_apt[[1]][[4]]*uniao_processo$Quartos[j]
    + modelo_venda_apt[[1]][[5]]*uniao_processo$Banheiros[j]
    uniao_processo$`Preço Estimado`[j] <- round((-0.05*est_norte+1)^(1/-0.05),2)
  }
}

uniao_edital <- edital
uniao_edital$estimacao <- NA
uniao_edital$sugerido <- NA
uniao_edital$comp_valor <- NA
uniao_edital$comp_perc <- NA
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
                        "Comparação (%)" = "comp_perc")

i=1

for (i in 1:nrow(uniao_edital)){
  if (uniao_edital$Bairro[i] == "Asa Sul"){
    est_apt <- modelo_venda_apt[[1]][[1]] + modelo_venda_apt[[1]][[2]]*uniao_edital$`Área útil (m²)`[i]
    + modelo_venda_apt[[1]][[3]]*uniao_edital$Vagas[i] + modelo_venda_apt[[1]][[4]]*uniao_edital$Quartos[i]
    + modelo_venda_apt[[1]][[5]]*uniao_edital$Banheiros[i] + modelo_venda_apt[[1]][[6]]
    uniao_edital$`Preço Estimado`[i] <- round((-0.05*est_apt+1)^(1/-0.05),2)
  } else {
    est_casa <- modelo_venda_casa[[1]][[1]] + modelo_venda_casa[[1]][[2]]*uniao_edital$`Área útil (m²)`[i]
    + modelo_venda_casa[[1]][[3]]*uniao_edital$Quartos[i] + modelo_venda_casa[[1]][[4]]*uniao_edital$Banheiros[i] 
    uniao_edital$`Preço Estimado`[i] <- round(exp(est_casa),2)
  }
}

saveRDS(uniao_aprovado, 'bancos tratados/uniao_aprovado.RDS')
saveRDS(uniao_edital, 'bancos tratados/uniao_edital.RDS')
saveRDS(uniao_processo, 'bancos tratados/uniao_processo.RDS')

