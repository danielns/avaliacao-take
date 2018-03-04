require(data.table)

funcionarios = data.table(read.csv("data-raw/lista_de_funcionários.csv"))

#### Analise prévia do banco


# Análise prévia do banco de dados

variaveis_numericas = c("satisfacao", "ultima_avaliacao", "volume_projetos", 
                        "media_horas_mensais",  "tempo_cia", "acidente_trabalho",
                        "saiu", "promocao_ultimos_5_anos")

variaveis_categoricas = setdiff(names(funcionarios), variaveis_numericas)

sumario_vars_numericas = funcionarios[, list(vars=variaveis_numericas,
                                             media = lapply(.SD, function(x) round(mean(x, na.rm=T),2)),
                                             std = lapply(.SD, function(x) round(sd(x, na.rm=T),2)),
                                             min = lapply(.SD, function(x) min(x, na.rm=T)),
                                             max = lapply(.SD, function(x) max(x, na.rm=T))
                                             ),
                                      .SDcols = which(names(funcionarios) %in% variaveis_numericas)]

sumario_comercial = funcionarios[, list(N_pessoas = .N, porcent = round(.N*100/nrow(funcionarios), 2)),
                                        list(comercial)]

sumario_salario = funcionarios[, list(N_pessoas = .N, porcent = round(.N*100/nrow(funcionarios), 2)),
                                 list(salario)]


## Categoriza variáveis numéicas

funcionarios[, satisfacao_categorica := ifelse(satisfacao< 0.5, "Satisfacao: Ruim",
                                        ifelse(satisfacao>=0.5 & satisfacao<0.8, "Satisfacao: Intermediário",
                                               "Satisfacao: Excelente"))]

funcionarios[, avaliacao_categorica := ifelse(ultima_avaliacao< 0.5, "Avaliacao: Ruim",
                                               ifelse(ultima_avaliacao>=0.5 & ultima_avaliacao<0.8, "Avaliacao: Intermediário",
                                                      "Avaliacao: Excelente"))]

funcionarios[, media_horas_categorica := ifelse(media_horas_mensais< 100, "Horas: Abaixo de 100",
                                         ifelse(media_horas_mensais >= 100 & media_horas_mensais <150, "Horas: Entre 100 e 150",
                                         ifelse(media_horas_mensais >= 150 & media_horas_mensais <200, "Horas: Entre 150 e 200",
                                         ifelse(media_horas_mensais >= 200 & media_horas_mensais <250, "Horas: Entre 200 e 250",
                                                "Horas: Acima de 250"))))]


funcionarios_agreg = funcionarios[, list(freq=.N), list(satisfacao_categorica, 
                                       avaliacao_categorica, 
                                       volume_projetos = paste("Volume:", volume_projetos),
                                       media_horas_categorica,
                                       tempo_cia = paste("Tempo_cia:", tempo_cia),
                                       acidente_trabalho = paste("Acidente:", acidente_trabalho),
                                       promocao_ultimos_5_anos = paste("Promoção:", promocao_ultimos_5_anos),
                                       comercial = paste("Comercial:", comercial),
                                       salario = paste("Salario", salario),
                                       saiu)]
funcionarios_agreg[, saiu:= ifelse(saiu==1, "SIM", "NAO")]

formula = "satisfacao_categorica + avaliacao_categorica + volume_projetos + media_horas_categorica + tempo_cia + acidente_trabalho + promocao_ultimos_5_anos + comercial + salario ~ saiu"

funcionarios_agreg = dcast(funcionarios_agreg, formula, value.var = "freq", fill=0)

funcionarios_agreg[, sum(SIM) + sum(NAO)] # tudo correto

geraAgregacao = function(funcionarios_agreg, n_combinacoes, limiar_porcentagem_sim = 0.1, limiar_raza_sim_nao = 5){
  
  # =========================================================================================
  # Gera bancos agregados segundo todas as combinações possíveis das variáveis 
  # categoricas agrupadas em n_combinacoes de elementos.
  # O banco final apresenta apenas as linhas com padrão de turn over
  # segundo os limiares da porcentagem de sim em relação ao total e da razão sim não
  #
  # Ags.:
  # funcionarios_agreg -- data.table de funcionarios_agreg
  # n_combinacoes -- determina o tamanho dos grupos nas agregações
  # limiar_porcentagem_sim -- determina o padrão turn over segundo porcentagem de sim em relação ao total 
  # limiar_raza_sim_nao -- determina o padrão turn over segundo a quantidade de pessoas que saem em relação as que ficam
  #
  # Return:
  # data.table com as linhas que apresentam turn over segundo todas as combinações possiveis 
  # das variáveis categóricas em n_combinacoes elementos.
  #
  #==========================================================================================
  
  variavies_id = names(funcionarios_agreg)[!names(funcionarios_agreg) %in% c("SIM", "NAO")]
  
  lista_combinacoes = list(combn(variavies_id, m=n_combinacoes, simplify = FALSE))[[1]]
  
  lista_resultados = list()
  
  for(combinacao in lista_combinacoes){
    
    dt_temp = funcionarios_agreg[, list(SIM = sum(SIM), NAO = sum(NAO)), by=eval(combinacao)]
    
    setnames(dt_temp, names(dt_temp)[1:n_combinacoes], paste0("V", 1:n_combinacoes))
    
    dt_temp[, porcent_sim_total := round(SIM/sum(SIM), 2)]
    dt_temp[, razao_sim_nao := round(SIM/(NAO + 0.001),2)] # Evitar divisão por 0
    
    dt_temp = dt_temp[porcent_sim_total >= 0.1 & razao_sim_nao > 5, ]
    
    lista_resultados[[length(lista_resultados)+1]] = dt_temp
  }
  
  resultado = rbindlist(lista_resultados, use.names = T, fill=T)
  resultado[, propensao_sair := round((SIM*100) / (SIM + NAO), 2)]
  
  return(resultado)
  
}

resultado_n3 = geraAgregacao(funcionarios_agreg, n_combinacoes=3)
resultado_n3 = resultado_n3[order(-propensao_sair)]


resultado_n4 = geraAgregacao(funcionarios_agreg, n_combinacoes=4)
resultado_n4 = resultado_n4[order(-propensao_sair)]
