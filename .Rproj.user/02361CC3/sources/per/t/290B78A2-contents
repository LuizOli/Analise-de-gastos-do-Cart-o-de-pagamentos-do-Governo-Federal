# Dados disponiveis pelo portal da transparencia
#
library(tidyverse)

message(getwd())

if (!("Dados_CPGF.rds" %in% dir())) {
  if (!dir.exists("Arquivos")) {
    dir.create("Arquivos")
    for (ano in 2013:2022) {
      for (mes in c(paste0(0, c(1:9)), 10, 11, 12)) {
        arquivo <- paste0(ano, mes)
        arquivo_zip <- paste0(arquivo, ".zip")
        destino <- paste0(paste0("/Arquivos/", arquivo_zip))
        if (!(arquivo_zip %in% dir("Arquivos"))) {
          try(expr = {
            message(paste("baixando", destino))
            Sys.sleep(runif(1, 0.5, 1))
            download.file(
              method = "auto",
              mode = "wb",
              cacheOK = F,
              url = paste0(
                "https://portaldatransparencia.gov.br/download-de-dados/cpgf/",
                arquivo_zip
              ),
              destfile = paste0(getwd(), destino)
            )
          })
        }
      }
    }
  }
  
  if (!dir.exists("CSVs")) {
    caminhos <-  paste0(getwd(), "/Arquivos/", dir("Arquivos"))
    
    lapply(caminhos, function(x) {
      unzip(zipfile = x, exdir = "CSVs")
    })
  }
  
  csvs <-
    paste0("CSVs/", dir("CSVs"))
  
  dados <-
    read_csv2(
      csvs[1],
      locale = locale(
        decimal_mark = ",",
        grouping_mark = ".",
        encoding = "Latin5"
      ),
      show_col_types = F
    )
  
  for (i in csvs[-1]) {
    dados <-
      rbind(dados,
            read_csv2(
              i,
              locale = locale(
                decimal_mark = ",",
                grouping_mark = ".",
                encoding = "Latin5"
              ),
              show_col_types = F
            ))
    message(paste("Lendo", i))
  }
  
  message(paste(colnames(dados), sep = " /n"))
  
  dados <-
    dados %>%
    mutate(
      `CÓDIGO ÓRGÃO SUPERIOR` = factor(`CÓDIGO ÓRGÃO SUPERIOR`),
      `NOME ÓRGÃO SUPERIOR` = factor(`NOME ÓRGÃO SUPERIOR`),
      `CÓDIGO ÓRGÃO` = factor(`CÓDIGO ÓRGÃO`),
      `NOME ÓRGÃO` = factor(`NOME ÓRGÃO`),
      `CÓDIGO UNIDADE GESTORA` = factor(`CÓDIGO UNIDADE GESTORA`),
      `NOME UNIDADE GESTORA` = factor(`NOME UNIDADE GESTORA`),
      `CPF PORTADOR` = factor(`CPF PORTADOR`),
      `NOME PORTADOR` = factor(`NOME PORTADOR`),
      `MÊS EXTRATO` = factor(`MÊS EXTRATO`),
      `CNPJ OU CPF FAVORECIDO` = factor(`CNPJ OU CPF FAVORECIDO`),
      `NOME FAVORECIDO` = factor(`NOME FAVORECIDO`),
      TRANSAÇÃO = factor(TRANSAÇÃO),
      `DATA TRANSAÇÃO` = lubridate::as_date(`DATA TRANSAÇÃO`, format = "%d/%m/%y")
    )
  saveRDS(dados,
          file = "Dados_CPGF.rds")
  infoRDS("Dados_CPGF.rds")
  
  message(unlink("Arquivos",
                 recursive = TRUE))
  message(unlink("CSVs",
                 recursive = TRUE))
}
{
  dados <- read_rds("Dados_CPGF.rds")
  
  colnames(dados) <-
    colnames(dados) %>%
    tolower() %>%
    make.names(unique = T)  %>%
    stringi::stri_trans_general("ascii")
  
  table(dados$nome.orgao.superior) %>%
    sort(decreasing = T)
  
  dados %>%
    group_by(nome.orgao.superior) %>%
    summarise(nome.orgao.superior, operacoes = n()) %>%
    distinct() %>%
    ggplot(aes(x = reorder(nome.orgao.superior, operacoes), y = operacoes)) +
    geom_bar(stat = "identity") +
    labs(y = "Operações",
         x = "Orgão") +
    coord_flip()
  
  dados %>% colnames()
  
  dados[is.na(dados$valor.transacao),] %>% dim() %>% message()
  
  dados <-
    dados %>%
    mutate(sigiloso = if_else(nome.portador == "Sigiloso", "Sim", "Não"))
  
}
# Corrigir pela inflacao

table(dados$ano.extrato,
      dados$mes.extrato)

ipca <-
  read_delim(
    "ipca_2002_2022.csv",
    delim = ";",
    escape_double = FALSE,
    locale = locale(decimal_mark = ",",
                    grouping_mark = "."),
    trim_ws = TRUE
  )

dados_sigilosos <-
  dados %>% filter(nome.portador == "Sigiloso")

dados_abertos <-
  dados %>% filter(nome.portador != "Sigiloso")

dados_abertos <-
  dados %>%
  mutate(ano = lubridate::year(data.transacao),
         mes_num = lubridate::month(data.transacao)) %>%
  left_join(ipca) %>%
  mutate(valor.corrigido = valor.transacao * fator_inflacao_2022)

dados_sigilosos <-
  dados_sigilosos %>%
  mutate(mes.extrato = as.numeric(mes.extrato)) %>%
  left_join(ipca,
            by = c("ano.extrato" = "ano",
                   "mes.extrato" = "mes_num")) %>%
  mutate(valor.corrigido = fator_inflacao_2022 * valor.transacao)

# Total anual gasto em milhoes

dados_abertos %>%
  select(valor.corrigido, ano.extrato, nome.orgao.superior) %>%
  rbind(dados_sigilosos %>%
          select(valor.corrigido, ano.extrato, nome.orgao.superior)) %>%
  group_by(ano.extrato, nome.orgao.superior) %>%
  summarise(
    ano.extrato,
    nome.orgao.superior,
    Total = sum(valor.corrigido,na.rm = T) / 1000000,
    Media = mean(valor.corrigido) / 1000000,
    Transacoes = n()
  ) %>%
  distinct() %>%
  ggplot(aes(
    x = (ano.extrato),
    y = Total,
    fill = nome.orgao.superior
  )) +
  # geom_bar(stat = "identity") +
  geom_area(position = "stack") +
  labs(x = "Ano",
       y = "Total Gasto",
       title = "Gasto anual total em milhões de reais",
       fill = "Orgão") +
  theme(legend.position = "bottom") +
  scale_x_continuous(n.breaks = 9)

# Gasto anual total PR


dados_abertos %>%
  select(valor.corrigido, ano.extrato, nome.orgao.superior, sigiloso) %>%
  rbind(
    dados_sigilosos %>%
      select(valor.corrigido, ano.extrato, nome.orgao.superior, sigiloso)
  ) %>%
  group_by(ano.extrato, sigiloso, nome.orgao.superior) %>%
  filter(nome.orgao.superior == "Presidência da República" &
           !is.na(valor.corrigido)) %>%
  summarise(
    ano.extrato,
    nome.orgao.superior,
    sigiloso,
    Total = sum(valor.corrigido,na.rm = T) / 1000000,
    Media = mean(valor.corrigido) / 1000000,
    Transacoes = n()
  ) %>%
  distinct() %>%
  ggplot(aes(
    x = factor(ano.extrato),
    y = Total,
    fill = sigiloso
  )) +
  geom_bar(stat = "identity") +
  labs(x = "Ano",
       y = "Total Gasto",
       title = "Gasto anual em milhões de reais pela Presidência da República") +
  theme(legend.position = "bottom")

# Comparando sigiloso e não sigiloso

dados_abertos %>%
  select(valor.corrigido,
         ano.extrato,
         nome.orgao.superior,
         nome.orgao,
         sigiloso) %>%
  rbind(
    dados_sigilosos %>%
      select(
        valor.corrigido,
        ano.extrato,
        nome.orgao.superior,
        nome.orgao,
        sigiloso
      )
  ) %>%
  filter(nome.orgao.superior == "Presidência da República") %>% 
  group_by(nome.orgao,ano.extrato,sigiloso) %>% 
  summarise(total = sum(valor.corrigido,na.rm = T)) %>%
  ggplot(aes(x = factor(ano.extrato),
             y = total/1000000,
             fill = nome.orgao))+
  geom_bar(stat= "identity")+facet_wrap(~sigiloso)

