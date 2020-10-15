
# pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(geobr)
library(viridis)


# dados iniciais ----------------------------------------------------------

dados_raw <- read_excel("./dados/dados-originais/Quadro das Empresas Estatais Estaduais PAF 2020v1.xlsx", skip = 2, sheet = "Dados")

dados_selecionados_raw <- dados_raw %>%
  select(
    Estado,
    emp       = `Nome da Empresa`,
    seg       = `Ficha de Identificação da Estatal > Setor`,
    dep       = `Ficha de Identificação da Estatal > Dependência`,
    PL        = `Ficha de Informações Financeiras da Estatal > Patrimônio Líquido`,
    lucros    = `Ficha de Informações Financeiras da Estatal > Lucro / Prejuízo Líquido do Exercício`,
    gov_ca    = `Ficha de Identificação da Estatal > Governança > Conselho de Administração`,
    gov_cf    = `Ficha de Identificação da Estatal > Governança > Conselho Fiscal`,
    gov_aud   = `Ficha de Identificação da Estatal > Governança > Comitê de Auditoria`,
    maior_rem = `Ficha de Informações Financeiras da Estatal > Valor da Maior Remuneração Paga`,
    plr_rva   = `Ficha de Informações Financeiras da Estatal > Foi Distribuído o PLR ou RVA em 2019?`
    )



# limpeza -----------------------------------------------------------------

## Setor
#dput(unique(dados_selecionados_raw$seg))

limpa_setor = data.frame(
  seg = c(
    "SETOR IMOBILIÁRIO", 
    "FINANCEIRO", 
    "TRANSPORTES", 
    "DESENVOLVIMENTO", 
    "OUTRO", 
    "SERVIÇOS PÚBLICOS", 
    "DISTRIBUIÇÃO DE GÁS", 
    "SANEAMENTO", 
    NA, 
    "ABASTECIMENTO",
    "URBANIZAÇÃO", 
    "PESQUISA", 
    "GESTÃO DE ATIVOS",  
    "Financeiro", 
    "Serviços Públicos", 
    "Abastecimento", 
    "Saneamento", 
    "Informática", 
    "SAÚDE", 
    "ASSISTENCIA TÉCNICA", 
    "Outro", 
    "Desenvolvimento", 
    "INFORMÁTICA", 
    "ASSIS. TÉCNICA", 
    "COMUNICAÇÕES", 
    "ENERGIA", 
    "SEAF", 
    "INFORMATICA", 
    "GÁS NATURAL", 
    "ASSITÊNCIA TÉCNICA", 
    "Agricultura", 
    "Administração de Obras", 
    "Energia", 
    "Transporte Ferroviário", 
    "Primário", 
    "Saneamento, Serv. Água e Gás", 
    "ASSISTÊNCIA TÉCNICA", 
    "OUTROS"),
  setor = c(
    "IMOBILIÁRIO", 
    "FINANCEIRO", 
    "TRANSPORTES", 
    "DESENVOLVIMENTO", 
    "OUTRO", 
    "SERVIÇOS PÚBLICOS", 
    "DISTRIBUIÇÃO DE GÁS", 
    "SANEAMENTO", 
    "OUTRO",
    "ABASTECIMENTO",
    "URBANIZAÇÃO", 
    "PESQUISA", 
    "GESTÃO DE ATIVOS",  
    "FINANCEIRO", 
    "SERVIÇOS PÚBLICOS", 
    "ABASTECIMENTO",
    "SANEAMENTO", 
    "INFORMÁTICA", 
    "SAÚDE", 
    "ASSISTÊNCIA TÉCNICA", 
    "OUTRO",
    "DESENVOLVIMENTO", 
    "INFORMÁTICA", 
    "ASSISTÊNCIA TÉCNICA", 
    "COMUNICAÇÕES", 
    "ENERGIA", 
    "ASSISTÊNCIA TÉCNICA",
    "INFORMÁTICA", 
    "DISTRIBUIÇÃO DE GÁS",
    "ASSISTÊNCIA TÉCNICA", 
    "ABASTECIMENTO", 
    "ADMINISTRAÇÃO DE OBRAS", 
    "ENERGIA", 
    "TRANSPORTE FERROVIÁRIO", 
    "PESQUISA", 
    "SANEAMENTO", 
    "ASSISTÊNCIA TÉCNICA",
    "OUTRO")
  )

## Dependência
#dput(unique(dados_selecionados_raw$dep_raw))

# limpa_dep <- data.frame(
#   dep_raw = c(
#     "NÃO DEPENDENTE", 
#     "DEPENDENTE", 
#     "Dependente", 
#     "Não dependente", 
#     NA),
#   dep = c(
#     "Não dependente",
#     "Dependente",
#     "Dependente",
#     "Não dependente",
#     "Não informado"
#   )
# )

# valores da CMTP :/

linha_CMTP <- dados_selecionados_raw$emp == "CMTP"
dados_selecionados_raw[linha_CMTP, "PL"] <- as.character(20.2e6)
dados_selecionados_raw[linha_CMTP, "lucros"] <- as.character(236.8e3)
dados_selecionados_raw[linha_CMTP, "maior_rem"] <- as.character(9.4e3)

# junta todo mundo

dados_selecionados <- dados_selecionados_raw %>%
  left_join(limpa_setor) %>%
  #left_join(limpa_dep) %>%
  mutate(
    dep    = toupper(dep),
    dep    = ifelse(is.na(dep), "NÃO INFORMADO", dep),
    PL     = as.numeric(PL),
    lucros = as.numeric(lucros))

#verifica empresas repetidas
#rep <- dados_selecionados %>% count(emp)

# corrige na mão alguns setores

termos <- c("COMPESA", "SUAPE", "DOCAS", "PORTOS", "Portos", "CAEMA")

gera_vetor <- function(termo){
  return(str_detect(dados_selecionados$emp, termo))
}

linhas <- map(termos, gera_vetor)
names(linhas) <- termos

atribui <- function(termo, coluna, valor){
  #print(dados_selecionados[linhas[[termo]], coluna])
  # pulo do gato aqui é o <<- para fazer o assignment na variável global
  dados_selecionados[linhas[[termo]], coluna] <<- valor
  #print(dados_selecionados[linhas[[termo]], coluna])
}

atribui("COMPESA", "setor", "SANEAMENTO")
atribui("SUAPE", "setor", "PORTOS E HIDROVIAS")
atribui("DOCAS", "setor", "PORTOS E HIDROVIAS")
atribui("PORTOS", "setor", "PORTOS E HIDROVIAS")
atribui("Portos", "setor", "PORTOS E HIDROVIAS")
atribui("CAEMA", "setor", "SANEAMENTO")
atribui("COMPESA", "setor", "SANEAMENTO")

#dados_selecionados[linhas[["CAEMA"]], "setor"] <- "SANEAMENTO"
#dados_selecionados[linhas[["COMPESA"]], "setor"]

# gráficos anteriores -- mapas --------------------------------------------

dados_qde_setor_estado <- dados_selecionados %>%
  count(setor, Estado)

mapa <- geobr::read_state()

mapa_qde <- mapa %>%
  inner_join(dados_qde_setor_estado, by = c("abbrev_state" = "Estado"))

ggplot(mapa_qde %>% filter(setor == "SANEAMENTO")) + 
    geom_sf(aes(fill = n > 0), color = "coral") +
    scale_fill_manual(values = c("TRUE" = "lightcoral", "FALSE" = NA)) +
    labs(fill = "Tem empresa de saneamento?")

