
# pacotes -----------------------------------------------------------------

library(tidyverse)
library(readxl)



# dados iniciais ----------------------------------------------------------

dados_raw <- read_excel("./dados/dados-originais/Quadro das Empresas Estatais Estaduais PAF 2020v1.xlsx", skip = 2, sheet = "Dados")

dados_essenciais <- dados_raw %>%
  select(
    Estado,
    emp    = `Nome da Empresa`,
    seg    = `Ficha de Identificação da Estatal > Setor`,
    dep    = `Ficha de Identificação da Estatal > Dependência`,
    PL     = `Ficha de Informações Financeiras da Estatal > Patrimônio Líquido`,
    lucros = `Ficha de Informações Financeiras da Estatal > Lucro / Prejuízo Líquido do Exercício`
  )

