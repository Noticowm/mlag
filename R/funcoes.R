#' Calcula a Correlação entre Variáveis
#'
#' Esta função calcula a correlação entre variáveis dependentes e independentes.
#'
#' @param data Data frame contendo os dados.
#' @param dependent_vars Vetor com os nomes das variáveis dependentes.
#' @param independent_vars Vetor com os nomes das variáveis independentes.
#' @param method Método de correlação (padrão é "pearson").
#'
#' @return Matriz de correlações.
#' @export
correlacao <- function(data, dependent_vars, independent_vars, method="pearson") {

  all_vars <- c(dependent_vars, independent_vars)
  if(!all(all_vars %in% names(data))) {
    stop("Algumas variáveis especificadas não estão presentes na base de dados.")
  }

  result <- matrix(NA, nrow=length(independent_vars), ncol=length(dependent_vars))
  colnames(result) <- dependent_vars
  rownames(result) <- independent_vars

  for (dep_var in dependent_vars) {
    for (ind_var in independent_vars) {
      subset_data <- na.omit(data[, c(dep_var, ind_var)])
      result[ind_var, dep_var] <- cor(subset_data[[dep_var]], subset_data[[ind_var]], method=method)
    }
  }

  return(result)
}


#' Cria Lags de uma Variável
#'
#' Esta função cria colunas com lags de uma variável específica.
#'
#' @param data Data frame contendo os dados.
#' @param lags Número de lags a serem criados.
#' @param var Nome da variável para a qual os lags serão criados.
#'
#' @return Data frame com as colunas de lags adicionadas.
#' @export
lag_comp<-function(data, lags, var){

  lag_var<-lapply(1:lags,function(x){dplyr::lag(data[,var],x)})
  lagged_data<-do.call(cbind, lag_var)
  colnames(lagged_data)<-sapply(1:lags, function(x)(paste(var,'_lag',x,sep='')))
  data_lagged<-cbind(data, lagged_data)
  return(data_lagged)
}


#' Calcula Média Móvel
#'
#' Esta função calcula a média móvel de uma variável específica em um data frame.
#'
#' @param data Data frame contendo os dados.
#' @param var Nome da variável para a qual a média móvel será calculada.
#' @param k Ordem (número de períodos) da média móvel.
#'
#' @return Data frame com a coluna da média móvel adicionada.
#' @export
media_movel <- function(data, var, k) {

  if (!var %in% names(data)) {
    stop("A variável especificada não está presente na base de dados.")
  }

  media_movel_var <- zoo::rollmean(data[[var]], k = k, fill = NA, align = "right")
  nome_coluna <- paste0(var, "_MM", k)
  data[[nome_coluna]] <- media_movel_var

  return(data)
}






#' Calcula Média Móvel
#'
#' Esta função calcula a média móvel de uma variável específica em um data frame.
#'
#' @param base Data frame contendo os dados.
#' @param coluna_ano Variavel ano
#' @param coluna_semana variavel semana
#'
#' @param variaveis_soma vector com lista de variaveis cuja forma de agregar é a soma
#' @param variaveis_media vector com lista de variaveis cuja forma de agregar é a média
#'
#' @return Data frame com a coluna da média móvel adicionada.
#' @export

# Função para converter ano e semana em data
ano_semana_para_data <- function(ano, semana) {
  # Ajustar a data para a semana correta do ano
  as.Date(paste0(ano, "-01-01")) + (semana - 1) * 7
}

# Função para adicionar coluna "mes" ao data frame
adicionar_mes <- function(base, coluna_ano, coluna_semana) {
  # Aplicando a função para obter as datas
  datas <- ano_semana_para_data(base[[coluna_ano]], base[[coluna_semana]])

  # Extraindo o mês das datas
  base$mes <- format(datas, "%m")

  return(base)
}

# Função para agregar dados por ano e mês
agregar_por_ano_mes <- function(base, coluna_ano, coluna_semana, variaveis_soma, variaveis_media) {
  # Adicionando a coluna "mes"
  base <- adicionar_mes(base, coluna_ano, coluna_semana)

  # Agrupando por ano e mês
  base_agrupada <- base %>%
    group_by(!!sym(coluna_ano), mes) %>%
    summarise(
      across(all_of(variaveis_soma), ~ sum(.x, na.rm = TRUE), .names = "soma_{col}"),
      across(all_of(variaveis_media), ~ mean(.x, na.rm = TRUE), .names = "media_{col}")
    ) %>%
    ungroup()

  return(base_agrupada)
}


