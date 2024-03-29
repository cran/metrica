#' @title Mean Lack of Precision (MLP)
#' @name MLP
#' @description It estimates the MLP, the unsystematic error component to
#' the Mean Squared Error (MSE), for a continuous predicted-observed dataset 
#' following Correndo et al. (2021).
#' @param data (Optional) argument to call an existing data frame containing the data.
#' @param obs Vector with observed values (numeric).
#' @param pred Vector with predicted values (numeric).
#' @param tidy Logical operator (TRUE/FALSE) to decide the type of return. TRUE 
#' returns a data.frame, FALSE returns a list; Default : FALSE.
#' @param na.rm Logic argument to remove rows with missing values 
#' (NA). Default is na.rm = TRUE.
#' @return an object of class `numeric` within a `list` (if tidy = FALSE) or within a
#' `data frame` (if tidy = TRUE).
#' @details The MLP represents the unsystematic (random) component of the MSE. 
#' It is obtained via a symmetric decomposition of the MSE (invariant to 
#' predicted-observed orientation) using a symmetric regression line. 
#' The MLP is equal to the sum of unsystematic differences divided by the sample size (n). 
#' The greater the value the greater the random noise of the predictions.
#' For the formula and more details, see [online-documentation](https://adriancorrendo.github.io/metrica/articles/available_metrics_regression.html)
#' @references 
#' Correndo et al. (2021). 
#' Revisiting linear regression to test agreement in continuous predicted-observed datasets. 
#' _Agric. Syst. 192, 103194._ \doi{10.1016/j.agsy.2021.103194}
#' @examples 
#' \donttest{
#' set.seed(1)
#' X <- rnorm(n = 100, mean = 0, sd = 10)
#' Y <- X + rnorm(n=100, mean = 0, sd = 3)
#' MLP(obs = X, pred = Y)
#' }
#' @rdname MLP
#' @importFrom rlang eval_tidy quo
#' @export 
MLP <- function(data=NULL,
                obs, 
                pred,
                tidy = FALSE,
                na.rm = TRUE){
  
  MLP <- rlang::eval_tidy(
    data = data,
    rlang::quo(
      sum (abs({{obs}} - ((mean({{obs}}) -
        (sqrt(sum(({{obs}} - mean({{obs}}))^2)/length({{obs}}))/
           sqrt(sum(({{pred}} - mean({{pred}}))^2)/length({{pred}}))*mean({{pred}}))) +
          sqrt(sum(({{obs}} - mean({{obs}}))^2)/length({{obs}}))/
          sqrt(sum(({{pred}} - mean({{pred}}))^2)/length({{pred}})) * {{pred}})) *
          abs({{pred}} - ((mean({{pred}}) - (sqrt(sum(({{pred}} - mean({{pred}}))^2)/length({{pred}}))/
          sqrt(sum(({{obs}} - mean({{obs}}))^2)/length({{obs}}))*mean({{obs}}))) +
            sqrt(sum(({{pred}} - mean({{pred}}))^2)/length({{pred}}))/
            sqrt(sum(({{obs}} - mean({{obs}}))^2)/length({{obs}})) * {{obs}}) ) ) /
    length({{obs}}) 
    )
  )
  
  if (tidy==TRUE){ return(as.data.frame(MLP)) }
  
  if (tidy==FALSE){ return(list("MLP" = MLP)) }
}

