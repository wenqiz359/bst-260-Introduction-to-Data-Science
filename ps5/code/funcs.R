#create get_cdc_data that receives and endpoint and returns a data frame
get_cdc_data <- function(url){
  ret <- request(url) |> 
    req_url_query("$limit" = 1000000000) |>
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  return(ret)
}