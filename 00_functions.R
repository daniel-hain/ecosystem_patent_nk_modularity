###########################################################
# Edgelist to sparse 2 mode or projected matrix
###########################################################

el_project <- function(data, i, j, direction = NULL){
  require(Matrix)
  
  i_vec <- data %>% pull({{i}}) 
  j_vec <- data %>% pull({{j}}) 
  
  mat <- spMatrix(nrow = i_vec %>% n_distinct(),
                  ncol = j_vec %>% n_distinct(),
                  i = i_vec %>% factor() %>% as.numeric(),
                  j = j_vec %>% factor() %>% as.numeric(),
                  x = rep(1, i_vec %>% as.numeric() %>% length())
  )
  
  row.names(mat) <- i_vec %>% factor() %>% levels()
  colnames(mat) <- j_vec %>% factor() %>% levels()
  
  if(direction == "i"){
    mat <- tcrossprod(mat)
  }
  if(direction == "j"){
    mat <- crossprod(mat)
  }
  
  return(mat)
}