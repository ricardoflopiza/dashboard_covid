

descript_block_function <- function(tabla){

  boxPad(color = "olive",   height = "9px", 
         lapply(1:nrow(tabla), function(i) {
           descriptionBlock(
             header = tabla$nombres[[i]], 
             text = tabla$V1[[i]], 
             rightBorder = FALSE,
             marginBottom = TRUE
           )
         
         })
  )
}


tab_function <- function(label, tabName,...){
menuItem(h4(label), tabName = tabName)
}








#tab_function <- function(label, size = 1, tabName,...){
#  #menuItem(rlang::parse_expr("h6(label)"), tabName = tabName)
#  menuItem(label, rlang::eval_tidy(rlang::parse_expr(sprintf("`h%s(\"%s\")`",size,label))), tabName = tabName)
#}


#parse(text = )