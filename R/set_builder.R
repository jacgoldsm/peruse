################################################
#Use an approximation of Python's set builder
#Notation in R, based around the magrittr pipe
################################################


########################################################
#Creates a vector [a,b) given integer inputs a and b. 
#Defeats the purpose of Python's range object because
#It eagerly evaluates the vector, but included to 
#accommodate Python notation
########################################################
range <- function(a,b) {
  a <- as.integer(a)
  b <- as.integer(b)
  if (a < b) return(a:(b-1)) else return(numeric())
}

#########################################################
#`that_for_all` and `that_for_any` are helper functions
#that allow the user to provide a set that the initial
#set will be compared to by `we_have`
#########################################################

that_for_all <- function(set1, set2) {
  set2 <- rlang::enexpr(set2)
  return(list(set1 = set1,
              set2 = set2, 
              quant = 'all'))
}

that_for_any <- function(set1, set2) {
  set2 <- rlang::enexpr(set2)
  return(list(set1 = set1,
              set2 = set2,
              quant = 'any'))
}


##########################################################
#The first argument is designed to be piped in from
#`that_for_*`, but is otherwise a list of two sets
#and a quantifier. `we_have` returns the elements of
#the first set that match the boolean condition for
#(all or any) of the second set
###########################################################
we_have <- function(that_for, formula) {
  ret <- list()
  if (that_for$quant == 'all') {
    for (x in 1:length(that_for$set1)) {
      bool_vec <- purrr::map2_lgl(that_for$set1[x], eval(that_for$set2), formula)
      bool_vec <- ifelse(is.na(bool_vec), FALSE, bool_vec)
      if (all(bool_vec)) {
        ret <- append(ret,that_for$set1[x])
      }
    }
  }
  
  if (that_for$quant == 'any') {
    for (x in 1:length(that_for$set1)) {
      bool_vec <- purrr::map2_lgl(that_for$set1[x], eval(that_for$set2), formula)
      bool_vec <- ifelse(is.na(bool_vec), FALSE, bool_vec)
      if (any(bool_vec)) {
        ret <- append(ret,that_for$set1[x])
      }
    }
    
  }
  return(unlist(ret))
}