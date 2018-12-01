

FirstSet <- function(x, y, n){
  
  for(i in 2:(n-1)){
    var = paste("a", i-1, sep = "")
    term = paste(x[i]^2, var, sep = " * ")
    
    var = paste("b", i-1, sep = "")
    second_term = paste(x[i], var, sep = " * ")
    term = paste(term, second_term, sep = " + ")
    
    var = paste("c", i-1, sep = "")
    term = paste(term, var, sep = " + ")
    
    term = paste(term, y[i], sep = " = ")
    #print(term)
    equations_list <- c(equations_list, term)
    
    
    
    var = paste("a", i, sep = "")
    term = paste(x[i]^2, var, sep = " * ")
    
    var = paste("b", i, sep = "")
    second_term = paste(x[i], var, sep = " * ")
    term = paste(term, second_term, sep = " + ")
    
    var = paste("c", i, sep = "")
    term = paste(term, var, sep = " + ")
    
    term = paste(term, y[i], sep = " = ")
    #print(term)
    equations_list <- c(equations_list, term)
  }  
  return(equations_list)
}


SecondSet <- function(x, y, n){
  var = "a1"
  term = paste(x[1]^2, var, sep = " * ")
  
  var = "b1"
  second_term = paste(x[1], var, sep = " * ")
  term = paste(term, second_term, sep = " + ")
  
  var = "c1"
  term = paste(term, var, sep = " + ")
  
  term = paste(term, y[1], sep = " = ")
  equations_list <- c(equations_list, term)
  
  
  var = paste("a", n-1, sep = "")
  term = paste(x[1]^2, var, sep = " * ")
  
  var = paste("b", n-1, sep = "")
  second_term = paste(x[1], var, sep = " * ")
  term = paste(term, second_term, sep = " + ")
  
  var = paste("c", n-1, sep = "")
  term = paste(term, var, sep = " + ")
  
  term = paste(term, y[(n-1)], sep = " = ")
  equations_list <- c(equations_list, term)
  return(equations_list)
}

ThirdSet <- function(x, y, n){
  for(i in 2:(n-1)){
    var = paste("a", i-1, sep = "")
    term = paste(2*x[i], var, sep = " * ")
    
    var = paste("b", i-1, sep = "")
    term = paste(term, var, sep = " + ")
    
    
    var = paste("a", i, sep = "")
    rhs_term = paste(2*x[i], var, sep = " * ")
    term = paste(term, rhs_term, sep = " = ")
    
    var = paste("b", i, sep = "")
    term = paste(term, var, sep = " + ")
    
    equations_list <- c(equations_list, term)
  }
  return(equations_list)
}

QuadraticSpline <- function(x, y, n){
  equations_list <- c()
  
  first_set = FirstSet(x, y, n)
  second_set = SecondSet(x, y, n)
  third_set = ThirdSet(x, y, n)
  
  equations_list <- c(equations_list, first_set)
  equations_list <- c(equations_list, second_set)
  equations_list <- c(equations_list, third_set)
  
  print(equations_list)
}


x <- c(3, 4.5,7, 9)
y <- c(2.5, 1, 2.5, 0.5)
n = length(x)

QuadraticSpline(x, y, n)



