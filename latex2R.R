# latex2R is an R script that parses, converts LaTeX mathematical formulas into R expressions and evaluates them. 
# The formula must be numerical and not algebraic.
# V1.0
#
#Copyright 2021 Charlie COLLIN
#
#Licensed under the Apache License, Version 2.0 (the "License");
#you may not use this file except in compliance with the License.
#You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#Unless required by applicable law or agreed to in writing, software
#distributed under the License is distributed on an "AS IS" BASIS,
#WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#See the License for the specific language governing permissions and
#limitations under the License.


library("stringr")

latex2R <- function(L2R_latex) {
  # Converting latex formula in a string interpretable to R
  # input : string
  # output : string
  
 
  #Privates functions
  L2R_findTwins <- function (s,left,lsymb,rsymb){
    # Searching twin brackets' location in a string
    # input : [s:string], [left:integer  (index of the left bracket)], [lsymb:character of left symbol], [rsymb:character of right symbol]
    # return : integer (index of the right bracket or FALSE if the search was unsuccessful or if the input index is out of range)
    if(left>str_length(s)){return(FALSE)}
    
    flag <- FALSE
    right <- FALSE
    ss <- strsplit(s, "")[[1]]
    atom <- 0
    
    ssleft <- ss==lsymb
    ssright <- ss==rsymb
    
    
    if(!ssleft[left]){return(FALSE)}
    
    i<-left
    while(!flag){
      if(ssleft[i]){
        atom <- atom + 1
      }else if(ssright[i]){
        if(atom>1){
          atom <- atom -1
        }else if(atom==1){
          flag <- TRUE
          right <- i
        }
      }
      
      i <- i + 1
      if(i>str_length(s)){
        flag <- TRUE
      }
    }
    
    return(right)  
    
  }
  
  L2R_insert <- function(s,c,n){
    # Insert a character at a specific location in a string
    # input : [s:string], [c:character], [n:integer]
    # return : string
    lhs <- paste0("^(.{", n-1, "})(.+)$")
    rhs <- paste0("\\1", c, "\\2")
    return(gsub(lhs, rhs, s))
  }
  
  L2R_frac <- function(s){
    # Recursive function to convert latex fractions to quotients with the "/" operator
    # input : string containing a latex formula
    # return : string
    flag <- FALSE
    numL <- str_locate(s, "frac\\{")[1,2] #index of numrator's left bracket
    if(!is.na(numL)){
      flag <- TRUE
      numR <- L2R_findTwins(s,numL,"{","}") #index of numrator's right bracket
      s1 <- L2R_insert(s,"/",numR+1)
      return(L2R_frac(str_remove(s1,"frac")))
    }else{
      return(s)
    }
    
  }
  
  L2R_implicit_prod <- function(s){
    # Recursive function to search implicit products and insert the "*" operator
    # input : string
    # return : string
    n <- str_locate(s, "[\\d)][(]|[)][\\d(]")[1,1]
    if(!is.na(n)){
      s1 <- L2R_insert(s,"*",n+1)
      return(L2R_implicit_prod(s1))
    }else{
      return(s)
    }
  }
  
  # MAIN
  L2R_string <- toString(L2R_latex)
  
  # Deleting backslashs
  L2R_string <- str_remove_all(L2R_string, "\\\\")
  
  # Deleting \left and \right
  L2R_string <- str_remove_all(L2R_string, "left")
  L2R_string <- str_remove_all(L2R_string, "right")

  # Replacing operators
  L2R_string <- str_replace_all(L2R_string,"times","*")
  L2R_string <- str_replace_all(L2R_string,"x","*")
  L2R_string <- str_replace_all(L2R_string,"cdot","*")
  L2R_string <- str_replace_all(L2R_string,"div","/")
  L2R_string <- str_replace_all(L2R_string,":","/")
  
  L2R_string <- str_replace_all(L2R_string,"\\[","(")
  L2R_string <- str_replace_all(L2R_string,"\\]",")")
  L2R_string <- str_replace_all(L2R_string,"lbrace","(")
  L2R_string <- str_replace_all(L2R_string,"rbrace",")")
  L2R_string <- str_replace_all(L2R_string,",",".")
  
  # Converting fractions
  L2R_string <- L2R_frac(L2R_string)

  # Deleting spaces
  L2R_string <- str_remove_all(L2R_string, " ")
  
  # Replacing {} to ()
  L2R_string <- str_replace_all(L2R_string,"\\{","(")
  L2R_string <- str_replace_all(L2R_string,"\\}",")")
  
  # Searching implicit products
  L2R_string <- L2R_implicit_prod(L2R_string)
  
  return(L2R_string)
}

latex2R_eval <- function(L2R_string){
  # Evaluate a string contain a numeric maths expression
  # input : string
  # output : float or NA if the string is not a valid expression
  tryCatch(eval(parse(text=L2R_string)), error=function(err) NA)
}
