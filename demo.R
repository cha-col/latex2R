###============ BEGIN TEST =============

source("latex2R.R")

testFile = file("latexFormulas.txt", "r")
while ( TRUE ) {
  line = readLines(testFile, n = 1)
  if ( length(line) == 0 ) {
    break
  }
  print(c("input :",line))
  exp <- latex2R(line)
  print(c("output :",exp))
  print(c("eval :",latex2R_eval(exp)))
  
}

close(testFile)


# Notes : bien faire un saut de ligne à la dernière ligne du document texte contenant les formules latex