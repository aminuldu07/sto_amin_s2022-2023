#API Keys_ChatGpt
#  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 #

library(RTutor)
run_app()


library(rcdk)
help(package = "rcdk")
help(rpart)
install.packages('chemometrics')
library(chemometrics)

## try http:// if https:// URLs are not supported

source("https://bioconductor.org/biocLite.R")
biocLite("ChemmineR")




library(rJava)

# create an R list with 15095 elements
r_list <- replicate(15095, rnorm(1), simplify = FALSE)

# create a Java array from the R list
j_array <- jarray(unlist(r_list), dim=c(length(r_list),1))

# create a Java object from the Java array
j_obj <- .jnew("java/util/ArrayList")
.jcall(j_obj, "Z", "addAll", j_array)
