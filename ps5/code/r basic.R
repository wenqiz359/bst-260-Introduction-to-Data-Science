#r basic

x <- c("b", "s", "t", " ", "2", "6", "0")
seq_along(x)

for (i in seq_along(x)){
  cat(toupper(x[i]))
}
