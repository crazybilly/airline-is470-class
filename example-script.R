foo  <- read.some.file

cyl6 <- mtcars[mtcars$cyl==6,]
cyl4 <- mtcars[mtcars$cyl==4,]

# is there a significant difference in mpg between 4 and 6 cylinder vehicles?
results  <- t.test(cyl6$mpg, cyl4$mpg)