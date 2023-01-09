## 1
# 1.1
# a 
# Nes israiskoje 2.0*3, dauginame sveika skaiciu is trupmeninio, 
# tad rezultatas bus trupmena
# O israiskoje 2L*3L dauginame du sveikus skaicius,
# tad rezultatas bus sveikas skaicius

# b
# Nes mtcars[1] duoda pirma stulpeli kartu su indeksuu
# O mtcars[[1]] duoda pradziai pirma stulpeli kartu su indeksu
# Ir is to po to pasiima tik reiksmes

# 1.2
# Pradziai susikuriame sarasa su reiksmeemis FFTT5
# Tada ji bandome sumuoti, taciau bool tipo reiksmes yra konvertuojamos i sveika skaiciu
# False - 0, True = 1
# Todel gauname 0+0+1+1+5=7

# 1.3
three <- list(sum, array(c(1,2,3)), c(1,2,3))
three[1]
three[2]
three[3]

## 2
A <- as.matrix(mtcars)
dimnames(A) <- NULL
# a
apply(as.matrix(1:ncol(A)),MARGIN=1, FUN=function(x){cor(A[,x], A[,-x])})

colmeans <- apply(A, MARGIN=2, mean)
apply(as.matrix(1:nrow(A)),MARGIN=1, FUN=function(x){sum(A[x,] > colmeans)})

# b
colmedians <- apply(A, MARGIN=2, median)
apply(as.matrix(1:ncol(A)), MARGIN=1, FUN = function(x){A[A[,x] > colmedians[x], x]})

## 3
# a
for (i in 1:10){
  for (j in 1:i){
    print(list(i)) 
  }
}
# b
apply(as.matrix(c(1:n)), MARGIN=1,FUN=
        function(x){
          if ( x %% 2 == 0) s_nuokr = 1
          else s_nuokr = 2
          list(
            rnorm ( 
              n = imciu_turiai [x] ,
              mean = vidurkiai [x], 
              sd = s_nuokr )
            )
          }
      )

## 4
four <- function(df, r = 1) {
  maxcols <- max(1,as.integer(ncol(df)*r))
  set.seed(as.integer(r*100)) 
  pickedcols <- sample(x = 1:ncol(df),size = maxcols)
  df[, pickedcols]
}

four(mtcars, 0.4)
# Gauname qsec, cyl, gear, vs

four(mtcars, 0.3)
# Gauname gear, cyl, disp

four(iris, 0.5)
# Gauname Petal.Length, Petal.Width

four(iris, 0.9)
# Gauname Petal.Width, Sepal.Width, Species, Sepal.Length