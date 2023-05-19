## code to prepare `DATASET` iris_df goes here

data("iris")
iris_df <- iris
names(iris_df) <- c("Sepal_Length",
                 "Sepal_Width",
                 "Petal_Length",
                 "Petal_Width",
                 "Species")
iris_df$Iris_Id <- 1:nrow(iris)

usethis::use_data(iris_df, overwrite = TRUE)

