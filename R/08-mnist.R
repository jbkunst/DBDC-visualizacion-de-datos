library(klassets)

# data(package = "klassets")
# data(mnist_train)


url <- "https://github.com/jbkunst/random-data/raw/main/mnist/train.gz"

data <- data.table::fread(url) |> 
  as_tibble() |> 
  mutate(label = factor(label))

data
