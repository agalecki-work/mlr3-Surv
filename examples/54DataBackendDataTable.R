library(mlr3)
library(data.table)

# Prepare data with ID in reverse order
data <- data.table(
  ID = 1:5,  # has to be numeric
  feature1 = c(5.1, 4.9, 4.7, 4.6, 5.0),
  feature2 = c(3.5, 3.0, 3.2, 3.1, 3.6),
  class = factor(c("setosa", "setosa", "setosa", "versicolor", "versicolor"))
)

# Create a DataBackend with the ID as primary key
backend <- DataBackendDataTable$new(data, primary_key = "ID")

# Use the backend to create a classification task
task <- TaskClassif$new(id = "iris_example",
                        backend = backend,
                        target = "class")

# Check task summary
print(task)

# Retrieve and print the entire backend data. Use task$row_ids directly, because they naturally match ID ordering.
backend_data <- task$backend$data(
  rows = task$row_ids,  # Directly use the ID column
  cols = colnames(data)
)

print(backend_data)
