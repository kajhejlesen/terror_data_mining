library(kohonen)

som_map <- som(target_matrix_norm, grid = somgrid(xdim = 12, ydim = 12))
plot(som_map, type = "counts")
