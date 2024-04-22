library(dplyr)
library(igraph)
library(assertthat)

ontology_tag <- c(Vocabulary = "V", VocabularyBundle = "VB", Description = "D", DescriptionBundle = "DB")

ontologies_file <- "build/tsv/ontologies.tsv"
ontology_imports_file <- "build/tsv/ontology-imports.tsv"

ontologies <- read.delim(file = ontologies_file, header = TRUE) |>
  mutate(
    tag = ontology_tag[X.o_type],
    name = paste(tag, X.o_title, sep = " ")
  )

name_map <- ontologies$name |> setNames(ontologies$X.o_iri)

ontology_imports <- read.delim(file = ontology_imports_file, header = TRUE)

imports_edges <- apply(X = ontology_imports, MARGIN = 2, FUN = function(c) name_map[c]) |>
  as_tibble() |>
  filter(!is.na(X.o_importing) & !is.na(X.o_imported)) |>
  as.matrix(ncol = 2, by_row = TRUE)

og <- graph_from_edgelist(imports_edges)
assert_that(is_dag(og), msg = "imports graph is not a directed acyclic graph")

dot <- file("build/dot/ontology-graph.dot", open = "w")

write("digraph ontology_graph {", file = dot)

write('rankdir = RL', file = dot)
write('node [shape = box]', file = dot)
write('edge [arrowhead = empty]', file = dot)

el <- as_edgelist(og) |> apply(MARGIN = 2, FUN = function(c) sprintf('"%s"', c))
write(paste(el[,1], el[,2], sep = " -> "),file = dot)

write("}", file = dot)

close(dot)