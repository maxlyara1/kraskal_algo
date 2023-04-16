kraskal <- function(matrix){
  # Устанавливаем пакеты
  install.packages('igraph')
  library('igraph')
  
  # Создаём 1 граф и веса рёбер
  g <- graph.adjacency(matrix, mode = "undirected", weighted = TRUE)
  E(g)$label <- E(g)$weight
  
  # Разукрашиваем граф в разные цвета
  colors <- c("red", "blue", "green", "purple", "orange", "gray", "pink", 
              "brown", "yellow", "cyan", '#BADBAD', '#FADFAD', '#654321','#50C878', '#560319')
  for (i in 1:(gorder(g))){
    V(g)[i]$color <- colors[i]
    set.seed(6)
    plot(g, edge.label=E(g)$label, layout=layout.circle)
    Sys.sleep(0.1)
  }
  
  # Создаём 2 граф, который будет использоваться как итоговый
  g.krask <- g
  set.seed(6)
  plot(g.krask, edge.label=E(g)$label, layout=layout.circle)
  
  
  # Пока у 1 графа есть рёбра...
  while(gsize(g)){
    # Находим номер ребра с минимальным весом
    min_num_edge <- which.min(E(g)$weight)
    # Находим вектор из вершин, которые принадлежат ребру с минимальным весом
    vert <- ends(g, min_num_edge)
    
    # Если цвет одной вершины не такой, как и у другой у рассматриваемого ребра,
    # то делаем их одного цвета
    
    if(V(g.krask)[vert[1]]$color != V(g.krask)[vert[2]]$color){
      for (i in 1:(gorder(g))){
        if (V(g.krask)[i]$color == V(g.krask)[vert[2]]$color){
          V(g.krask)[i]$color <- V(g.krask)[vert[1]]$color
        }
      }
      V(g.krask)[vert[2]]$color <- V(g.krask)[vert[1]]$color
      cat(vert[1], '-', vert[2], '\n')
      E(g.krask, P = c(vert[1],vert[2]))$color <- 'red'
    }
    
    
    # Удаляем рассмотренное ребро из 1 графа, чтобы оно не зацикливало while,
    # так, как иначе оно всегда будет минимальным
    g <- delete.edges(g, min_num_edge)
    
    
    # Рисуем 2 граф
    set.seed(6)
    plot(g.krask, edge.label=E(g)$label, layout=layout.circle)
    Sys.sleep(0.3)
  }
}


# Задаем матрицу
#Из https://neerc.ifmo.ru/wiki/index.php?title=%D0%90%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC_%D0%9A%D1%80%D0%B0%D1%81%D0%BA%D0%B0%D0%BB%D0%B0
#matr <- matrix(0, ncol = 5, nrow = 5)
#matr[1,2] <- 3
#matr[1,5] <- 1
#matr[2,5] <- 4
#matr[5,3] <- 6
#matr[2,3] <- 5
#matr[5,4] <- 7
#matr[3,4] <- 2
matr <- matrix(0, ncol = 11, nrow = 11)
matr[1,2] <- 10
matr[1,3] <- 4
matr[1,4] <- 8
matr[2,3] <- 8
matr[2,5] <- 6
matr[3,4] <- 4
matr[3,5] <- 7
matr[4,7] <- 7
matr[4,8] <- 7
matr[8,6] <- 7
matr[8,10] <- 6
matr[10,6] <- 11
matr[6,9] <- 4
matr[10,9] <- 12
matr[6,11] <- 5
matr[6,4] <- 8
matr[5,4] <- 8
matr[4,11] <- 13
matr[9,11] <- 5



kraskal(matr)


