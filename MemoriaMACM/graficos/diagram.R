DiagrammeR::grViz("digraph {
  graph [layout = dot, rankdir = TB]
  
  node [shape = rectangle
        fontname = Helvetica
        penwidth = 2.0]        
    rec1 [label = '1. Recopilación de datos']
    rec2 [label = '2. Tratamiento de los valores atípicos (MAD)']
    rec3 [label = '3. Limpieza de la base de datos']
    rec4 [label = '4. Modelos OLS']
    rec5 [label = '5. Modelo de mejor ajuste']
  
  
  node [shape = rectangle
        penwidth = 0.5] 
    rec4_1 [label = 'Modelo Lineal']
    rec4_2 [label = 'Modelo Exponencial']
    rec4_3 [label = 'Modelo Potencial']
    
    
  # edge definitions with the node IDs
  # edge [arrowhead = diamond]
  #rec1 -> rec2; rec2 -> rec3; rec3 -> rec4; rec4 -> rec4_1; rec4 -> rec4_2; rec4 -> rec4_3; rec4_1 -> rec5; rec4_2 -> rec5; rec4_3 -> rec5;
  rec1 -> rec2 -> rec3 -> rec4; rec4 -> rec4_1 -> rec5; rec4 -> rec4_2 -> rec5; rec4 -> rec4_3 -> rec5  
  #si pongoi en el edge [label = ''], me aparece un nombre en la linea,  no en el cuadrado
  
  graph [nodesep = 0.1]
  }",
                  height = 500)

png("modelo_metodologia1.png")
dev.off()
