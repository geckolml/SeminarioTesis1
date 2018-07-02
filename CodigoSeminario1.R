library(dplyr) 
library(Rtsne) 
library(ggplot2) # 2D Plot
library(magrittr) # Pipes
library(rbenchmark)
library(tibble)

library(ggmosaic)

library(ggplot2)
library(tidyr)
library(pryr) # Obtener memoria usada, pesos etc
library(dummies)

Data<- 
  read.csv("bank.csv")

# No se toman en cuenta:
# default, contact, day, month, duration, compaign, pdays, previous,
# poutcome
Data <- 
  Data %>% 
  select(age, job, marital, education,
         balance,# ingreso
         housing, loan,y # Tenencia de producto
         ) 



k_means_bank <- function(Data){
  
  Data2 <- dummy.data.frame(data = Data) %>% data.frame
  
  data <- scale(Data2) # Se escala cada columna del dataframe para que todas las variables
                       # tengan el mismo peso
 
  fit1 <- kmeans(data,12)
  
}

k_medoids_bank <- function(Data){
  gower_dist <- daisy(Data,
                      metric = "gower",
                      type = list(logratio = 3))
  
  gower_mat <- as.matrix(gower_dist)

    pam_fit <- pam(gower_dist, diss = TRUE, k = 12)
  
}

Tiempos <- NULL

n<-10

for(i in 1:n){
  
  DataPrueba <- 
    Data %>% 
    sample_frac(i/n)
  
  n_rows <- nrow(Data)*(i)/n
  start.time <- Sys.time()
  print("#====================================================================================================")
  print(paste("Cantidad de filas ",n_rows,sep = ""))
  print("#====================================================================================================")
  print(paste("Inicio Procesando k-means: ",start.time))
  k_means_bank(DataPrueba)
  time.kmeans <- as.numeric(difftime(Sys.time(), start.time,units = "mins"))
  print(paste("K-means tomo : ",time.kmeans))
  print("#====================================================================================================")
  
 
  start.time <- Sys.time()
  print("#====================================================================================================")
  print(paste("Inicio Procesando k-medoids: ",start.time, sep = ""))
  k_medoids_bank(DataPrueba)
  time.kmedoids <- as.numeric(difftime(Sys.time(), start.time,units = "mins"))
  print(paste("K-medoids tomo : ",time.kmedoids))
  print("#====================================================================================================")

  Tiempos <- Tiempos %>% 
    rbind(data.frame(n_rows = n_rows, 
                                 Algorithm = "k-means",
                                 Time = time.kmeans,
                     n_trial = n_rows*n/nrow(Data)),
          data.frame(n_rows = n_rows, 
                      Algorithm = "k-medoids",
                      Time = time.kmedoids,
                     n_trial = n_rows*n/nrow(Data))
          )
  
    
}

ggplot(data=Tiempos, 
       aes(x=n_trial, y=Time, group=Algorithm)) +
  geom_line(aes(linetype = Algorithm, color=Algorithm))+
  geom_point(aes(color=Algorithm)) +
  scale_linetype_manual(values=c("twodash", "dotted"))+
  scale_color_manual(values=c('#C00000','#2F4F4F'))+
  scale_size_manual(values=c(10, 10))+
  ggtitle("Rendimiento de algoritmos de Clustering") + 
  xlab("Número de ensayo(450 registros/ensayo)") +
  ylab("Tiempo (mins)") +
  theme(legend.position="top")


#==================================================================================
#==================================================================================
# K-means
  
Data2 <- dummy.data.frame(data = Data)
  
  data <- scale(Data2) # Se escala cada columna del dataframe para que todas las variables
  
  #== 
  # Hallando K
  
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:20) wss[i] <- sum(kmeans(data, 
                                       centers=i)$withinss)
  #plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares", main="Scree plot")
  
  wss_df <- data.frame(ind=1:20, wss=wss)
  
  ggplot(data=wss_df, 
         aes(x=ind, y=wss)) +
    geom_line()+
    geom_point(aes(y = wss)) +
    scale_linetype_manual(values=c("twodash", "dotted"))+
    scale_color_manual(values=c('#C00000','#2F4F4F'))+
    scale_size_manual(values=c(10, 10))+
    ggtitle("Método de suma de cuadrados dentro de clusters") + 
    xlab("Número de clusters") +
    ylab("suma de cuadrados dentro de clusters") +
    theme(legend.position="top")+
    geom_vline(xintercept = 13,color = "#C00000")
  
  
  ggplot(d_tsne_1, aes_string(x="V1", y="V2", color="Grupos")) +
    geom_point(size=1) +
    guides(colour=guide_legend(override.aes=list(size=2))) +
    xlab("") + ylab("") +
    ggtitle("Clustering con K-means") +
    theme_bw(base_size=10) +
    theme(axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          legend.direction = "horizontal", 
          legend.position = "bottom",
          legend.box = "horizontal")
  
  
 
  #===========
  
  fit1 <- kmeans(data,13)
  
  tsne_model_1 = Rtsne(as.matrix(data), check_duplicates=FALSE, 
                       pca=TRUE, perplexity=30, theta=0.5, dims=2)
  d_tsne_1 = as.data.frame(tsne_model_1$Y) 
  
  d_tsne_1$Grupos = factor(fit1$cluster)
  
  ggplot(d_tsne_1, aes_string(x="V1", y="V2", color="Grupos")) + 
    #scale_color_brewer(palette="Paired") +
    scale_color_manual(values = c("#9E0142", "#D53E4F", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#E6F598",
                                  "#ABDDA4", "#66C2A5",
                                  "#3288BD", "#5E4FA2", "#FF0000","#006600"))+
    geom_point( size=1) +   # draw points
    xlab("Comp1") + ylab("Comp2") +
    labs(title="Grupos K-means")


  Data %>% 
    cbind(GrupoKmeans=factor(fit1$cluster)) %>% 
    write.csv("Kmeans.csv",
              row.names = F)
  
#==================================================================================
#==================================================================================
# K-medoids
  
Data<- 
    read.csv("C:/Users/LENOVO/Documents/UNI/SeminarioDeTesis/AplicacionClustering/bank.csv") %>% 
    select(age, job, marital, education,
           balance,# ingreso
           housing, loan,y # Tenencia de producto
    )
  
  
  gower_dist <- daisy(Data,
                      metric = "gower",
                      type = list(logratio = 3))
  
  #================================================}
  # Hallando K 
  
  sil_width <- c(NA)
  
  for(i in 2:10){
    
    pam_fit <- pam(gower_dist,
                   diss = TRUE,
                   k = i)
    
    sil_width[i] <- pam_fit$silinfo$avg.width
    
  }
  
  sil_width_df <- data.frame(ind=1:10, sil_width=sil_width)
  
  ggplot(data=sil_width_df, 
         aes(x=ind, y=sil_width)) +
    geom_line()+
    geom_point(aes(y = sil_width)) +
    scale_linetype_manual(values=c("twodash", "dotted"))+
    scale_color_manual(values=c('#C00000','#2F4F4F'))+
    scale_size_manual(values=c(10, 10))+
    ggtitle("Método de Silhouette promedio") + 
    xlab("Número de clusters") +
    ylab("Ancho promedio de Silhouette") +
    theme(legend.position="top")+
    geom_vline(xintercept = 8,color = "#C00000")
  

  
  pam_fit <- pam(gower_dist, diss = TRUE, k = 8)
  
  
  #================================================
  
  pam_results <- Data %>%
    mutate(cluster = pam_fit$clustering) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  
  #tsne
  tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
  
  tsne_data <- tsne_obj$Y %>%
    data.frame() %>%
    setNames(c("V1", "V2")) %>%
    mutate(Grupos = factor(pam_fit$clustering))
  
  # 8 clusters para kmedoids
  ggplot(tsne_data, aes(V1, V2, col=Grupos)) + 
    scale_color_manual(values=  c("#0000FF", "#FF3399",
                                  "#ABDDA4", "#66C2A5",
                                  "#3288BD", "#5E4FA2", "#FF0000","#006600"))+
    #scale_color_brewer(palette="Dark2")+
    geom_point( size=1) +   # draw points
    xlab("Comp1") + ylab("Comp2") +
    labs(title="Grupos k-medoids")
  

  Data %>% 
    cbind(GrupoKmedoid=pam_fit$clustering) %>%
    write.csv("Kmedoids.csv",
              row.names = F)


  #===============================================================================
  # Comparar
  
  Data1<- 
    read.csv("Kmeans.csv") 
  
  Data1 <- cbind(dni=1:4521,Data1)
  
  Data2<- 
    read.csv("Kmedoids.csv")
  
  Data2 <- cbind(dni=1:4521,Data2)
  
  Data <- 
    Data1 %>% 
    left_join(
      
      Data2 %>% 
        select(dni,GrupoKmedoid)
    )
  
  
  Data %>% 
    data.frame() %>% 
    count(GrupoKmeans,GrupoKmedoid)
  
  
  Plot <- 
    table(Data$GrupoKmeans,Data$GrupoKmedoid) %>% data.frame() 

  
  table(Data$GrupoKmeans,Data$GrupoKmedoid) %>% 
    ggplot() +
    geom_mosaic(aes(weight = Freq,  x = product(Var2), fill=Var1)) +
    theme(axis.text.x=element_text(angle=0, hjust= .5))+
    labs(x="KMedoids", y=" ",  title="Kmedoids vs Kmeans")+
    guides(fill=guide_legend(title = "Kmeans", reverse = TRUE)) 
  
  ggplot(data = Plot) +
    geom_mosaic(aes(weight = Freq,  x = product(Var1), fill=Var2)) +
    theme(axis.text.x=element_text(angle=0, hjust= .5))+
    labs(x="Kmeans", y=" ",  title="Kmeans vs Kmedoids ")+
    guides(fill=guide_legend(title = "KMedoids", reverse = TRUE)) 
  
  
  
  #=============================================================================
  # Resumen de las variables
  Data %>% 
    group_by(GrupoKmeans) %>% 
    #group_by(GrupoKmedoid) %>% 
    summarise(
      CantidadClientes = n(),
      Edad = median(age),
      Perc_admin = sum(job=="admin.")/CantidadClientes,
      Perc_blue = sum(job=="blue-collar")/CantidadClientes,
      Perc_entr = sum(job=="entrepreneur")/CantidadClientes,
      Perc_hous = sum(job=="housemaid")/CantidadClientes,
      Perc_manag = sum(job=="management")/CantidadClientes,
      Perc_tech = sum(job=="technician")/CantidadClientes,
      Perc_ret = sum(job=="retired")/CantidadClientes,
      Perc_unem = sum(job=="unemployed")/CantidadClientes,
      Perc_stu = sum(job=="student")/CantidadClientes,
      
      Perc_divorced = sum(marital=="divorced")/CantidadClientes,
      Perc_married = sum(marital=="married")/CantidadClientes,
      Perc_single = sum(marital=="single")/CantidadClientes,
      
      Perc_prim = sum(education=="primary")/CantidadClientes,
      Perc_sec = sum(education=="secondary")/CantidadClientes,
      Perc_tert = sum(education=="tertiary")/CantidadClientes,
      
      balance = median(balance),
      
      Perc_housing = sum(housing=="yes")/CantidadClientes,
      
      Perc_loan = sum(loan=="yes")/CantidadClientes,
      
      Perc_depos = sum(y=="yes")/CantidadClientes
      
    ) %>% 
    data.frame() %>% 
    write.csv("Summary_Kmeans.csv",
              row.names = F)