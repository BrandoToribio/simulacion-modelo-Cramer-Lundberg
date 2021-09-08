library(ggplot2)
################################################################################
ProcesoPoissonHomogeno <- function(lambda_Nt,t){
  n <- 0
  Wn <- 0
  while(Wn <= t){
    Wn <- Wn + rexp(1,lambda_Nt)
    n <- n + 1
  }
  n
}
################################################################################
clcontinuo <- function(u,c,t,lambda_Nt,lambda_Yj){
  if (t == 0){
    Ct = u
    Ct
  }else{
    Xt <- 0
    Nt <- ProcesoPoissonHomogeno(lambda_Nt,t)
    for(j in 1:Nt){
      Xt <- Xt + rexp(1,lambda_Yj)
    }
    Ct <- u + c*t - Xt
    Ct
  }
}
################################################################################
simular <- function(u,c,t,lambda_Nt,lambda_Yj,no_simulaciones){
  val2 <- NULL
  for(i in 1:no_simulaciones){
    val1 <- NULL
    for(k in 0:t){
      val1 <- c(val1,clcontinuo(u,c,k,lambda_Nt,lambda_Yj))
    }
    val2 <- c(val2,val1)
  }
  simulaciones <- data.frame(sim = rep(c(1:no_simulaciones), each = t+1),
                             t = rep(c(0:t), no_simulaciones),
                             val = val2)
  simulaciones
}
# Simulación 1 #################################################################
grafica <- simular(0,1,50,0.9,1,10)
ggplot(grafica, aes(x=t, y=val, group=sim)) +
  geom_line(aes(color=sim)) + theme(legend.position="none") +
  scale_x_continuous(name="tiempo") +
  scale_y_continuous(name="Ct   Balance al tiempo t")
ggplot(grafica, aes(x=t, y=val, group=sim)) +
  geom_line(aes(color=sim)) + theme(legend.position="none") +
  geom_abline(intercept = 0, slope = (1 - (0.9*(1/1))), color="red",size=1) +
  scale_x_continuous(name="tiempo") +
  scale_y_continuous(name="Ct   Balance al tiempo t")
# Simulación 3 #################################################################
reclamaciones <- function(t,lambda_Nt,lambda_Yj){
  if (t == 0){
    Xt = 0
    Xt
  }else{
    Xt <- 0
    Nt <- ProcesoPoissonHomogeno(lambda_Nt,t)
    for(j in 1:Nt){
      Xt <- Xt + rexp(1,lambda_Yj)
    }
    Xt
  }
}
simular12 <- function(u,c,t,lambda_Nt,lambda_Yj,no_simulaciones){
  val2 <- NULL
  for(i in 1:no_simulaciones){
    val1 <- NULL
    val1 <- u
    for(k in 1:t){
      val1 <- c(val1, 
                val1[k] + c - reclamaciones(1,lambda_Nt,lambda_Yj))
    }
    val2 <- c(val2,val1)
  }
  simulaciones <- data.frame(sim = rep(c(1:no_simulaciones), each = t+1),
                             t = rep(c(0:t), no_simulaciones),
                             val = val2)
  simulaciones
}
grafica <- simular12(0,1,50,1,2,25)
ggplot(grafica, aes(x=t, y=val, group=sim)) +
  geom_line(aes(color=sim)) + theme(legend.position="none") +
  scale_x_continuous(name="tiempo") +
  scale_y_continuous(name="Ct   Balance al tiempo t")
# Probabilidad de Ruina por regla recursiva ####################################
psi_ruina_exponencial <- function(u,lambda,alpha,c){
  psi_ruina_exponencial <- (lambda/(alpha*c))*exp(-u*(alpha-(lambda/c)))
  psi_ruina_exponencial
}

psi_ruina_exponencial(u = 0,lambda = 0.9,alpha = 1,c = 1)
psi_ruina_exponencial(u = 8,lambda = 0.9,alpha = 1,c = 1)
psi_ruina_exponencial(u = 0,lambda = 1,alpha = 2,c = 1)
################################################################################
graficacion <- function(lambda,alpha,c,n){ #n: Monto maximo
  monto <- c(NA)
  monto[1] <- 0
  prob <- c(NA)
  prob <- psi_ruina_exponencial(0,lambda,alpha,c)
  for(i in seq(from=0.1, to=n, by=0.05)){
    monto <- c(monto,i)
    prob <- c(prob,psi_ruina_exponencial(i,lambda,alpha,c))
  }
  datos <- data.frame(x=monto, y=prob)
  print(qplot(x, y, data=datos, geom="line", 
              xlab = "Monto inicial", ylab = "Probabilidad de ruina") + 
          xlim(0, n) + ylim(0,1))
}
# Gráfica 1 ####################################################################
graficacion(lambda = 0.9,alpha = 1,c = 1,n = 35) + 
  geom_point(aes(x=0, y=0.9), colour="blue") +
  geom_point(aes(x=8, y=0.4043961), colour="red")
# Gráfica 2 ####################################################################
graficacion(lambda = 1,alpha = 2,c = 1,n = 8) + 
  geom_point(aes(x=0, y=0.5), colour="magenta")



