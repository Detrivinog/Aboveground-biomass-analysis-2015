DAP<-read.csv("datos.csv")
DAP<-DAP[,1]
diametricas<-function(datos,clases){
        Abasal<-vector()
        rango=range(datos)
        intervalos=c(1,seq(5,(ceiling(rango[2]/5)*5),5))
        tab<-as.data.frame(table(cut(datos,breaks=intervalos,
                include.lowest = FALSE,right = TRUE)))
        
        AB<-vector()
        for(i in 1:length(1:length(datos)){
                for(j in 1:length(1:(length(intervalos)-1))){
                        for(k in 1:length(1:(length(intervalos)-1))){
                                if(datos[i]>=intervalos[j] & datos[i]<=intervalos[j+1]){
                                        AB[k]=((pi*(datos[i])^2)/40000)+AB[k]
                                }else{next}
                                next
                        }
                        next
                }next
        }
        tab$Sum.AB<-AB
        tab
}



diametricas(DAP)
AB<-vector()
for(i in 1:length(1:length(A.basal)){
        for(j in 1:length(1:(length(intervalos)-1))){
                for(k in 1:length(1:(length(intervalos)-1))){
                        if(A.basal[i]>=intervalos[j] & datos[i]<=intervalos[j+1]){
                                AB[k]=A.basal[i]+AB[k]
                        }
                        
                }
                
        }
        
}




for(k in 1:length(intervalos)){
        for(j in 1:(length(intervalos)-1)){
                for(i in 1:length(1:lenght(datos)){
                        if(datos[i]>=intervalos[k]&datos[i]<=intervalos[k+1]){
                                
                        }
                        
                }
                
        }    
}


