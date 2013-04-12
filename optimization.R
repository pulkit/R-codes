

genetic <-function(FUN,pc = 0.85, pm = 0.1,N,pop_length = 4,size_length = 8){
  pop_mat = matrix(ncol = 4)
  pop_mat[1,] = sample(1:128,pop_length) 
  FUN = match.fun(FUN)

  categorize<-function(pop_mat,fitness_funct){
    fitness_funct = fitness_funct
    r = nrow(pop_mat)
    sample_fitness = sample(1:100,4)    
    cumsum_fitness = cumsum(fitness_funct)
    pop_num_fit<-NULL
    for(i in 1:pop_length){
      for(j in 1:pop_length){
        if(sample_fitness[i]<=cumsum_fitness[j]){
        
        pop_num_fit = c(pop_num_fit,pop_mat[r,j])  
        break 
        }
      }
      
    }
    return(pop_num_fit)
  }
  
  number2binary <- function(number, noBits) {

    bin_mat = matrix(nrow = pop_length,ncol=8)
    for(i in 1:pop_length){
      binary_vector = rev(as.numeric(intToBits(number[i])))
      if(missing(noBits)) {
        bin_mat[i,] = binary_vector
        } 
      else {
        bin_mat[i,] = binary_vector[-(1:(length(binary_vector) - noBits))]
    }
  }
return(bin_mat)
}

binary2number <- function(x) {
  
  num = NULL
  for(i in 1:pop_length){
    
    num = c(num,sum(x[i,] * 2^(rev(seq(along=x[i,])) - 1)))
    
  }
  return(num)
}
  
cross_over<-function(pop_num_fit){
     
  position = sample(2:size_length-1,2)
  pop_index = sample(1:pop_length,pop_length)#repeat random not allowed
  bin_mat = matrix(nrow = pop_length,ncol=8)
  bin_mat = number2binary(pop_num_fit,size_length)
  bin_mat_new = matrix(nrow = pop_length,ncol=8)
  bin_mat_new = bin_mat
  j = 1
  for( i in 1:(pop_length/2)){
    if(sample(0:100,1)<100*pc){
    {
    bin_mat_new[j,] = c(bin_mat[pop_index[j],position[1]:position[2]],bin_mat[pop_index[j+1],-(position[1]:position[2])])
    j = j+1
    bin_mat_new[j,] =  c(bin_mat[pop_index[j],position[1]:position[2]],bin_mat[pop_index[j-1],-(position[1]:position[2])])
    j = j+1  
    }
  }
    else{
      j = j+2
    }
  }
  return(bin_mat_new)
}
    
mutation<-function(bin_mat_new){
    
    num_new <- binary2number(bin_mat_new)
    num_index = NULL
    for(i in 1:pop_length){
      if(sample(0:100,1)<100*pm){
      num_index <- c(num_index,i)
      }
    }
    rand_index <- sample(2:size_length-1,1)
    ifelse(bin_mat_new[num_index,rand_index]==0,bin_mat_new[num_index,rand_index]<-1,bin_mat_new[num_index,rand_index]<-0)
    num_new <- binary2number(bin_mat_new)
    return(num_new)
  }

  plot.genetic<-function(fit_sum){
    
    plot(fit_sum,xlab="Iteration",ylab="Fitness Function",type="l")
    
  }
  table.genetic<-function(pop_mat,fit_sum){
    
    gdf<-data.frame(cbind(pop_mat,fit_sum))
    
    #colnames(gdf)<-c("Species 1,Species 2,Species 3,Species 4,fitness sum")
    return(gdf)
    
  }  
  fit_sum = NULL
  fitness_funct = FUN(pop_mat[nrow(pop_mat),])
  sm = sum(fitness_funct)
  fitness_funct = (fitness_funct / sm)*100
  fit_sum = c(fit_sum,sm)
  #(fit_sum[length(fit_sum)]==fit_sum[length(fit_sum)-1] && length(unique(pop_mat[nrow(pop_mat),]))==1) ||
while( nrow(pop_mat)<=100 )
{
  fitness_funct = FUN(pop_mat[nrow(pop_mat),])
  sm = sum(fitness_funct)
  fitness_funct = (fitness_funct / sm)*100 
  fit_sum = c(fit_sum,sm)
  pop_num_fit = categorize(pop_mat,fitness_funct)
  bin_mat_new = cross_over(pop_num_fit)
  num_new = mutation(bin_mat_new) 
  pop_mat<-rbind(pop_mat,num_new)
  
  #print(pop_mat)
  
}
  plot.genetic(fit_sum)
  gdf<-table.genetic(pop_mat,fit_sum)
  print(length(fit_sum))
  print(nrow(pop_mat))
return(gdf)
  
  }