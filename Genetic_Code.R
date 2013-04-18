#Defined a function which take fitness fuction, pc, pm, population length as input
#size_length represents number of bits
genetic <-function(FUN,pc, pm ,pop_length,size_length)
 
  {
      
      pop_mat = matrix(ncol = pop_length)
      # function "smaple()" is used for random number generation between 1 to 128
      n <- 2^size_length
      pop_mat[1,] = sample(1:n,pop_length) 
      # match function can match the function which is given as an input
      FUN = match.fun(FUN)
      
      # Catgorize function will check Conduct Selection using roulette wheel
      # Note fitness_funct is defind below 
      categorize<-function(pop_mat,fitness_funct)
        {
    
               r = nrow(pop_mat)
               sample_fitness = sample(1:100,pop_length)
               #cumsum is used for summing the scale and making interval
               cumsum_fitness = cumsum(fitness_funct)
               
               pop_num_fit<-NULL
               # Loop for checking that random generated number lies in which categories
               for(i in 1:pop_length)
                 {
                     for(j in 1:pop_length)
                       {
                         if(sample_fitness[i] <= cumsum_fitness[j])
                           {
                             pop_num_fit = c(pop_num_fit,pop_mat[r,j])  
                             break 
                           }
                        }
        
                 }
                # Returning the fittest fittest vector for further process
                return(pop_num_fit)
          }
        #######################################################################
      
        # This function is written for converting decimal number into binary number
        number2binary <- function(number, noBits)
         {

               bin_mat = matrix(nrow = pop_length,ncol=size_length)
               for(i in 1:pop_length)
                 {
                    binary_vector = rev(as.numeric(intToBits(number[i])))
                    if(missing(noBits)) 
                      {
                         bin_mat[i,] = binary_vector
                      } 
                    else
                      {
                         bin_mat[i,] = binary_vector[-(1:(length(binary_vector) - noBits))]
                      }
                  }
               return(bin_mat)
          }
        ###########################################################################
      
        # Tihs function convert binary number back into decimal number
        binary2number <- function(x) 
          {
  
                num = NULL
                for(i in 1:pop_length)
                  {
    
                     num = c(num,sum(x[i,] * 2^(rev(seq(along=x[i,])) - 1)))
    
                   }
                return(num)
           }
  
        ###########################################################################
      
        # Function defined for the crossover 
        
        cross_over<-function(pop_num_fit)
          {
     
                 position = sample(2:size_length-1,2)
                 # Note:- Repeated random number is not allowed in this case
                 pop_index = sample(1:pop_length,pop_length)
                 bin_mat = matrix(nrow = pop_length,ncol=size_length)
                 bin_mat = number2binary(pop_num_fit,size_length)
                 bin_mat_new = matrix(nrow = pop_length,ncol=size_length)
                 bin_mat_new = bin_mat
                 j = 1
                 #Loop for Swaping at randomly generated point
                 for( i in 1:(pop_length/2))
                   {
                      # Condition to check randomly selected pairs of speceis satisfy crossover or not
                      if(sample(0:100,1)<100*pc)
                        {
   
                            bin_mat_new[j,] = c(bin_mat[pop_index[j],position[1]:position[2]],bin_mat[pop_index[j+1],-(position[1]:position[2])])
                            j = j+1
                            bin_mat_new[j,] =  c(bin_mat[pop_index[j],position[1]:position[2]],bin_mat[pop_index[j-1],-(position[1]:position[2])])
                            j = j+1  
   
                         }
                      else
                        {
                            j = j+2
                        }
                    }
                  # After Crossover repeat a new vactor named bin_mat_new
                  return(bin_mat_new)
            }
       ###########################################################################
       # Function Defined for mutation process
       mutation<-function(bin_mat_new)
           {
    
                num_new <- binary2number(bin_mat_new)
                num_index = NULL
                # Loop for mutation
                for(i in 1:pop_length)
                  {  
                    # Condition for mutation is checked
                     if(sample(0:100,1)<100*pm)
                       {
                          num_index <- c(num_index,i)
                       }
                  }
                rand_index <- sample(2:size_length-1,1)
                ifelse(bin_mat_new[num_index,rand_index]==0,bin_mat_new[num_index,rand_index]<-1,bin_mat_new[num_index,rand_index]<-0)
                num_new <- binary2number(bin_mat_new)
                # After mutation returning a new vacter 
                return(num_new)
            }
      
      ##############################################################################
      # Function for plotting the graph
      plot.genetic<-function(fit_sum)
           {
               par(bg="thistle")
    
               plot(fit_sum,xlab="Iteration",ylab="Fitness Function",type="l", col="brown")
    
            }
      
      ################################################################################
      # Table for arranging the output of x in each generation
      table.genetic<-function(pop_mat,fit_sum)
           {
    
    
                gdf<-data.frame (cbind(pop_mat,fit_sum),row.names = NULL)
                return(gdf)
    
            } 
       ################################################################################
       
       # scaling of fitness function on 100 which is provided by user   
       fitness_funct = FUN(pop_mat[nrow(pop_mat),])
       sm = sum(fitness_funct)
       fitness_funct = (fitness_funct / sm)*100
       # Here I assumed that first element of the fit_sum will be greater than sum of other 
       # element of fit_sum as I used this thing for termination condition of loop
       fit_sum= 100* sm
       fit_sum = c(fit_sum,sm)
 
       ###############################################################################
       
       # Loop for Continuing Iteration till Termination Condition will met
       # Unique is used for extracting number of unique number in a row
       # To avoid infinite loop max cap for number of iteration is supposed to be 100
       while((length(unique(pop_mat[nrow(pop_mat),]))!=1 || fit_sum[length(fit_sum)]< fit_sum[length(fit_sum)-1])  && nrow(pop_mat)<100  ) 
            {
               # Calling different function one by one
               pop_num_fit = categorize(pop_mat,fitness_funct)
               bin_mat_new = cross_over(pop_num_fit)
               num_new = mutation(bin_mat_new) 
               pop_mat<-rbind(pop_mat,num_new)
               # Scaling of Fitness fuction
               fitness_funct = FUN(pop_mat[nrow(pop_mat),])
               sm = sum(fitness_funct)
               fitness_funct = (fitness_funct / sm)*100 
               fit_sum = c(fit_sum,sm)
  
  
  
            }
         
        #########################################################################
          plot.genetic(fit_sum[-1])
          gdf<-table.genetic(pop_mat,fit_sum[-1])
  
          return(gdf)
  
    }