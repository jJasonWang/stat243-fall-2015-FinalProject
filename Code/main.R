ARS <- function(n,func,start,end){ #optional: derivative
  ## the func should be a function has the form e.g.func(x){return(x^2)}
  #check func is concave
  #normalize?
  #write a function called Dev(f) to get the derivative function
  Tk = intTk(func)  #sorted?
  sample=numeric(n) #the resulting sample x*'s
  size=1
  uk = genu(Tk,f)
  while (size < n){
    x.temp = sampleXstar(uk)
    u.x = evalu(x.temp,uk)
    l.x = evall(x.temp,Tk)
    w=runif(0,1)
    if(w <= exp(l-u.x)){
      sample[size] = x.temp
      size += 1
    }else if(w <= exp(func(x.temp))-u.x){
      sample[size] = x.temp
      size += 1
      Tk = sort(c(Tk,x.temp))
      uk = genu(Tk,f)
      ##generate h.x and h.dev for checking concavity
      h.x = sapply(Tk, func)
      h.dev = sapply(Tk, Dev)
      #check concavity
    }
  }
  return(sample)
}
