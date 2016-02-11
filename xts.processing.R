
# calculate warming series from xts
yearly.hval = function(ts,col) {
  processed = ts[,col]
  minimum = min(ts[,col])
  highest = minimum
  for(i in 1:nrow(ts)){
    if(.indexyday(ts[i,col]) == 0) {
      highest = minimum
    }
    if(as.numeric(ts[i,col]) > as.numeric(highest)){
      highest = ts[i,col]
    }
    processed[i] = highest
  }
  tshval = processed
  return(tshval)
}

# calculate cooling series from xts
yearly.lval = function(ts,col) {
  processed = ts[,col]
  minimum = min(ts[,col])
  highest = minimum
  for(i in nrow(ts):1){
    if(.indexyday(ts[i, col]) == 0) {
      highest = minimum
    }
    if(as.numeric(ts[i,col]) > as.numeric(highest)){
      highest = ts[i,col]
    }
    processed[i] = highest
  }
  tslval = processed
  return(tslval)
}
