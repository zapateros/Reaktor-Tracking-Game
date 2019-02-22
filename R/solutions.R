library(rjson)

# Part 1
signal <- readLines("distress_signal.txt")
sg     <- unlist(strsplit(signal,""))
for(i in 1:length(sg)){
  x <- sg[i:(i+15)]
  if(any(duplicated(x)) == FALSE){
    y <- paste0(x, collapse = "")
  }
}

sg_dec         <- base64enc::base64decode(y)
decoded_signal <- rawToChar(sg_dec)
cat(decoded_signal)

# Part 2
ppb_txt <- readLines("ppb.bin.log.txt")
ppb     <- unlist(strsplit(ppb_txt," "))
bit_v   <- 2^(7:0)
log     <- NULL
for(i in 1:length(ppb)){
  bin <- ppb[i]
  num <- as.numeric(unlist(strsplit(bin,"")))
  val <- sum(num * bit_v)
  cha <- rawToChar(as.raw(val))
  log <- paste0(log,cha)
}

js    <- fromJSON(log)
rsums <- NULL
n_h   <- length(js[[1]]$readings)
for(j in 1:length(js)){
  for(i in 1:n_h){
    rel_rs <- sum(unlist(js[[j]]$readings[[i]]$contaminants), na.rm=TRUE)
    rsums  <- c(rsums,rel_rs)
  }
}

ind    <- which.max(rsums)
day    <- ceiling(ind/24)
hour   <- ind %% 24
id     <- js[[day]]$readings[[hour]]$id
id_sep <- sapply(seq(1, nchar(id), by=2), function(x) substr(id, x, x+1))
place  <- rawToChar(as.raw(strtoi(id_sep, 16L)))
cat(place)

# Part 3
flood <- readLines("flood.txt")
fld   <- fromJSON(flood)
for(j in 1:length(fld$regions)){
   for(i in 1:(length(fld$regions[[1]]$readings)-1)){
    df <- fld$regions[[j]]$readings[[i]]$reading
    ds <- fld$regions[[j]]$readings[[i+1]]$reading
    chs <- sum(abs(ds - df))
    if(chs > 1000){
      char <- fld$regions[[j]]$readings[[i+1]]$readingID
      cat(char)
    }
  }
}
