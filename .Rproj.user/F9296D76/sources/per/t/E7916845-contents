assossiation <- function(c1, c2, K){

  A = rep(NA,K)
  J = jaccard(c1,c2,K)

  J[is.nan(J)] = 0.0

  for(i in 1:K){
    p = which(J==max(J), arr.ind=TRUE)[1,1] #linha do max
    q = which(J==max(J), arr.ind=TRUE)[1,2] #coluna do max
    A[p]=q
    J[p,]=-Inf
    J[,q]=-Inf
  }

  return(A)
}

constraint <- function(A, c1, c2, d){

  n = length(c1)
  R = rep(0,n)

  for(x in 1:n){
    i = c1[x]
    j = c2[x]

    if(j==A[i])
      R[x]=i
  }

  r0 = which(R==0, arr.ind=TRUE)
  nrp=sum(R==0)
  restricoes_maisproximas = rep(0,nrp)
  R[is.na(R)]=0
  for(i in 1:length(r0)){

    daux = d
    for(j in 1:n){
      daux[j,j]=Inf
    }

    p=1
    while(p<nrp){
      pos = which.max(-daux[r0[i],])

      restricoes_maisproximas[p] = R[pos]
      p=p+1

      daux[r0[i],pos] = Inf
      daux[pos,r0[i]] = Inf

    }

    restricoes_maisproximas[is.na(restricoes_maisproximas)]=0
    ent = entropia(restricoes_maisproximas[restricoes_maisproximas!=0])

    if(ent <= 0.9) {
      m = moda(restricoes_maisproximas[restricoes_maisproximas!=0])
      R[r0[i]] = m
    }

  }

  return(R)
}

mcopkmeans <- function (data, R, K) {

  N = nrow(data)
  d = ncol(data)
  R[is.na(R)]=0
  grupos = R
  centroids = recalculoDosCentroides(grupos, data, K)

  existe_alteracao = TRUE

  while ( existe_alteracao == TRUE )  {
    existe_alteracao = FALSE
    for (i in 1:N) {
      gruponovo = maisProximo(data[i,], centroids)
      if (gruponovo != grupos[i]) {
        if (constraintBreak(i, R, gruponovo) == FALSE) {
          existe_alteracao = TRUE
          grupos[i] = gruponovo
        }
      }
    }
    centroids = recalculoDosCentroides(grupos, data,K)

  }

  return(grupos)
}


