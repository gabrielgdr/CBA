
jaccard <-function(G_kmeans, G_hier, K){

  mj = matrix(0, K, K)
  for (i in 0:(K))
    for (j in 0:(K)) {
      juncao = c(which(G_kmeans==i), which(G_hier == j))
      interseccao = length(juncao)-length(unique(juncao))
      uniao = length(unique(juncao))
      mj[i,j] = (interseccao)/(uniao)
    }

  return(mj)

}

moda <- function(vetor) {
  ux <- unique(vetor)
  ux[which.max(tabulate(match(vetor, ux)))]
}

entropia <- function(vetor) {
  N = length(vetor)
  Nu = length(unique(vetor))
  if (Nu == 1) return(0);

  ent = 0
  for (u in unique(vetor)) {
    p = sum(vetor==u)/N
    ent = ent -p*log(p)
  }

  norm = -log(1.0/Nu)

  (ent/norm)
}

constraintBreak <- function(i, restricoes, gruponovo) {

  if( (restricoes[i]==0) || (restricoes[i]==gruponovo) ){
    return(FALSE)
  }
  return(TRUE)
}

maisProximo <- function(data, centroides) {

  distancias = calculaDistanciaDocumentoCentroid(data, centroides)
  pos = which.max(distancias)
  return(pos)
}

recalculoDosCentroides <- function (grupos, data, K) {

  m = length(grupos)
  d = ncol(data)
  cont = rep(0,m)

  centroids = matrix(0, nrow=K, ncol=d)

  #soma e contagem de cada coluna referente a um documento e seu grupo, e a contagem de quandos doc pertencem a cada grupo
  for(i in 1:m){
    if(grupos[i]!=0){
      centroids[grupos[i],]=centroids[grupos[i],]+data[i,]
      cont[grupos[i]]=cont[grupos[i]]+1
    }
  }

  #divisao dos somatorios com a contagem de docs para cada grupo
  n = nrow(centroids)
  for(i in 1:n)
    if(cont[i]!=0)
      centroids[i,]=centroids[i,]/cont[i]


  return(centroids)

}

calculaDistanciaDocumentoCentroid <- function(doc, centroides){
  N = nrow(centroides)
  for (i in 1:N) {
    norma = sqrt(sum(centroides[i,]**2))
    if(norma>0)
      centroides[i,] = centroides[i,]/norma
  }

  dists=rep(0,N)
  N = nrow(centroides)
  for (i in 1:N)
    dists[i] = sum(doc*centroides[i,])

  return(dists)
}

avaliacao <- function(G, modelo, K) {


  N = nrow(G)
  C = ncol(G)

  ## vetor para guardar a quantidade de erros de cada agrupamento em G referente ao modelo perfeito
  resultado = c(rep(-1,N))

  for(i in 1:N){

    ## usado uma variavel auxiliar para que se calule as equivalencias de G com o conjunto original
    modelo_aux = modelo

    ## Montar as associacoes entre groupIDs
    A = assossiation(G[i,], modelo_aux,K)

    ## Ajuste dos groupIDs do modelo conforme as associacoes
    modelo_aux = ajustarAssociacoes(modelo, A)
    #modelo_aux = ajustarAssociacoes2(modelo, A,unique(G[i,]))

    ## Calculo no numero de diferencas entre os dois agrupamentos
    if (length(modelo_aux) != length(G[i,]))
      stop()

    resultado[i] = sum(modelo_aux != G[i,])
  }

  return(resultado)
}

avaliacao_single <- function(G, modelo, K) {


  N = length(G)

  for(i in 1:N){

    ## usado uma variavel auxiliar para que se calule as equivalencias de G com o conjunto original
    modelo_aux = modelo

    ## Montar as associacoes entre groupIDs
    A = assossiation(G, modelo_aux,K)

    ## Ajuste dos groupIDs do modelo conforme as associacoes
    modelo_aux = ajustarAssociacoes(modelo, A)

    ## Calculo no numero de diferencas entre os dois agrupamentos
    if (length(modelo_aux) != length(G))
      stop()

    resultado = sum(modelo_aux != G)
  }

  return(resultado)
}

ajustarAssociacoes <- function(modelo, associacao) {
  modelo_aux = rep(-1, length(modelo))

  for (i in 1:length(modelo)) {
    modelo_aux[i] = which(associacao==modelo[i])
  }
  return(modelo_aux)
}

organizavetor <- function(v){

  uv=unique(v)
  n=length(v)
  nv=rep(-1,n)

  for(i in 1:length(uv)){
    nv[v==uv[i]]=i
  }
  return(nv)
}
