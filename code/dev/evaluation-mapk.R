apk <- function(k, actual, predicted)
{
  score <- 0.0
  cnt <- 0.0
  for (i in 1:min(k,length(predicted)))
  {
    if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
    {
      cnt <- cnt + 1
      score <- score + cnt/i 
    }
  }
  score <- score / min(length(actual), k)
  return(score)
}

mapk <- function (k, actual, predicted){
  scores <- rep(0, length(actual))
  for (i in 1:length(scores))
  {
    act <- actual[[i]][1]
    pred <- unlist(strsplit(predicted[i], ' '))
    scores[i] <- apk(k, act, pred)
  }
  score <- mean(scores)
  return(score)
}