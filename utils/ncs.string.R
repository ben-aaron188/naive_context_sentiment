setwd('/Users/bennettkleinberg/GitHub/naive_context_sentiment')
source('./utils/ncs.preprocess.R')
source('./utils/ncs.modify_sentiment.R')

ncs.string = function(list_of_clusters
                      , cluster_lag = 2
                      , cluster_lead = 4
                      , standardization = T
                      , weight.amplifier = 1.5
                      , weight.advconj = 1.3
){
  weight.amplifier_ = weight.amplifier
  weight.advconj_ = weight.advconj
  new_sentiments = lapply(list_of_clusters, function(list_element){
    temp_df = list_element
    modified_sentiment_scores = ncs.modify_sentiment(list_element =  temp_df
                                                     , weight.amplifier =  weight.amplifier_
                                                     , weight.advconj =  weight.advconj_)
  })
  if(standardization == T){
    temp_list = lapply(new_sentiments, function(x){
      sum(x)/sqrt(cluster_lag+cluster_lead+1)
    })
    output_vector = unlist(temp_list)
  } else {
    output_vector = unlist(new_sentiments)
  }
  return(output_vector)
}


