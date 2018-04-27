###############################################################################
###############################################################################
### Servant function
### Preprocessing for raw strings to contextualized list (list of context clusters).
### Context size is determined by cluster lag and cluster lead
### used in ncs.full pipeline.
### INPUT: raw string + param
### OUTPUT: list of context clusters
###############################################################################

library(lexicon)
library(stringr)
library(data.table)

ncs.preprocess = function(string_input
                          , cluster_lead = 4
                          , cluster_lag = 2
){

  mod_string = paste(string_input, collapse = ' ')
  mod_string = str_replace_all(mod_string, "[.,;:!?]", "")
  mod_string = tolower(mod_string)
  mod_string = unlist(str_split(mod_string, " "))

  #transform to data table
  text.raw = data.table(text = mod_string
                        , index = 1:length(mod_string))

  #load hash tables OPTIMIZE!
  hash.sentiment = lexicon::hash_sentiment_jockers_rinker
  hash.valence_shifters = lexicon::hash_valence_shifters

  #locate sentiments
  text.sentiment = merge(text.raw
                         , hash.sentiment
                         , by.x = 'text'
                         , by.y = 'x'
                         , all.x = TRUE)
  text.sentiment = text.sentiment[order(index),]

  #locate valence shifters
  text.valence_shifters = merge(text.sentiment
                                , hash.valence_shifters
                                , by.x = 'text'
                                , by.y = 'x'
                                , all.x = TRUE)
  text.valence_shifters = text.valence_shifters[order(index),]

  text.table = text.valence_shifters

  #recode valence shifters
  names(text.table)[3:4] = c('sentiment'
                             , 'valence_shifter')

  #define context cluster
  sentiment_indices = text.table$index[!is.na(text.table$sentiment)]
  text.table$context_cluster = ifelse(!is.na(text.table$sentiment), text.table$index, NA)
  text.table$context_cluster = sapply(text.table$context_cluster, function(x){
    transformed_col = ifelse(x %in% sentiment_indices
                             , which(x == sentiment_indices)
                             , text.table$context_cluster
    )
  })

  context.list = sapply(text.table$index, function(i){
    if(!is.na(text.table$context_cluster[i])){
      if(i - cluster_lead <= 0){
        target_index_start = 0
      } else {
        target_index_start = i - cluster_lead
      }
      if(i + cluster_lag > max(text.table$index)){
        target_index_end = max(text.table$index)
      } else {
        target_index_end = i + cluster_lag
      }
      mini_context_iter = text.table[target_index_start:target_index_end, ]
    } else {
      NULL
    }
  })

  context.list = Filter(Negate(is.null), context.list)
  return(context.list)

}
