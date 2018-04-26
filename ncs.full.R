rm(list = ls())

setwd('/Users/bennettkleinberg/GitHub/naive_context_sentiment')
source('./utils/ncs.preprocess.R')
source('./utils/ncs.modify_sentiment.R')
source('./utils/ncs.string.R')


ncs.full = function(txt_input_col
                    , txt_id_col
                    , low_pass_filter_size = 5
                    , transform_values = F
                    , context_size_lag = 2
                    , context_size_lead = 4
){

  require(syuzhet)

  currentwd = getwd()
  t1 = Sys.time()

  txt_col = txt_input_col
  empty_matrix = matrix(data = 0
                        , nrow = 100
                        , ncol = length(txt_col)
  )
  for(i in 1:length(txt_col)){
    print(paste('---> naive context sentiment extraction: ', txt_id_col[i], sep=""))
    a = ncs.preprocess(string_input = txt_col[i]
                       , cluster_lead = context_size_lead
                       , cluster_lag = context_size_lag)
    text.scored = ncs.string(a)
    text.scored_binned = get_dct_transform(text.scored
                                           , x_reverse_len=100
                                           , low_pass_size = low_pass_filter_size
                                           , scale_range = transform_values
                                           , scale_vals = F)
    empty_matrix[, i] = text.scored_binned
  }
  final_df = as.data.frame(empty_matrix)
  colnames(final_df) = txt_id_col
  t2 = Sys.time()
  print(t2-t1)
  setwd(currentwd)
  return(final_df)
}


?get_dct_transform

#TODO
#- write list function that calculates correct sentiment per cluster X
#- speed issues?
#- remove stopwords
#- make NAs sparse
#- saved?



#full wrapper

##inputs:
###text columns
###context
###scaling
###to lower?
###punctuated?

# data = data.frame('text' = character(3)
#                   , 'text_id' = character(3))
# data$text = c('this is a super, great positive sentence and I just love doing this. Now this will be very negative and with disgusting words and ugly phrases'
#               , 'here we begin in a bad, bad, and ugly way but quickly become overly positive for all the great things this exciting code can do'
#               , "I haven't been sad in a long time. I am extremely happy today. It's a good day. But suddenly I'm only a little bit happy. Then I'm not happy at all. In fact, I am now the least happy person on the planet. There is no happiness left in me. Wait, it's returned! I don't feel so bad after all!")
# data$text_id = c('text1', 'text2', 'text3')
#
# my_ncs_sentiment_analysis = ncs.full(txt_input_col = data$text
#                       , txt_id_col = data$text_id
#                       , transform_values = T
#                       )
