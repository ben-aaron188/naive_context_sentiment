###############################################################################
###############################################################################
### Evaluation of NCS
###############################################################################
rm(list = ls())

#load dependencies
source('/Users/bennettkleinberg/GitHub/r_helper_functions/txt_df_from_dir.R')
source('/Users/bennettkleinberg/GitHub/naive_context_sentiment/ncs.R')
source('/Users/bennettkleinberg/GitHub/r_helper_functions/get_narrative_dim.R')
require(sentimentr)

# #load file for evaluation (takes data from the sentimentr package)
# data("course_evaluations")
# data = course_evaluations
# data$id = 1:nrow(data)

data = txt_df_from_dir(dirpath = './sample_data'
                       , recursive = F
                       , include_processed = F
                       )

#ORIGINAL DATA (includes punctuation)
##syuzhet
s.syuzhet = get_narrative_dim_min(txt_input_col = data$text
                                          , txt_id_col = data$id
                                          , method = 'syuzhet'
                                          , transform_values = T
                                          , sentence_based = F
                                          , low_pass_filter_size = 5
                        )

##sentimentr
s.sentimentr = get_narrative_dim_min(txt_input_col = data$text
                                          , txt_id_col = data$id
                                          , method = 'sentimentr'
                                          , transform_values = T
                                          , sentence_based = F
                                          , low_pass_filter_size = 5
                                          )

#NON-PUNCTUATED DATA
data_np = data
data_np$text =str_replace_all(data_np$text, "[.,;:!?]", "")

##syuzhet
snp.syuzhet = get_narrative_dim_min(txt_input_col = data_np$text
                                  , txt_id_col = data_np$id
                                  , method = 'syuzhet'
                                  , transform_values = T
                                  , sentence_based = F
                                  , low_pass_filter_size = 5
)

##sentimentr
snp.sentimentr = get_narrative_dim_min(txt_input_col = data_np$text
                                     , txt_id_col = data_np$id
                                     , method = 'sentimentr'
                                     , transform_values = T
                                     , sentence_based = F
                                     , low_pass_filter_size = 5
)

##NCS
snp.ncs = ncs_full(txt_input_col = data_np$text
                   , txt_id_col = data_np$id
                   , low_pass_filter_size = 5
                   , normalize_values = F
                   , transform_values = T
                   , cluster_lead = 2
                   , cluster_lag = 2
                   , verbose = T
                   )


#plot evaluation

plot(x = 1:100
     , y = snp.ncs$`4`
     , type = 'l'
     , col = 'green')

points(snp.syuzhet$`4`
       , type = 'l'
       , col = 'red')

points(snp.sentimentr$`4`
       , type = 'l'
       , col = 'blue')

#raw sentiment evaluation
##syuzhet and sentimentr are identical but sentimentr has better debugging capab.
text.tokens = syuzhet::get_tokens(data_np$text[4], pattern = "\\W")
text.tokens_sentimentr = sentimentr::get_sentences(text.tokens)
raw_sentiment_sentimentr = sentimentr::sentiment(text.tokens_sentimentr)
raw_sentiment_sentimentr = cbind(raw_sentiment_sentimentr, text.tokens)
raw_sentiment_sentimentr

raw_sentiment_ncs = ncs_preprocess(data_np$text[4]
                                   , return_df = T)
raw_sentiment_ncs


#Example from https://github.com/trinker/sentimentr#annie-swaffords-examples

ase <- c(
  "I haven't been sad in a long time.",
  "I am extremely happy today.",
  "It's a good day.",
  "But suddenly I'm only a little bit happy.",
  "Then I'm not happy at all.",
  "In fact, I am now the least happy person on the planet.",
  "There is no happiness left in me.",
  "Wait, it's returned!",
  "I don't feel so bad after all!"
)

text.tokens_sentimentr = sentimentr::get_sentences(ase)
raw_sentiment_sentimentr = sentimentr::sentiment(text.tokens_sentimentr)
raw_sentiment_sentimentr

sentimentr_binned = get_dct_transform(raw_sentiment_sentimentr$sentiment
                                       , x_reverse_len=100
                                       , low_pass_size = 5
                                       , scale_range = T
                                       , scale_vals = F)


ncs_preprocess(ase
               , return_df = T)

ase_df = data.frame('text' = paste(ase, collapse = ' ')
                    , id = 1)
ncs_binned = ncs_full(ase_df$text
                      , ase_df$id
                      , cluster_lead = 4
                      , cluster_lag = 2)


plot(x = 1:100
     , y = ncs_binned$`1`
     , type = 'l'
     , col = 'green')

points(sentimentr_binned
       , type = 'l'
       , col = 'blue')


#note how sensitive these methods are to cluster size
ncs_binned_1_1 = ncs_full(ase_df$text
                          , ase_df$id
                          , cluster_lead = 1
                          , cluster_lag = 1)
ncs_binned_2_2 = ncs_full(ase_df$text
                      , ase_df$id
                      , cluster_lead = 2
                      , cluster_lag = 2)
ncs_binned_3_3 = ncs_full(ase_df$text
                          , ase_df$id
                          , cluster_lead = 3
                          , cluster_lag = 3)


par(mfrow=c(2,2))
plot(x = 1:100
     , y = ncs_binned$`1`
     , type = 'l'
     , col = 'green'
     , main = 'Cluster size -4 : +2 = 7')

plot(x = 1:100
     , y = ncs_binned_1_1$`1`
     , type = 'l'
     , col = 'green'
     , main = 'Cluster size -1 : +1 = 3')

plot(x = 1:100
     , y = ncs_binned_2_2$`1`
     , type = 'l'
     , col = 'green'
     , main = 'Cluster size -2 : +2 = 5')

plot(x = 1:100
     , y = ncs_binned_3_3$`1`
     , type = 'l'
     , col = 'green'
     , main = 'Cluster size -3 : +3 = 7')

par(mfrow=c(1,1))
