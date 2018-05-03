###############################################################################
###############################################################################
### Evaluation of NCS
###############################################################################
rm(list = ls())

source('/Users/bennettkleinberg/GitHub/naive_context_sentiment/ncs.full.R')
source('/Users/bennettkleinberg/GitHub/naive_context_sentiment/utils/ncs.string.R')
source('/Users/bennettkleinberg/GitHub/r_helper_functions/get_narrative_dim.R')
require(sentimentr)

#load file for evaluation
data("crowdflower_deflategate")
data("crowdflower_weather")
data("course_evaluations")
data = course_evaluations
data$id = 1:nrow(data)

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

# s.sentimentr2 = get_narrative_dim_min(txt_input_col = data$text
#                                      , txt_id_col = data$id
#                                      , method = 'sentimentr'
#                                      , transform_values = T
#                                      , sentence_based = T
#                                      , low_pass_filter_size = 5
# )

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
snp.ncs = ncs.full(txt_input_col = data_np$text
                   , txt_id_col = data_np$id
                   , transform_values = T
                   , context_size_lag = 1
                   , context_size_lead = 2
                   )

a = data_np$text[1]

text.tokens = syuzhet::get_tokens(a, pattern = "\\W")
text.scored_syuzhet = get_sentiment(text.tokens)
text.scored_sentimentr = sentiment(text.tokens)$sentiment
text.contextclusters = ncs.preprocess(a)
text.scored_ncs = ncs.string(text.contextclusters)

all.equal(text.scored_sentimentr, text.scored_syuzhet)

#SET ZERO SENTIMENTS TO VALUES SO THAT SENTIMENTS = INDEX LENGTH!
#AVOID CLUSTERING WHEN NO VALENCE SHIFTERS
#DETERMINE WHICH SENTIMENT TO MODIFY