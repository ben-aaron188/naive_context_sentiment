###############################################################################
###############################################################################
### Debugging and review of NCS
###############################################################################
rm(list = ls())

#load dependencies
source('/Users/bennettkleinberg/GitHub/r_helper_functions/txt_df_from_dir.R')

setwd('/Users/bennettkleinberg/GitHub/naive_context_sentiment')
source('./ncs.R')

#load sample data
text_data = txt_df_from_dir(dirpath = './sample_data'
                            , recursive = F
                            , include_processed = F
                            )

dim(text_data)
names(text_data)
View(text_data)

#perform open NCS processing (this returns a dataframe to see the processing steps more clearly)
system.time(
  ncs_preprocess(string_input = text_data$text[1]
                 , cluster_lower_ = 2
                 , cluster_upper_ = 4
                 , return_df = T
                 # WEIGHTS ARE SET TO DEFAULT
                 # , weight_negator_
                 # , weight_amplifier_
                 # , weight_deamplifier_
                 # , weight_advcon_
                 , verbose = F
  )
)

text_processed = ncs_preprocess(string_input = text_data$text[1]
                                , cluster_lower_ = 2
                                , cluster_upper_ = 2
                                , return_df = T
                                # WEIGHTS ARE SET TO DEFAULT
                                # , weight_negator_
                                # , weight_amplifier_
                                # , weight_deamplifier_
                                # , weight_advcon_
                                , verbose = T
                                )


#run on data frame with sample data
binned_sentiments = ncs_full(txt_input_col = text_data$text
                             , txt_id_col = text_data$id
                             , low_pass_filter_size = 5
                             , transform_values = T
                             , normalize_values = F
                             , min_tokens = 10
                             , cluster_lower = 2
                             , cluster_upper = 2
                             # WEIGHTS ARE SET TO DEFAULT
                             # , weight_negator
                             # , weight_amplifier
                             # , weight_deamplifier
                             # , weight_advcon
                             , bins = 100
                             , verbose = F
                             )