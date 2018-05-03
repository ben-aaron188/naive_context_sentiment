###############################################################################
###############################################################################
### Servant function
### Modifies sentiment in listwise context cluster with valence shifters.
### Used in ncs pipeline.
### INPUT: list of context clusters + params
### OUTPUT: vector of modified sentiments
###############################################################################

#TODO:
#SET ZERO SENTIMENTS TO VALUES SO THAT SENTIMENTS = INDEX LENGTH!
#AVOID CLUSTERING WHEN NO VALENCE SHIFTERS
#DETERMINE WHICH SENTIMENT TO MODIFY

ncs.modify_sentiment = function(list_element
                                , weight.amplifier
                                , weight.advconj){
  list_element_df = list_element
  val_bin_checker = c(1,2,3,4) %in% list_element_df$valence_shifter

  if(any(val_bin_checker)){

    output_list = sapply(seq(1:nrow(list_element)), function(i){

      negators_post = negators_pre = amplifiers_pre = amplifiers_post = deamplifiers_pre = deamplifiers_post = advconj_pre = advconj_post = 0

      if(!is.na(list_element$sentiment[i])){

        sent_val = list_element$sentiment[i]
        pre_sub_context = list_element[1:i,]
        pre_val_bin_checker = c(1,2,3,4) %in% pre_sub_context$valence_shifter

        if(any(pre_val_bin_checker)){
          #negators before
          negators_pre = sum(pre_sub_context$valence_shifter == '1', na.rm = T)
          #amplifiers before
          amplifiers_pre = sum(pre_sub_context$valence_shifter == '2', na.rm = T)
          #deamplififers after
          deamplifiers_pre = sum(pre_sub_context$valence_shifter == '3', na.rm = T)
          #adversative conjunction before
          advconj_pre = sum(pre_sub_context$valence_shifter == '4', na.rm = T)
        }

        post_sub_context = list_element[i:nrow(list_element),]
        post_val_bin_checker = c(1,2,3,4) %in% post_sub_context$valence_shifter

        if(any(post_val_bin_checker)){
          #negators after
          negators_post = sum(post_sub_context$valence_shifter == '1', na.rm = T)
          #amplififers after
          amplifiers_post = sum(post_sub_context$valence_shifter == '2', na.rm = T)
          #deamplififers after
          deamplifiers_post = sum(post_sub_context$valence_shifter == '3', na.rm = T)
          #adversative conjunction after
          advconj_post = sum(post_sub_context$valence_shifter == '4', na.rm = T)
        }

        #calculate sentiment
        weight_deamplifier = 1-weight.amplifier+1
        weight_amplifier = weight.amplifier
        weight_advconj_before = weight.advconj
        weight_advconj_after = 1-weight.advconj+1
        negator_weight = ((-1)^negators_pre)+2
        amplifier_weight = ifelse(negator_weight %% 2 != 0, weight_deamplifier, weight_amplifier)
        deamplifier_weight = ifelse(negator_weight %% 2 != 0, weight_amplifier, weight_deamplifier)

        n_amplifiers = ifelse((amplifiers_pre+amplifiers_post) > 1, 1, (amplifiers_pre+amplifiers_post))
        n_deamplifiers = ifelse((deamplifiers_pre+deamplifiers_post) > 1, 1, (deamplifiers_pre+deamplifiers_post))
        n_advconj_before = ifelse(advconj_pre > 1, 1, advconj_pre)
        n_advconj_after = ifelse(advconj_post > 1, 1, advconj_post)

        formulaval.negators = ifelse(negator_weight %% 2 != 0, -1, 1)
        formulaval.amplifiers = ifelse(n_amplifiers > 0, n_amplifiers*amplifier_weight, 1)
        formulaval.deamplifiers = ifelse(n_deamplifiers > 0, n_deamplifiers*deamplifier_weight, 1)
        formulaval.advconj_before = ifelse(n_advconj_before > 0, n_advconj_before*weight_advconj_before, 1)
        formulaval.advconj_after = ifelse(n_advconj_after > 0, n_advconj_after*weight_advconj_after, 1)

        cluster_sent_val = sent_val*formulaval.negators*formulaval.amplifiers*formulaval.deamplifiers*formulaval.advconj_before*formulaval.advconj_after
#STORED IN MEMORY FOR MULTIPLE SENTIMENTS?
      }
    })
  } else {

    output_list = sapply(seq(1:nrow(list_element)), function(i){
#FILL NAs WITH 0
      if(!is.na(list_element$sentiment[i])){
        sent_val = list_element$sentiment[i]
      }

    })

  }

  return(unlist(output_list))
}
