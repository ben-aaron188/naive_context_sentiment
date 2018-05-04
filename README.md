# Naive context sentiment analysis


## Aim:
This R script should address the problem that several sentiment analysis scripts ignore valence
shifters (e.g. "hardly difficult", "not great at all"). For a great outline of that issue, you can
see [trinker's](https://github.com/trinker) argument and `sentimentr` package
[here](https://github.com/trinker/sentimentr).

The `sentimentr` package does a remarkable job in handling valence shifters but it requires 'good'
text data that is properly punctuated  - because the valence shifter weighting is done on "polarized
context clusters" in sentences (i.e., you get one sentiment value per sentence).

Many text data are not suitable in that pipeline because they are

- not punctuated at all (e.g., auto-generated YouTube transcripts)
- badly punctuated (e.g., data
from blogs where punctuation is not necessarily a given) 
- or because they are very brief: Twitter
data, for example, even if properly annotated for sentence-boundary-disambiguation, would return one or two sentiment values.

## Why "naive context sentiment analysis"
Our approach is based on the `sentimentr` idea of creating a "cluster" around sentiments.
Within that cluster, we then look for valence shifters (taken from the brilliant [`lexicon`](https://github.com/trinker/lexicon) package), weight the original sentiment, and returns a vector of sentiments of the size **v** (where v = number of tokens that are not punctuation marks).

Our approach does not rely on sentences and punctation and is therefore "naive" towards the broader structure texts.


**Note:** We are still developing this tool.
