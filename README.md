# Predictive Text App

The app in action can be viewed at: https://cwaring.shinyapps.io/PredictiveText/

This was a capstone project completed as part of the [Johns Hopkins University Data Specialization](https://www.coursera.org/specializations/jhu-data-science) on Coursera.

## Motivation

This project demonstrates the use of R to for a natural language processing (NLP) application.  The goal of the project is to generate an interactive [R Shiny](https://shiny.rstudio.com) webpage to predict the next word in a text string in a fashion similar to word prediction in messaging/texting apps.

## Repo Contents
* 1.Load_Data.R - Converts raw text files into RDS format for quick loading into R

* 2.Subsample_Data.R- Subsamples the raw data to generate data for training

* 3.Process_Data.R - Processes text to generate data used for predictions

* ./TextApp/app.R - The shiny app

* ./TextApp/data/train_processed_60Set_withstop-TOP10.Rdata - Processed data used in prediction.

## Packages

* [data.table](https://cran.r-project.org/web/packages/data.table/index.html)
* [dplyr](https://cran.r-project.org/web/packages/dplyr/index.html)
* [ggplot2](http://ggplot2.org)
* [quanteda](https://cran.r-project.org/web/packages/quanteda/index.html)
* [stringr](https://cran.r-project.org/web/packages/stringr/index.html)

## Data
The model was built using english language text data collected from tweets on twitter, news stories and blog posts and supplied by the course.  The data supplied for training can be summarized as follows:

| Data Source   | File Size (MB)    | Lines of Text|
| ------------- |:-------------:| -----:|
| Twitter      | 210 | 2,360,000 |
| News      | 206      |   1,010,000 |
| Blogs | 167      |    900,000 |

## Data Processing
1. Data tables were used to improved the [speed](data table speed) of data processing.

2. To decrease the data size and improve the app speed 60% of the data was randomly sampled to use for training.

## Text processing
1. The quanteda package was used to tokenize the text and to generate n-grams (bi, tri and four).  To minimize the data size bigrams that occur at least 3 times and trigrams and four-grams that occur at least twice were kept.

2.  For each of the generate n-grams the final word was defined as a "target" next word. The remaining n-gram stem was used as the predictor.

3.  For each n-gram stem a probability for the next word was generated by dividing the number of instances of the target next word by the total instances of the stem.  A sample of a stem and it's target next words is shown below:

| Stem   | Target Next Word    | Probability of Target Being the Next Word|
|:-------------:|:-------------:|:-----:|
| top_secret      | information | 0.166 |
| top_secret      | security      |   0.166 |
| top_secret | mission      |    0.133 |
| top_secret | project      |    0.133 |
| top_secret | but      |    0.100 |
| top_secret | clearances      |    0.100 |
| top_secret | manhattan      |    0.100 |
| top_secret | stuff      |    0.100 |      

Using the above process ~500 MB of data was reduced to ~17MB.

## Model Prediction

1. Input text from the user is tokenized using the same process as used on the training data.  N-grams are generated using the final 3, 2 and 1 words of the input text.

2.  A data table containing all stems and targets is searched first for a match to N-grams generated for the user inputed text.   A match is searched in order for the trigram, bigram and unigrams respectively.

3.  The matching stem along with its target next words and their probabilities is returned.  The predicted next words and probabilities are display in a bar plot using the ggplot2 library.

## Future Work

* Perform proper cross validation of the model to characterize performance for predicting words(this was started and present in some sections).

* Single word prediction to predict a word as it is being typed based on the characters.
