# JHU-Coursera Capstone Final Project
This code represents the work that was done (without the false starts) for the Johns Hopkins University - Coursera capstone project. If you are currently taking this course please remember the plagerism policy and you should probably not look at this code.  

This code covers an implementation of the modified Kneser-Ney smoothing algorithm using the fixed discounting calculated from the word count frequency distribution.  

The functions are:
1. clean2 - This function was inspired by the clean function found [here](https://github.com/DaveVinson/cmscu-tutorial/blob/master/cmscu-tutorial.Rmd). This function processes a line of text by converting equivalent symbols (like converting all dashes to an en-dash, or all apostrophes to a standard apostrophe) and also replacing things like \@ to "at" when contextually appropriate.  
2. clean3 - This function converts a string to lower case and handles some formating that escaped clean2. It is also important for removing periods from titles and abbreviations, as periods are used for sentence detection.
3. clean_wrapper - This function calls clean2 line by line and displays a progress bar.  
4. kneserney - This function takes a data.table, the ngram number, the desired value for data.tables::setDTthreads and an option flag to pack the discount values into a list with the final data.table. This function takes a data.table with columns for the "feature" and "r" (the frequency of the feature, also known as count("feature")), and outputs a data.table with columns "root", "term", "r", "root_sum", "gamma", "pcont" and "pKN."  
   - "root" is the (n-1)-gram obtained by removing the last word from the input "feature."
   - "term" is the terminal word from the input "feature."
   - "r" is the same "r" in the input, and represents the count of the "root"-"term" pair.
   - "root_sum" is the count of all occurances of the "root."
   - "gamma" is a value used in calculating "pKN." It represents the probability taking from the discounting.
   - "pcont" is the continuation probability that is used in calculating "pKN."
   - "pKN" is the terminal word probability as calculated by the modified Kneser-Ney smoothing algorithm.
5. ngrGenerator - This function takes a list of words and an ngram length, and generates all of the n-grams from that list of words. This was inspired by code found [here](https://github.com/DaveVinson/cmscu-tutorial/blob/master/cmscu-tutorial.Rmd).
6. sentence2words - This function takes a list of sentences and returns a list of lists of words, in order, from those sentences. It includes a progress bar.
7. split_sentence - The regex for splitting was taken from [this StackOverflow post](https://stackoverflow.com/questions/46884556/split-character-vector-into-sentences). This function takes a corpus (as a character vector) and splits each document into sentences with the above indicated regex. It includes a progress bar.

The worked example shows how this can be implemented on a larger dataset by splitting the corpus and recombining after calculating the "feature" count for each fragment.
