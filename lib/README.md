# Project: OCR (Optical Character Recognition) 

### Code lib Folder

The lib directory contains various files with function definitions and code.

confusionMatrix.R: create a matrix with probablity of letters being mistacken as other letters  

correct.R: calculate the scores for canadates words  

detect.R:compare words in tesseract with specified digram to detect error  

Dictionary.R: create a dictionary of words with ground truth files  

differ.R: Among the error words, find the canadates words that differ from the dictionary by only one or two letters  

Digram.R: generate positional binary diagrams  

OCRtext.R: find the document and line position of each word in tesseract files  

outputtext.R: create a text file for each documents containing correctted words  

performance.R: calculate the precision from correctted words and dictionary  

topicModelling.R: Train LDA model from ground truth data  

wordsTable.R:give the document and line position of the the words  






