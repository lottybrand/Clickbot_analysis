Data and Analysis scripts for our "Clickbot" study using Qualtrics. 

About the study:
We made a simplified replication of Altay et al.'s french chatbot study that shows an increase 
in vaccination intentions and attitudes after chatting with their vaccination "chatbot." (Altay, S., Hacquin, A. S., Chevallier, C., & Mercier, H. (2021). Information delivered by a chatbot has a positive impact on COVID-19 vaccines attitudes and intentions https://psyarxiv.com/eb2gt ) Their chatbot consisted of various questions that participants could select, and the  chatbot would provide answers to. 

We translated this information into English and updated it to be more UK-specific, consistent with the latest studies up to May 2021 (avaiable here: https://drive.google.com/drive/folders/1vWti9UnpRI6T1sS7EG9HnkIzyRvVLpBh?usp=sharing ). We also had it fact-checked by a researcher in epidemiology. 
We simplified their chatbot's questions into 4 main choices consisting of 5 questions each. Participants could choose to view Q&A information about:
1) how safe vaccines are 2) how effective they are 3) whether they've been rushed 4) whether we can trust those who produce them 5) whether they are necessary for everyone

In our control condition, participants did not have a choice of information but saw 4 randomly selected dialogues. 

All dialogues were between 200-500 words in length, consisting of 2 - 3 follow-up questions each. 

** Using this repo: **
To reproduce our results, you can simply run
analysis_script.R 

To reproduce how we got our final dataframes, you can first run data_inputting.R followed by data_processing.R
