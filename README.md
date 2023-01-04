# Effect of Being a Betting Favourite on Winning UFC Fights

This project seeks to evaluate the causal effect of being a betting favourite on the outcome of Ultimate Fighting Championship (UFC) fights from 2010-2021. 

### Methodology
Regression Discontinuity Design (RDD) with regression adjustments is use to estimate the effect of being a betting favourite on a fighter’s likelihood of winning a UFC fight. 

### Assumptions
* If a fighter's odds for a particular fight is negative, he is assigned as the betting favourite. If a fighter’s odds is positive, he is assigned as the betting underdog.
* Betting odds cannot be manipulated by the fighters. Betting odds are determined by oddsmakers who usually run a statistical analysis based on each fighter’s history. 
* Randomness. Betting odds are affected by the fighters’ skills (S), while some luck (ɛ) will come from the randomness in how market makers design their odds, as well as the inherent luck and randomness in the historical performances of the fighter (e.g. how well the fighter slept before their past fights,
throwing a lucky punch, etc.)

Data Source: https://www.kaggle.com/datasets/mdabbert/ultimate-ufc-dataset?select=ufc-master.csv
