## Machine-Hack-Predict-Used-Car-Price

To Predict the price of Used Car based on Given dataset. 

This Project is about Predicting Used Car prices based on car parameters. 

Dataset DO not have New Car price in many of the observations, However, we feel not to exclude the variable for final prediction. Hence we decided to predict New price first - based on the avaliable New_Car Price observations around 800 dataet. ANd after that we'll use the predicted New_Car Price along with other parameters to predict actual target variable to Used_Car price. 

<br>

## Results

TO predict the Used Car Price - We came with two seperate results. 
- __[Predict 1](https://github.com/RutvijBhutaiya/Machine-Hack-Predict-Used-Car-Price/blob/master/Predict%201.csv)__ Test data Missing values for New_Price
- __[Predict 2](https://github.com/RutvijBhutaiya/Machine-Hack-Predict-Used-Car-Price/blob/master/Predict%202.csv)__ First predicted New_Prive based on prun.new.price model, and then predicted Used Car Price. 

<br>

### Learning

- Train dataset - New_Price variable (Independent) has more than 70% missing values. However, we saw there are high +ve correlation (0.72)(after study we got this value) between Price (Target variable for Study) and New_Price.
- We decided to First predict the New_Price from train dataset - CART technique. 
- After Train dataset completed, we again build CART model to predict Price (Target var), and then applied model to predict Test dataset - used car Price var. 

### Acknowledge 
Machine Hack - https://www.machinehack.com/ 
