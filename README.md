# The NSW Covid Forecasting Project

I created this project to primarily satisfy my curiosity about the trends of the covid cases in NSW since its 2021 wave due to the 'Delta variant,' which resulted in thousands of infections, dozens of deaths despite the multi-week lockdown across the state. 

But there is more to that. With this project, I have combined two important topics of my research and teaching career. I use a variant of a smooth transition regression to fit the seemingly exponentially trending data to make short term (up to seven-days-ahead) forecasts.

## Data and Models

I use the data obtained from the [COVID Australia](https://www.covidaustralia.com/) project. 

The main model of interest is a smooth transition model given by: $$y_t = \alpha+\beta G(t;\gamma,c)+\varepsilon_t,$$ where $y_t$ is the number of cases in a given day, denoted by $t$, and $G(t;\gamma,c) = (1+\exp(-\gamma(t-c)))^{-1}$ is the *logistic* smooth transition function, bounded by zero and one, where $\gamma$ and $c$ are the 'smoothness' and 'centrality' parameters to be estimated; $\varepsilon_t$ is an error term with the usual assumptions. These model will fit the data well in the beginning of the 'wave' but not subsequently. At some point, a better representation will be the multiple-regime smooth transition model, such as, for example: $$y_t = \alpha+\beta_1 G_1(t;\gamma_1,c_1)+\beta_2 G_2(t;\gamma_2,c_2)+\beta_{12} G_1(t;\gamma_1,c_1) G_2(t;\gamma_2,c_2)+\varepsilon_t,$$ where $c_2 > c_1$. This will, potentially, allow for a turning point in the data.

As an alternative model, I consider the exponential trend model: $y_t=\exp(\alpha+\beta t)$. This model is expected to fit the data well in the beginning of the 'wave' but not subsequently.


