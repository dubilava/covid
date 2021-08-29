# The NSW Covid Forecasting Project

I created this project to primarily satisfy my curiosity about the trends of the covid cases in NSW since its 2021 wave due to the 'Delta variant,' which resulted in thousands of infections, dozens of deaths despite the multi-week lockdown across the state. 

But there is more to that. With this project, I have combined two important topics of my research and teaching career. I use a variant of a smooth transition regression to fit the seemingly exponentially trending data to make short term (up to seven-days-ahead) forecasts.

## The Data and Model

I use the data obtained from the [COVID Australia](https://www.covidaustralia.com/) project, and fit the following smooth transition model to the data: $$y_t = \alpha+\beta G(t;\gamma,c)+\varepsilon_t,$$ where $y_t$ is the number of cases in a given day, denoted by $t$, and $G(t;\gamma,c) = (1+\exp(-\gamma(t-c)))^{-1}$ is the smooth transition function, bounded by zero and one, where $\gamma$ and $c$ are the 'smoothness' and 'centrality' parameters to be estimated; $\varepsilon_t$ is an error term with the usual assumptions.
