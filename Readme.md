---
title: "FiShiny"
author: "Jonas Vasconcelos"
date: '2022-02-24'
output: html_document
---

<p align="center"><a href="https://jonasvasconcelos.shinyapps.io/fishiny/?_ga=2.158772986.1906815402.1647603278-1329163440.1646699208" target="_blank"><img src="https://github.com/JonasVasconcelos/FiShiny/blob/main/www/logo.png" width="400"></a></p>

<p align="center">
<a href=""><img src="https://img.shields.io/github/downloads/JonasVasconcelos/FiShiny/total?style=social" alt="Total Downloads"></a>
</p>

# Table of Contents
1. [FiShiny](#fishiny)
2. [Description](#Description)
3. [Features](#Features)
4. [Usage](#Usage)


## FiShiny <a name="fishiny"></a>
[**FiShiny**](https://jonasvasconcelos.shinyapps.io/fishiny/?_ga=2.158772986.1906815402.1647603278-1329163440.1646699208) is a personal, non-profit and ongoing project that aims to implement and optimize processes focused on fisheries biology, such as data collection, management, and analysis.

## Description <a name="Description"></a>
Until now, this application fits 1) Length-Weight Relationship (LWR) with linear and non-linear methods, plus body growth behavior testing, and 2) Growth Models based on von Bertalanffy, Gompertz, and Logistic equations using an intuitive interface. Add, you can set how many bootstraps you want to create confidence intervals, use the Akaike criterion for model evaluation, and run comparisons between groups.

## Features <a name="Features"></>
  - Input data set from your directory
  - [GGPLOT](https://ggplot2.tidyverse.org/index.html) graphs customized with high resolution
  - Robust statistics methods
  - Download results

## Usage <a name="Usage"></a>

### Length-Weight Relationship (LWR)
1. First, on the input tab, the user uploads the length and weigth data and sets comma or semicolon separator. Review the structure and the summary data.

2. Next, write the axis labels, set the graphics parameters like color and size dots, and resolution on the Graph tab.

3. Check the linear or non-linear model box. The confidence interval was estimated by 999 bootstraps for the non-linear model and by normal distribution for the linear model. Move the level slider to set the significance level.

4. Finally, download your graph and the results. 

### Growth Models
1. First, on the input tab, the user uploads the length and age data and sets comma or semicolon separator. Review the structure and the summary data.

2. Next, write the axis labels, set the graphics parameters like color and width line or size dots, and resolution on the Graph tab.

3. Check the growth model box and how many resamples you want. Larger the bootstraps number as much more time to run. Von Bertallanfy is the easiest model to fit. Then, no need to change the start parameters values. On other hand, Gompertz needs sometimes and Logistic can be lit be harder. Keep trying moving the slider buttons until the fit curve and the fit summary appears.

4. Note that there are four tabs on your right. One for each model and one for all together. User can downloads them.

5. Finally, do comparison group using the chi-square test of likelihood ratios.

