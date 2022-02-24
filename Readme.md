---
title: "FiShiny"
author: "Jonas Vasconcelos"
date: '2022-02-24'
output: html_document
---


## About FiShiny

**FiShiny** is an obvious combined name of Fish and Shiny. So, I pretend to use Shiny applications for Fish Biology. Until now, this application fits growth models based on von Bertalanffy, Gompertz, and Logistic equations using an intuitive interface. Add, you can set how many bootstraps you want to create confidence intervals, use the Akaike criterion for model evaluation, and run comparisons between groups.

1. First, on the input tab, the user uploads the length and age data and sets comma or semicolon separator. Review the structure and the summary data.

2. Next, write the axis labels, set the graphics parameters like color and width line or size dots, and resolution on the Graph tab.

3. Check the growth model box and how many resamples you want. Larger the bootstraps number as much more time to run. Von Bertallanfy is the easiest model to fit. Then, no need to change the start parameters values. On other hand, Gompertz needs sometimes and Logistic can be lit be harder. Keep trying moving the slider buttons until the fit curve and the fit summary appears.

4. Note that there are four tabs on your right. One for each model and one for all together. User can downloads them.

5. Finally, do comparison group using the chi-square test of likelihood ratios.

