# Novel Exploration Growth (NEG)

## Introduction

Welcome to the repository for the project associated with the publication **"Novel Exploration Growth Quantifies Anxiety-like Behaviour in the Elevated Plus Maze"**. This repository contains the code and data analysis tools used in the research.

## Repository Overview

- **NEG_Calc:** Processes XY coordinate time series data into NEG time series for the total maze and by arm type (Open, Closed)
- **XYTest.csv:** CSV file for testing NEG_Calc and to show required raw data strcture including column names

## Project Description

The purpose of this project is to introduce and quantify a new metric called Novel Exploration Growth (NEG) to assess anxiety-like behavior in the Elevated Plus Maze (EPM). The NEG metric offers a detailed analysis of exploration behavior, which can be utilized in various behavioral studies.

## Key Features

- **Pre-processing:** Pre-process coordinates into Novel Exploration Growth over time
- **Bayesian Change-point Analysis (Coming Soon)** Localise change-points in each time series to evaluate phasic nature of exploration
    - This is completed primarily through the use of the [mcp](https://lindeloev.github.io/mcp/) package  
- **Bayesian Generalised Additive Model Analysis (Coming Soon)** Compare effects of interest using GAM models
    - This is completed primarily through the use of the [brms](https://paul-buerkner.github.io/brms/) and [mgcv](https://www.maths.ed.ac.uk/~swood34/mgcv/) packages   

## Usage

The code and analysis tools will be provided soon. Instructions on how to use the code and interpret the results will also be included.

## Citation

If you use this code or find the results of our research helpful, please cite our publication:

> Zelko, M; Robinson, S.R; Hill-Yardin, E, Nasser, H. "Novel Exploration Growth Quantifies Anxiety-like Behaviour in the Elevated Plus Maze,".

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

## Contact

For any questions or feedback, please open an issue in this repository or contact us at mattdzelko@gmail.com.

---
