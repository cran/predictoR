#' @name predictoR
#' @aliases predictoR
#' @docType package
#' @title Predictive Data Analysis System
#' @author Oldemar Rodriguez Rojas \cr
#' Maintainer: Oldemar Rodriguez Rojas <oldemar.rodriguez@ucr.ac.cr>
#' @description
#' Perform a supervised data analysis on a database through a 'shiny' graphical interface. 
#' It includes methods such as K-Nearest Neighbors, Decision Trees, ADA Boosting, 
#' Extreme Gradient Boosting, Random Forest, Neural Networks, Deep Learning, 
#' Support Vector Machines and Bayesian Methods.
#' @details
#' \tabular{ll}{
#' Package: \tab predictoR\cr
#' Type: \tab Package\cr
#' Version: \tab 2.0.7\cr
#' Date: \tab 2022-04-27\cr
#' License: \tab GPL (>=2)\cr
#' }
#' @keywords package
#' @docType package

NULL
utils::globalVariables(c(
  "datos","<<-", "z", "prop", "x",
  "OOB", "cont", "MeanDecreaseAccuracy", "plot.MC", "y", "importancia", "count"))