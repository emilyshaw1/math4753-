#' Title
#'
#' @param N Number of seats in the flight
#' @param gamma Probability a plane is truly overbooked
#' @param p Probability of a "show"
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve layout polygon
#' @importFrom stats dnorm pbinom pnorm qbinom qnorm sd
#'
#' @return 2 plots of Objective function Vs n and list containing nd, nc, N, p and gamma are outputed
#' @export
#'
#' @examples
#' ntickets(N=200, gamma = 0.02, p=0.95)
ntickets <- function(N,gamma,p)
{
  #Calculate the number of tickets to be sold
  #N: Number of seats in the flight
  #P: Probability of a "show"
  #gamma: Probability a plane is truly overbooked
  #n: Number of tickets sold

  #Plotting

  #creates a plotting region to make 4 plots
  layout(matrix(1:2, nrow=2,ncol=1))

  #creates an empty vector to put all n values into
  v<- c()

  #creates a function where we can get all min values
  f <- function(n) {
    abs(1-gamma-pbinom(N,n,p))
  }

  #puts all min values into that vector
  for(i in 1:10) {
    v[i] <- f(i+N)
  }

  #get the vector index for the minimum value
  vectormin <- which.min(v)

  #get the value of that smallest n given the index above
  min <- v[vectormin]

  #equation to get the number of tickets the discrete way
  discreteTicket <- N + (N-qbinom(1-gamma, N, p))

  #plot function I have created to make our discrete plot
  plt1 <- function(N, gamma, p) {

    #get all of the n values
    n <- seq(N, floor(N + N/10), by = 1)
    #get all the corresponding objective values to n
    Objective <- 1 - gamma - pbinom(N,n,p)

    #plotting the data that we got above and using type b to get the connecting lines
    plot(n, Objective, type = "b", xlim=range(n), ylim=range(Objective), xlab="n", ylab="Objective",
         main = "Objective Vs n to find optimal tickets sold", pch=16, col = "blue")

    #plot the horizontal red line from the min we found earlier
    abline(h = min, col = "red")

    #plot the vertical from the n we found earler
    abline(v = discreteTicket, col = "red")
  }

  #call the function to plot our graph 1
  plt1(N,gamma,p)



  #Part 2 Continuous Distribution
  continuousTicket <- N + (N - qnorm(1-gamma, N*p, sqrt(N*p*(1-p))))
  continuousTicket


  #creates an empty vector to put all n values into
  v<- c()

  #creates a function where we can get all min values
  f <- function(n) {
    abs(1-gamma-pnorm(n, n*p, sqrt(n * p * (1-p))))
  }

  #puts all min values into that vector
  for(i in 1:10) {
    v[i] <- f(i+N)
  }

  #get the vector index for the minimum value
  vectormin <- which.min(v)

  #get the value of that smallest n given the index above
  min <- v[vectormin]

  #plot function I have created to make our discrete plot
  plt2 <- function(N, gamma, p) {

    #get all of the n values
    n <- seq(N, floor(N + N/10), by = .5)

    #get all the corresponding objective values to n
    Objective <- 1-gamma-pnorm(N, n*p, sqrt(n * p * (1-p)))

    #plotting the data that we got above and using type b to get the connecting lines
    plot(n, Objective,type = "l" , xlim=range(n), ylim=range(Objective), xlab="n", ylab="Objective", main = "Objective Vs n to find optimal tickets sold", pch=16, col = "black")

    #plot the horizontal red line from the min we found earlier
    abline(h = min, col = "blue")

    #plot the vertical from the n we found earler
    abline(v = continuousTicket, col = "blue")
  }

  #call the function to plot our graph 1
  plt2(N,gamma,p)

  printList <- list(discreteTicket, continuousTicket, N, p, gamma)
  printList

}
