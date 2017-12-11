\name{HyperPlaneClassifier}
\alias{HyperPlaneClassifier}
\title{ die Seite, in die der Normalenvektor W zeigt, definiert +1 Hyperplane H(x) mit Gleichung:  W*x - C ==0 Punkte auf der Hyperebene werden als positiv klassifiziert }
\arguments{
\item{Data[1:d,1:n]}{the n-dimensional data, cases in rows}
\item{Hyperplane[1:n+1]}{Hyperebene H(x) mit Gleichung:    W*x - C ==0 (hess'sche Normalform)  in  als c(W,C) Note: W ist normierter Normalenvektor! (D.h. Laenge = 1)}
\item{PlotIt}{==1 (default) plots are made}
}
\value{Cls[1:n] 				  Cls[i] = +-1 je nach Seite auf der der Datensatz liegt}
\description{
 die Seite, in die der Normalenvektor W zeigt, definiert +1 Hyperplane H(x) mit Gleichung:  W*x - C ==0 Punkte auf der Hyperebene werden als positiv klassifiziert }

