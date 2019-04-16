\name{USelectionPoll}
\alias{USelectionPoll}

\docType{data}
\title{
Data of an Election Survey in the USA
}
\description{
Selected numerical features of an election survey in the USA.
USelectionPoll data is a list with
\itemize{
\item{Key}{: unique decimal identifier of rows with size [1:5511]
}
\item{Data}{: The 26 features of data are described in the details section. Size [1:5511,1:26]. 
}
\item{Cls}{: vector of [1:5511] describes the answer of the participant to vote either for a rebuplican (1) or democrate (2)
}
}
}
\usage{data("USelectionPoll")}

\details{
Features of Data:

conservative:Modeled conservative -- higher values indicate more likely a conservative 

liberal:	Modeled liberal -- higher values indicate more likely a liberal
 
ownrent:	Home owner or renter -- higher values indicate more likely to be home owner, lower values 

dem_performance_pct:	Average Democratic performance percentage in precinct

turnout_pct:	Expected voter turnout percent in precinct  

age:	Age in years

earliest_reg_date:	Earliest voter registration date

obama08_minus_kerry:	Obama 2008 vote share minus Kerry 2004 vote share in precinct

obama08_pct_mean:	Obama 2008 vote share in precinct

cen_advcnddegreepcnt:	Census percent with advanced degree -- calculated by Census block group or tract (which ever was available)

cen_armedforcespct:	Census percent in armed forces -- calculated by Census block group or tract (which ever was available)

cen_avgtrvltime:	Census average travel time to work -- calculated by Census block group or tract (which ever was available)

cen_blackpcnt:	Census percent African American -- calculated by Census block group or tract (which ever was available)

cen_childrenpcnt:	Census percent children -- calculated by Census block group or tract (which ever was available)

cen_collegepcnt:	Census percent college graduate -- calculated by Census block group or tract (which ever was available)

cen_hisppcnt:	Census percent hispanic -- calculated by Census block group or tract (which ever was available)

cen_i_art_ent_rec_srvspr:	Census percent employeed in arts/entertainment/recreation industry -- calculated by Census block group or tract (which ever was available)

cen_i_farmingprct:	Census percent employed in farming/agriculture -- calculated by Census block group or tract (which ever was available)

cen_i_miningprct:	Census percent employed in mining -- calculated by Census block group or tract (which ever was available)

cen_i_utilitiesprct:	Census percent employed in utilities industry -- calculated by Census block group or tract (which ever was available)

cen_i_wholesaletrprct:	Census percent employed in whole sale -- calculated by Census block group or tract (which ever was available)

cen_medianincome:	Census median household income -- calculated by Census block group or tract (which ever was available)

cen_ruralpcnt:	Census percent rural -- calculated by Census block group or tract (which ever was available)

cen_singlemomspcnt:	Census percent single mom -- calculated by Census block group or tract (which ever was available)

cen_unemprate:	Census percent unemployed -- calculated by Census block group or tract (which ever was available)

cen_urbanpcnt:	Census percent urban -- calculated by Census block group or tract (which ever was available)

}

\references{
...
}
\examples{
data(USelectionPoll)
str(USelectionPoll)
Cls=USelectionPoll$Cls
Key=USelectionPoll$Key
Data=USelectionPoll$Data
}
\keyword{datasets}
\keyword{USelectionPoll}
