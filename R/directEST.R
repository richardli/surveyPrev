#' Calculate direct estimates
#'
#' This function calculate direct estimates at given admin level.
#'
#' @param dat.tem  dataframe that contains the indicator of interests
#' @param clusterinfo dataframe that contains admin 1 and admin 2 information and coordinates for each cluster.
#' @param admininfo dataframe that contains population and urban/rural proportion at specific admin level
#' @param admin admin level for the model
#'
#' @return This function returns the dataset that contain district name and population for given  tiff files and polygons of admin level,
#'   \item { directEST.result
#' }
#' @import dplyr
#' @importFrom survey svydesign svyby
#' @author Qianyu Dong
#' @examples
#' \dontrun{
#' }
#'
#' @export


directEST <- function(dat.tem, clusterinfo, admininfo, admin ){
  if(sum(is.na(dat.tem$value))>0){
    dat.tem <- dat.tem[rowSums(is.na(dat.tem)) == 0, ]
    message("Removing NAs in indicator response")
  }
  if(admin==2){
    #prepare data
    modt<- left_join(dat.tem,cluserinfo$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]

    #model
    clusterVar = "~cluster+householdID"
    design <- survey::svydesign(ids = stats::formula(clusterVar),
                                weights = ~weight , data = modt)

    admin2_res <- survey::svyby(formula = ~value, by = ~admin2.name,
                          design = design, survey::svymean, drop.empty.groups = FALSE)
    #aggregate results
    admin1_agg<- left_join(admin2_res,admininfo,by="admin2.name")%>%
      mutate(prop=population/sum(population))%>%
      group_by(admin1.name) %>%
      summarise(weighted_avg = weighted.mean(value, prop))

    nation_agg<- left_join(admin2_res,admininfo,by="admin2.name")%>%
      mutate(prop=population/sum(population))%>%
      summarise(weighted_avg = weighted.mean(value, prop))

    return(list(admin2_res,admin1_agg,nation_agg))
    }
  else{
    modt<- left_join(dat.tem,cluserinfo$cluster.info,by="cluster")
    modt<- modt[!(is.na(modt$LONGNUM)), ]

    #model
    clusterVar = "~cluster+householdID"
    design <- survey::svydesign(ids = stats::formula(clusterVar),
                                weights = ~weight , data = modt)

    admin1_res <- survey::svyby(formula = ~value, by = ~admin1.name,
                                design = design, survey::svymean, drop.empty.groups = FALSE)
    #aggregate results

    nation_agg<- left_join(admin1_res,admininfo,by="admin1.name")%>%
      mutate(prop=population/sum(population))%>%
      summarise(weighted_avg = weighted.mean(value, prop))
    return(list(admin1_res,nation_agg))
  }

}
