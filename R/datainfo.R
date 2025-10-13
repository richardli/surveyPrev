#' Summarize Sample and Event Information by Administrative Level
#'
#' @description
#' Computes summary statistics on the number of sampled observations, events,
#' and unique clusters at national, admin1, and admin2 levels. The function
#' merges the input dataset with cluster- and admin-level metadata and returns
#' counts of samples, events, and clusters per geographic unit.
#'
#' @param data A data frame containing at least the variables \code{cluster} and \code{value}.
#'   Typically the raw or processed indicator data (e.g., from \code{getDHSdata()}).
#' @param cluster.info A list containing the element \code{data}, a data frame with
#'   cluster-level information. Must include a \code{cluster} column for merging.
#' @param admin.info1  A list containing the element \code{data}, a data frame
#'   with admin1-level identifiers and names. Must include the variable \code{admin1.name}.
#' @param admin.info2  A list containing the element \code{data}, a data frame
#'   with admin2-level identifiers and names. Must include the variable \code{admin2.name.full}.
#'
#' @details
#' The function performs left and right joins to associate each observation
#' in \code{data} with its cluster and administrative region. It removes rows
#' with missing values before summarizing. For each administrative level:
#' \itemize{
#'   \item \code{n_samples}: number of non-missing observations.
#'   \item \code{n_events}: number of cases where \code{value == 1}.
#'   \item \code{n_clusters}: number of distinct survey clusters.
#' }
#'
#' If \code{admin.info1} or \code{admin.info2} is \code{NULL}, the corresponding
#' summaries are omitted.
#'
#' @return
#' A list with up to three data frames:
#' \describe{
#'   \item{\code{summary.ad0}}{National-level totals across all clusters.}
#'   \item{\code{summary.ad1}}{Admin1-level summaries (if \code{admin.info1} provided).}
#'   \item{\code{summary.ad2}}{Admin2-level summaries (if \code{admin.info2} provided).}
#' }
#'
#' @examples
#' \dontrun{
#' datainfo(
#'   data = data,
#'   cluster.info = cluster.info,
#'   admin.info1 = admin.info1,
#'   admin.info2 = admin.info2
#' )
#' }
#'
#' @export


datainfo=function( data=data,
                   cluster.info=cluster.info,
                   admin.info1=admin.info1,
                   admin.info2=admin.info2){

  data <- data[rowSums(is.na(data)) == 0, ]

  if(!is.null(admin.info1)){

    modt<- left_join(data,cluster.info$data,by="cluster")%>%
      left_join(., admin.info1$data,by="admin1.name")
    modt<- modt[!(is.na(modt$admin1.name)), ]


    summary.ad1 <- modt |>
      group_by(admin1.name) |>
      summarise(
        n_samples = sum(!is.na(value)),
        n_events  = sum(value == 1, na.rm = TRUE),
        n_clusters = n_distinct(cluster),
      ) |>
      arrange(admin1.name)


  }


  if(!is.null(admin.info2)){

    modt<- left_join(data,cluster.info$data,by="cluster")%>%
      right_join(., admin.info2$data,by="admin2.name.full")


    modt<- modt[!(is.na(modt$admin2.name.full)), ]


    summary.ad2 <- modt |>
      group_by(admin2.name.full) |>
      summarise(
        n_samples = sum(!is.na(value)),
        n_events  = sum(value == 1, na.rm = TRUE),
        n_clusters = n_distinct(cluster),
      ) |>
      arrange(admin2.name.full)


  }


  summary.ad0<- modt |>
    summarise(
      n_samples = sum(!is.na(value)),
      n_events  = sum(value == 1, na.rm = TRUE),
      n_clusters = n_distinct(cluster),
    )


  return(
    list(summary.ad0=summary.ad0,
         summary.ad1=summary.ad1,
         summary.ad2=summary.ad2
    ))

}
