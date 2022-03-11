#' @title Plots of DDT Species
#'
#' @param df The DDT data set.
#' @param SPECIES The given name of fish species.
#'
#' @return A plot of of LENGTH v WEIGHT and a named list of the ddt data frame.
#' @export
#'
#' @examples
#' \dontrun{myddt(df = ddt, SPECIES = "CCATFISH")}
myddt <- function(df, SPECIES) {

  # filter the data frame by a specific species
  dfsubset <- df %>% filter(SPECIES == {{SPECIES}})

  # write the data frame to csv file
  # LvsWforSPECIES.csv
  write.csv(dfsubset, paste("C:\\Users\\calwa\\OneDrive - University of Oklahoma\\Shared Folder\\Desktop\\OU\\MATH4753\\Projects\\Project1\\", "LvsWfor", SPECIES, ".csv", sep = ""), row.names = FALSE)

  # make a ggplot object with the subsetted data frame
  # LENGTH (y) v WEIGHT (x)
  g <- ggplot(dfsubset, aes_string(x = "WEIGHT", y = "LENGTH")) +
    # color the points of the plot according to the RIVER variable
    geom_point(aes_string(color = "RIVER")) +
    # place a quadratic curve over the data
    geom_smooth(formula = y~x + I(x^2), method = "lm", se = FALSE) +
    # add my name as the plot's title
    ggtitle("Cal Watson")

  # output the plot
  print(g)

  # create a relative frequency table of RIVER before subsetting
  freqtable <- df %>% group_by(RIVER) %>% summarize(n = n()) %>% mutate(freq = n / sum(n))

  # print named list of data frame before subsetting and after, as well as the above frequency table
  lst(df, dfsubset, freqtable)

}
