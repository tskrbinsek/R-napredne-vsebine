#' Title
#'
#' 
#' @param data 
#' @param area 
#'
#' @returns
#' @export
#'
#' @examples
calculate_entity_plastic = function(data, area){
  data %>% 
    filter(Entity == area) %>% 
    group_by(Fate) %>%
    summarise(Quantity = sum(Quantity)) %>% 
    mutate(Entity = area)
} 



#' Title
#'
#' @param data 
#' @param area 
#' @param naslov 
#' @param barva 
#'
#' @returns
#' @export
#'
#' @examples
draw_total_plastic <- function(data, area, 
                               naslov = "Skupne smeti: ", 
                               barva = "red"){
  data %>% 
    filter(Entity == area,
           Fate == "TotalPlastic") %>% 
    ggplot(aes(x = Year, y = Quantity)) +
    geom_line(color = barva) +
    ggtitle(paste(naslov, area))
}
