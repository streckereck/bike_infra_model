get_sb_comfort_from_Can_BICS <- function(Can_BICS){
  case_when(
    Can_BICS %in% c("Multi-Use Path",
                    "Cycle Track",
                    "Bike Path",
                    "Local Street Bikeway") ~ "High comfort",
    Can_BICS %in% c("Painted Bike Lane") ~ "Medium/Low Comfort",
    T ~ "Non-conforming"
  )
}