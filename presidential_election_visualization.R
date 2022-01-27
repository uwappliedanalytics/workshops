library(dplyr)
library(ggplot2)
library(maps)
library(plotly)
# Load the prepared data
data <- read.csv("~/Desktop/aacuw/president_county_candidate.csv",
                 stringsAsFactors = FALSE)

# dplyr functions
dem <- data %>%
  filter(party == "DEM") %>%
  group_by(state) %>%
  summarise(TotalVotes = sum(total_votes))
rep <- data %>%
  filter(party == "REP") %>%
  group_by(state) %>%
  summarise(TotalVotes = sum(total_votes))

# determine which party won for each state
election <- data.frame("state" = dem$state, 
                        "party" = ifelse(dem$TotalVotes>rep$TotalVotes, 
                                         "Democrat", "Republican"),
                        "dem_votes" = dem$TotalVotes,
                        "rep_votes" = rep$TotalVotes)
# lat and long data frame for plotting us map
us_states <- map_data("state")
election$region <- tolower(election$state)
us_states_elec <- left_join(us_states, election)
us_states_elec$dem_votes <- formatC(us_states_elec$dem_votes, format="d", big.mark=",")
us_states_elec$rep_votes <- formatC(us_states_elec$rep_votes, format="d", big.mark=",")
p <- ggplot(data = us_states_elec,
            aes(x = long, y = lat, State = state, 
                Dem.Votes = dem_votes, Rep.Votes = rep_votes,
                group = group, fill = party))

p1 <- p + geom_polygon(color = "gray90", size = 0.1)

# fill colors of two parties
p2 <- p1 + scale_fill_manual(values = c("#0015BC","#FF0000")) +
  labs(title = "Election Results 2020", fill = NULL)
ggplotly(p2,tooltip = c("State","Dem.Votes", "Rep.Votes"))

# Looking at Washington state only
wash_dem <- data %>%
  filter(party == "DEM" & state == "Washington") %>%
  group_by(county) %>%
  summarise(TotalVotes = sum(total_votes))
wash_rep <- data %>%
  filter(party == "REP" & state == "Washington") %>%
  group_by(county) %>%
  summarise(TotalVotes = sum(total_votes))
wash_election <- data.frame("county" = wash_dem$county, 
                            "party" = ifelse(wash_dem$TotalVotes>wash_rep$TotalVotes, 
                                            "Democrat", "Republican"),
                            "dem_votes" = wash_dem$TotalVotes,
                            "rep_votes" = wash_rep$TotalVotes)
us_counties <- map_data("county")
wash_counties <- us_counties[which(us_counties$region == "washington"),]

# subregion column does not show "county" (ex. adams county -> adams),
wash_counties$county <- paste(wash_counties$subregion,"county")
wash_election$region <- tolower(wash_election$county)
wash_counties_elec <- left_join(wash_counties, wash_election, 
                                by = c("county"="region"))
wash_p <- ggplot(data = wash_counties_elec,
            aes(x = long, y = lat, County = county, 
                Dem.Votes = dem_votes, Rep.Votes = rep_votes,
                group = group, fill = party))
wash_p1 <- wash_p + geom_polygon(color = "gray90", size = 0.1)
wash_p2 <- wash_p1 + scale_fill_manual(values = c("#0015BC","#FF0000")) +
          labs(title = "Election Results 2020 in Washington State", fill = NULL)
ggplotly(wash_p2,tooltip = c("County","Dem.Votes", "Rep.Votes"))
