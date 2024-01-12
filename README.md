# nba_data_management
This repo takes functions from `hoopR` R package and wrangles the data to assist in further development and analysis activities.  

The ESPN play-by-play is a lot easier to work with than that straight from the NBA API but does not include player participation information. There is also no way to map ESPN game IDs to NBA game IDs. These are essential for building player impact models and other types of analysis. Functions in this repo attempt to remedy this issue and yield easy to work with data.
