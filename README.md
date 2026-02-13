# Lacrosse-Rankings
NCAA Men's Lacrosse rankings. 

This project implements a rankings system for Division One Mens NCAA Lacrosse, inculding:
- RPI ratings
- ELO based ratings
- A combined ranking of RPI (60%) and ELO (40%) for a ranking.
      - Until there has been 8 games played in a season, the combined ranking system will mostly be ELO, until RPI becomes crediable.

## Current Progress

Verision 0.2:
- Data collection from ncaa.com for the years 2023, 2024, and 2025.
- Data collection from ESPN.com for the year 2026.
- Simple RPI ratings from the end of season 2023, 2024, and 2025.
- ELO model.
- Combined Ranking of ELO and RPI.
- Prediction of the next weeks games.

## How to use

1) Run d1_lacrosse_games_scraper.R to fetch all games for 2023, 2024, 2025.
       a) To fetch games that have been played in 2026, run espn_scraper_2026_all_games.r.
3) Once ran, games will be saved to ncaa_lax_2023, ncaa_lax_2024, ncaa_lax_2025, and ncaa_lax_2026.
4) Next, to obtain the final RPI rankings for 2023, 2024 and 2025, and to obtain the current RPI rankings for 2026, run rpi_calculator.r.
    a) Rankings will be shown in the console and saved to rpi_2023, rpi_2024, rpi_2025, and rpi_2026.
5) Following that, to obtain the ELO rankings, run the file elo_ratings.R which will give back the ELO ratings for each year and the ELO's accuracy for each year.
6) Next, run the combine_rpi_and_elo.r to obtain the combined final rankings for 2023, 2024, and 2025, and the current rankings for 2026.
7) Lastly, run predicting_the_next_weeks_games.r to predict the next weeks games. This code will scrape ESPN to obtain the next weeks games and pass in the elo ratings to then be able to provide game picks with percentages.

## RPI Breakdown

- Simple RPI formula made up of:
  - 25% was based on your winning percentage.
  - 50% was based on the combined winning percentages of opponents.
  - 25% was based on the combined winning percentage of a teams opponents opponents.

## ELO Breakdown

- The ELO formula was based on 538's ELO rating system for the NFL and the following inputs were determined to provide the best accuracy:
      - k-factor = 30
      - home field advantage = 20
      - initial elo = 1500
      - reversion = 1/5

## Combine Final Ranking

- The combined final ranking is composed of:
    - 60% RPI ranking
    - 40% ELO ranking
- 60% and 40% are the combined final ranking once there have been 8 games played in the season.
    - Before 8 games have been played, the ranking is more ELO than RPI.

## Predicting the Next Weeks Games

- The code for predicting_the_next_weeks_games.r will scrape the ESPN website to obtain the future games for the current day, for example a Wednesday, and fetch all games til the following Wednesday.
- The code will run and then spit out the date, away team, home team, away %, home %, and then the pick with their percent to win.


## Citing

- Used Claude.ai for code clean up, optimazation, and comments.
