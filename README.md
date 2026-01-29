# Lacrosse-Rankings
NCAA Men's Lacrosse rankings. 

This project implements a rankings system for Division One Mens NCAA Lacrosse, inculding:
- RPI ratings
- ELO based ratings
- A combined ranking of RPI (60%) and ELO (40%) for a ranking

## Current Progress

Verision 0.1:
- Data collection from ncaa.com
- Simple RPI ratings from the end of season 2024 and 2025
- ELO model
- Combined Ranking of ELO and RPI

## How to use

1) Run D1 Lacrosse Games Scrapper.R to fetch all games for 2023, 2024, 2025 and for the games that have been played in 2026
2) Once ran, games will be saved to ncaa_lax_2023, ncaa_lax_2024, ncaa_lax_2025, and ncaa_lax_2026
3) Next, to obtain the final RPI rankings for 2023, 2024 and 2025, run the RPI calculator
    a) Rankings will be shown in the console and saved to rpi_2023, rpi_2024, and rpi_2025
    b) rpi_2026 has not been implemented yet as the first game of 2026 is January 31st
4) Following that, to obtain the ELO rankings, run the file ELO Ratings.R which will give back the ELO ratings for each year and the ELO's accuracy for each year
5) The final step is to run the Combine RPI and ELO.R file which will combine the two rankings to form one final ranking

## RPI Breakdown

- Simple RPI formula made up of:
  - 25% was based on your winning percentage
  - 50% was based on the combined winning percentages of opponents 
  - 25% was based on the combined winning percentage of a teams opponents opponents

## ELO Breakdown

- Incoming

## Combine Final Ranking

- The combined final ranking is composed of:
    - 60% RPI ranking
    - 40% ELO ranking

## Citing

- Used Claude.ai for code clean up, optimazation, and comments
