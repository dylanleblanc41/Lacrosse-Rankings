# Lacrosse-Rankings
NCAA Men's Lacrosse rankings. 

This project implements a rankings system for Division One Mens NCAA Lacrosse, inculding:
- RPI ratings
- ELO based ratings (in progress)

## Current Progress

Verision 0.1:
- Data collection from ncaa.com
- Simple RPI ratings from the end of season 2024 and 2025
- ELO model in progress

## How to use

- Run D1 Lacrosse Games Scrapper.R to fetch all games for 2024, 2025 and for the games that have been played in 2026
- Once ran, games will be saved to ncaa_lax_2024, ncaa_lax_2025, and ncaa_lax_2026
- Next, to obtain the final RPI rankings for 2024 and 2025, run the RPI calculator
  - Rankings will be shown in the console and saved to rpi_2024 and rpi_2025
  - rpi_2026 has not been implemented yet as the first game of 2026 is January 31st
- 

## Citing

- Used Claude.ai for code clean up, optimazation, and comments
