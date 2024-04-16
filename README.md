# NBA Offense Simulator Web App

bit.ly/Andrew-Vaughn

## Overview
This project is an NBA Team Simulator that allows users to create and manage rosters, simulate games, and analyze player statistics.

## Box Score Creation
The box score is created using a Monte Carlo simulation on a per-possession basis. Each possession is simulated to determine shot outcomes, turnovers, and other statistics.

## App Functionality
The app consists of three main tabs:

- **Create Roster:** Allows users to select a team and add or remove players to create a roster. Users can also adjust player minutes and simulate player swaps.
- **Simulate Games:** Enables users to simulate multiple games based on the created roster. The simulation calculates game statistics and updates the box score accordingly.
- **Player Finder:** Provides tools to filter and search for players based on various statistics such as points, assists, rebounds, etc. Users can filter players by position, salary range, age, minutes per game, and games played.

## How to Use
1. **Create Roster:**
   - Select a team from the dropdown menu.
   - Add or remove players to create your roster.
   - Adjust player minutes and simulate player swaps using the provided controls.

2. **Simulate Games:**
   - Specify the number of games to simulate using the slider.
   - Click on "Start Simulation" to begin simulating the games.

3. **Player Finder:**
   - Choose the statistics to display from the dropdown menu.
   - Filter players by position, salary range, age range, minutes per game, and games played using the sliders and checkboxes.

## Installation (locally)
To run the web app locally, you need to have R and the following R packages installed:

- shiny
- dplyr
- googlesheets4
- formattable

## Credits
This web app was co-authored by Vaughn Hajra and Andrew Mayer.
