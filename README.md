## Data for study is under embargo, contact Judy Shamoun-Baranes for more information
# Directory

- data
  - raw
    - raw data supplied
    - bestandnaam ***
  - processed
    - dataframe generated using code from main.R
    - bestandnaam ***
    - directory to main.R
- code
  - main.R
    - Takes csv file with titles: "device_info_serial,"date_time","longitude","latitude"
    - processing of raw data
    - calculating the spring and autumn migration onset
    - works specifically for this dataset, contact me if to use for something else
      - michaelvangompel@hotmail.com
      - Problem in GetSpringMigration
  - visualisation.R
    - needs main.R to run
    - generating box plots for migration onset
    - generating map plots for individual or grouped birds
    - generating latitude plots for individual or grouped birds
  - required_packages.R
    - checks for necessary packages and dependencies for installation
    - original from AMA-2017 course 

