# Entry point: sources all component scripts in dependency order.
# Each file can also be sourced individually after sourceMeFirst_ufp.R.

source("load_ufp.R")   # SMPS_SCALE, read_smps_files(), site data frames
source("plot_ufp.R")   # plot_smps_banana(), plot_smps_conversion()
source("npf_ufp.R")    # find_modes_*, link_mode_tracks(), detect_npf_events()
