# rGaze

R package for gaze data analysis

In this package you can find some fairly simple functions to process raw gaze data and apply a velocity-based filter to identify fixations/saccades.
I use this package for some of my data analysis and in some workshops I teach on eye tracking data analysis.

It is a work in progress, based on scripts I wrote during my studies at LMU.
It has all the basic functionality:
- Apply filter to identify and group raw samples into fixations or saccades.
- Check if gaze data is inside areas of interest (AOIs)

Also included are some useful funcitons for plotting or calculating some statistics that are not available in other packages (e.g. SEM or error lines for line plots).

Please note that these scripts and any eventual package based on them come with no guarantee and you may use them at your own risk.
