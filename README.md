# rGaze

R package for gaze data analysis

In this package you can find some fairly simple functions to process raw gaze data and apply a velocity-based filter to identify fixations/saccades.

I use this package for some of my data analysis and in some workshops I teach on eye tracking data analysis.

It is a work in progress, based on scripts I wrote during my studies at LMU.
It has all the basic functionality:
- Apply filter to identify and group raw samples into fixations or saccades.
- Check if gaze data is inside areas of interest (AOIs)

Also included are some useful functions for plotting or calculating some statistics (e.g. SEM or error lines for line plots).

The workflow of using the functions is as follows:
1. read in a single recording raw data and assign it to a variable in R
2. apply the ivt() function, specifying which variables in the dataset correspond to the timestamp, x coordinates and y coordinates. Other important parameters of this function are the velocity threshold, output (saccades or fixations), the sampling rate of the eye tracker with which the data was recorded, and whether consecutive nearby fixations/saccades should be grouped.
3. read in an AOIs file; two types of AOIs are supported: circular and rectangular. This file should be organized with one AOI per column, with the header containing AOI names. For circular AOIs the first and second rows should contain the x and y coordinates of the center of each AOI respectively. The third row should contain the radius of AOIs. For Rectangular AOIs, first and second rows should contain the minimum and maximum X coordinates respectively. Third and fourth rows should contain minimum and maximum Y coordinates of each AOI.
4. apply the aois() function, which returns a vector of the same number of rows of the dataset passed to it with the AOI names that correspond to each row in the data.

Afterwards one can process the data to calculate the various metrics from the processed data.

Please note that these scripts and any eventual package based on them come with no guarantee and you may use them at your own risk.
