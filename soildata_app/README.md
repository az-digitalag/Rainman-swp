# Interactive exploration of RainMan soil data

The large amount of data by treatment, depth, and time hindered easy understanding of the treatment effects on soil moisture. Therefore, we developed in interactive visualization of the time series that allows:

(1) Time series of soil temperature, VWC, air temperature, and VPD, with user defined inputs of treatment, water year, season, and date range

(2) Comparison across treatments of irrigation or precipitation and VWC at each depth, with user defined inputs of treatment type, water year, and date range

(3) For the two plots instrumented with both sensors, relate SWP to VWC by depth and overlay the two time series, with user defined inputs of treatment type, water year, and date range

Each set of interactive visualizations was developed as a separate tab in the Shiny app. 

## Daily time series

For the initial landing tab, the 'Select season' UI was dynamic depending on the selected water year and the 'Select Date Range' UI was dynamic depending on the selected water year and season. All three panels were double-axis plots that required calculating the ratio between the two axes in response to the selected inputs. Toggling the date ranges affects all three panels, and sample sizes are dynamically displayed in the legend. 

Note that only summer treatments (S1, S2, S3, and S4) were incorporated here. When winter data is available, the app should be updated so that users can select combinations of summer and winter treatments. 

## Compare treatments

For the second tab, the 'Select Date Range' UI was dynamic depending on the selected water year. Here, the irrigation and precipitation inputs are plotted in the first panel for all treatments, to show the difference in frequency and amount of irrigation. The following three panels compare the average VWC response between the three soil depths. Toggling the date ranges affects all three panels, and sample sizes are dynamically displayed in the legend.

Here, either Summer or Winter treatments can be selected and compared, although the data for the winter treatments were not yet available. Future work should update the top panel to include winter irrigation treatments, and could add a check box UI that allows for comparison between Summer/Winter treatment combinations of interest. 

## Compare SWP and VWC

For the third tab, the 'Select Date Range' UI was dynamic depending on the selected water year. Here, only the paired SWP and VWC data from two plots in House 3 were utilized, representing an S1 and and S4 treatment. The first panel is a bivariate plot of SWP and VWC for the selected treatment and time range for all three depths. A dynamic tooltip was created to identify outliers in the SWP-VWC relationship. The bottom two panels show SWP and VWC as time series, with watering as the second axis, which can show hysteresis between the two sensor types. 

Since there are only two plots instrumented with both types of sensors, future winter treatments should not affect the utility of this tool, which was primarily used to identify regions of congruence between SWP and VWC as input periods for the Van Genuchten model. 