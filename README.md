# OSUM
Oregon Small Urban Model

Alex Bettinardi
8-23-16

This reposistory contains Oregon's Small Urban Model, which is a 3-step trip-based travel demand model (TDM).  It is designed for areas with less than 50,000 people (sub-MPO).  Since these small areas usually have lower levels of transit service, OSUM skips the mode choice "step" in the traditional 4-step TDM.  Skipping mode choice allows this model to be very small and fast, running in minutes as opposed to hours, which is what a 4-step or ABM typically take for an MPO area.  The OSUM code is built in R, however the trip-assignment step of OSUM (the final step) uses ptv's Visum software which requires a license.  To aquire a Visum license please contact ptv - http://company.ptvgroup.com/en/home/

For more detailed instructions on how to opperate OSUM, please consult the master R script, "run_model.R" and the user guide under the "Instructions" folder.

Happy modeling!
