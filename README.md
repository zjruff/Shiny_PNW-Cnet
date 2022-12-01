# Shiny_PNW-Cnet
Shiny-based desktop application to process audio data and facilitate automatic detection and human review of vocalizations of several dozen species native to the Pacific Northwest, USA. Uses the latest version (v4) of the PNW-Cnet neural network with 51 target classes. Run through RStudio; also requires SoX.
See the document "Shiny_PNW-Cnet_installation_and_use" for detailed instructions on installation and use of the tool.

To get started, click the green Code button at the top right and choose Download ZIP, then extract the folder into your RStudio home directory.

<b>NOTES 01 Dec 2022:</b>

We have recently added some useful features to the app, including:

- More informative audio data summary

- Saving spectrograms in a custom location for faster processing and better disk space usage

- Renaming audio files from within the app

- Finer control over multiprocessing

These new features are all outlined in the user's guide, which also features a shiny new table of contents.

<b>NOTES 29 Sept 2022:</b>

The <code>reticulate::install_miniconda()</code> function is now installing a new version of conda (22.9) which may cause issues installing some packages. Installing miniconda from RStudio still works, but it then fails to create the r-reticulate package and returns an error. We recommend using the Anaconda Prompt to create the r-reticulate environment from outside RStudio using the command

<code>conda create -n r-reticulate python=3.8</code>

You can then activate the environment and install the required packages using pip as detailed in the guide.
If you run into further issues, it may be helpful to downgrade conda to the previous version (4.14; this looks like a huge jump but it's not, they just changed versioning systems). From the Anaconda Prompt, with the base environment active, run the command

<code>conda install conda=4.14</code>

and you should be all set.
