# Shiny_PNW-Cnet
Shiny-based desktop application to process audio data and facilitate automatic detection and human review of vocalizations of several dozen species native to the Pacific Northwest, USA. Uses the PNW-Cnet neural network with either 135 (v5) or 51 (v4) target classes. Runs through RStudio; also requires SoX.

See the document "Shiny_PNW-Cnet_installation_and_use" for detailed instructions on installation and use of the tool.

To get started, click the green Code button at the top right and choose Download ZIP, then extract the folder into your RStudio home directory.

<b>25 Sep 2023</b>

A few notes on installation issues:

1\. The <code>reticulate::install_miniconda()</code> function is currently failing to create the <code>r-reticulate</code> environment correctly. We recommend installing Miniconda by downloading and running the latest Windows installer from https://docs.conda.io/en/latest/miniconda.html. For greatest ease of use, set the install location to <code>C:\\Users\\[USERNAME]\\AppData\\Local\\r-miniconda</code>.

2\. You may receive an error like the following when launching the app:

<code>Error in py_run_file_impl(file, local, convert):</code>

<code>NameError: name 'base_events' is not defined</code>

This is because the latest release of the <code>reticulate</code> R package is not working correctly with the <code>asyncio</code> Python package, which is required to run TensorFlow. To get around this, you will need to install the development version of <code>reticulate</code>, which includes a fix for this issue. First download and install Rtools from https://cran.r-project.org/bin/windows/Rtools/ (use the Rtools version that matches the version of R that you are running). 

Then, in the RStudio console, install the <code>pak</code> R package by running <code>install.packages("pak")</code>, then run <code>pak::pak("rstudio/reticulate")</code>, which will download, compile and install the development version of <code>reticulate</code>. After restarting R and RStudio, the app should launch successfully.

<b>07 Aug 2023:</b>

We have a new paper describing Shiny_PNW-Cnet which was just published in the journal SoftwareX. This supersedes the preprint which was previously available through SSRN. The article is open-access and is available here: https://doi.org/10.1016/j.softx.2023.101473

Please cite this article in any publications or presentations referencing the Shiny app or the PNW-Cnet model.

<b>01 Dec 2022:</b>

We have recently added some useful features to the app, including:

- More informative summary of audio data

- Saving spectrograms in a custom location for faster processing and better disk space usage

- Renaming audio files from within the app

- Finer control over multiprocessing

- Processing speed calculated and reported in the RStudio console

These new features are all outlined in the user's guide, which has also been reorganized, including a shiny new table of contents.

To benefit from the new features, delete the Shiny_PNW-Cnet-main folder from your RStudio home directory, then download this repository and extract it into your home directory as usual. If you installed the app after August of 2022, you should already have all the required software. If not, you may want to run through the installation procedure in the user's guide and make sure everything matches what is on your machine.

<b>29 Sept 2022:</b>

The <code>reticulate::install_miniconda()</code> function is now installing a new version of conda (22.9) which may cause issues installing some packages. Installing miniconda from RStudio still works, but it then fails to create the r-reticulate package and returns an error. We recommend using the Anaconda Prompt to create the r-reticulate environment from outside RStudio using the command

<code>conda create -n r-reticulate python=3.8</code>

You can then activate the environment and install the required packages using pip as detailed in the guide.
If you run into further issues, it may be helpful to downgrade conda to the previous version (4.14; this looks like a huge jump but it's not, they just changed versioning systems). From the Anaconda Prompt, with the base environment active, run the command

<code>conda install conda=4.14</code>

and you should be all set.
