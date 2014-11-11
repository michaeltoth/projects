##Plotting the Yield Curve 

#####Introduction

This R function allows you to plot the treasury curve historically for all dates since 1982.  Using the default graphical parameters will produce exactly the yieldOutput.gif file, but you can edit the graphical parameters to produce different styles of plot.


#####Installation

If you do not already have it installed, you will need to install the [ImageMagick](http://www.imagemagick.org/) image editor.  This is required in order to run the saveGIF function from the animation package.  After installing, you may need to update your path so that R knows where to find ImageMagick.  This happened for me automatically on Ubuntu 14.04, but this may not always be the case.

You'll also need to install any of the required packages that you do not have installed { install.packages("pkg-name") }

After taking care of the above steps, you should be able to run this script by copying into an R file and running, or by running directly line-by-line in R.
