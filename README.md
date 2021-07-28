# buffsam
This package implements the buffered random sampling algorithm described in the paper: Kingsley, M., Kanneworff, P., & Carlsson, D. (2004). Buffered random sampling: a sequential inhibited spatial point process applied to sampling in a trawl survey for northern shrimp Pandalus borealis in West Greenland waters. Ices Journal of Marine Science, 61, 12-24.

# Algorithm in words
See the paper for more detail.

1. Create a list of all stratums.
1. Calculate the sampling density for each stratum by dividing the number of stations with stratum area.
1. Pick the stratum with the lowest sampling density as the _current stratum_.
1. Set buffering distance to sqrt((2A)/(nπ)), where A is stratum area and n is the number of stations for that stratum.
   1. If the element list is empty, restart from 4, but reduce the buffering distance by 10 %. 
   1. Pick a random element from the stratum that is not closer than the buffering distance to any other previously picked element. 
   1. If the number of stations have been picked, remove the current stratum from the stratum list and repeat from 3. Otherwise repeat from ?i?.
1. Go to 3.

## Area definitions.
The _survay area_ is devided into a number of _stratums_ which contains _elements_. Each element is denoted by a fixed point, and has a surronding area. In the West Greenland shrimp survay eachelement is a rectangle which is roughly a 2-n.mi. square.

The algorithm doesn't impose any requirements on how the elements are laid out, of how many of them are avaiable. However, an element list is needed; the algorithm cannot select arbitrary points within each stratum.
$s_1$

# Data needed

Stratum:
Straum, Number of stations, Area

Elements in stratum:
Stratum, Latitude, Longiture

# GitHub page
https://github.com/johan-ejstrud/buffsam
