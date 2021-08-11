# buffsam
This package implements the buffered random sampling algorithm described in the paper: Kingsley, M., Kanneworff, P., & Carlsson, D. (2004). Buffered random sampling: a sequential inhibited spatial point process applied to sampling in a trawl survey for northern shrimp Pandalus borealis in West Greenland waters. Ices Journal of Marine Science, 61, 12-24.

# Algorithm in words
See the paper for more detail.

1. Create a list of all stratums, and a list of all elements.
2. Calculate the sampling density for each stratum by dividing the required number of stations with the stratum's area.
3. Pick the stratum with the lowest sampling density as the _current stratum_.
4. Set buffering distance to sqrt((2A)/(nπ)), where A is stratum area and n is the required number of stations for the current startum.
5. Pick an element at random that is not closer than the buffering distance to any other selected elements.
   1. If the element is within the current stratum, mark it as  _selected_.
   2. If the element is not within the current startum, mark it as _temporary selected_.     
6. If the required number of stations has been reached:
   1. Remove the current stratum from the stratum list.
   2. Move all the temporary selected elements back to the element list.
   3. Go to 3.
7. If the element list is empty:
   1. Move all the temporary selected elements back to the element list.
   2. Move all the selected elements from this stratum back to the element list.
   3. Reduce the buffering distance by 10 %.
   4. Go to 5.
8. Go to 5.

## Area definitions
The _survay area_ is devided into a number of _stratums_ which contains a number of _elements_. Each element is a fixed point with a surronding area. In the West Greenland shrimp survey each element is the center point of an approximately 2-n.mi. square.

The algorithm doesn't impose any requirements on how the elements are laid out, or how many of them are avaiable. However, an element list is needed; the algorithm cannot select arbitrary points within each stratum.

# Data needed
Stratum:
Straum, Number of stations, Area

Elements in stratum:
Stratum, Latitude, Longiture

# GitHub page
https://github.com/johan-ejstrud/buffsam
