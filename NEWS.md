# rmoo 0.2.0
* Considerable changes made to the rmoo package:

  * All the algorithms implemented in rmoo will now be called through the rmoo() 
  function, passing the strategy to be used as a parameter.
  * New s4 methods created, the functions scatter(), pcp(), heatmap() and polar() 
  will be called by the plot method and passing the object and plot type.
  * s4 methods: summary() and print() for brief verification of returned results.
  * Summary() function deprecated: Now it is executed by the s4 method progress(), 
  the method is used within the execution.
  * Other s4 methods created are getFitness(), getPopulation(), getDummyFitness(),
  getCrowdingDistance(), getMetrics(), they were created so that the user does 
  not need to directly access the object slots.
  * the structure of classes and inheritance were improved, following the rules 
  of object-oriented programming.

* Bugs Fixed: 

  * The non-dominance process was solved, an error was generated when the first 
  front was only made up of one individual.
  * ecr, cdata, dplyr, reshape2 packages are now suggested and not imported. 
  Only the packages that are exactly necessary or that generate errors if they are
  not imported are found in import.


# rmoo 0.1.8

* Minor inconvenient the documentation fixed.
* The structure of the non-dominated in the main functions modified: They did not 
evaluate fronts with one individual.
* Condition on validation into the main functions deleted: It was not necessary.

# rmoo 0.1.6

* Dataset: Add KRO dataset from TSPLIB to permutation operation in data and inst.
* Change donttest to dontrun at the documentation.
* Scatter Plotting: scatter() functions for plotting points in 2-D, 3-D and N-D 
planes, where the dimensions represent the objectives.
* Parallel Coordinate Plots: pcp() functions show a fitness set of points in an 
objectives n-dimensional space.
* Heat Map: head_map() function to show a set of fitness values as colors in two 
dimensions.
* rgl packages change to suggests in DESCRIPTION.

# rmoo 0.1.5

* The first CRAN version release.
