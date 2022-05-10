#'@name traj-package
#'@aliases traj-package
#'@aliases traj
#'@docType package
#'@title traj: Trajectory Analysis
#'@description Implements the three step procedure proposed by Leffondree et al. (2004) to identify clusters of individual longitudinal trajectories. 
#'
#'The procedure involves (1) calculating 24 measures describing the features of the trajectories, (2) using factor analysis to select a subset of the 24 measures and (3) using cluster analysis to identify clusters of trajectories, and classify each individual trajectory in one of the clusters.
#'@details 
#'\tabular{ll}{
#'Package: \tab traj\cr
#'Version: \tab 2.0\cr
#'License: \tab MIT + file LICENSE \cr
#'LazyData: \tab true\cr
#'Depends: \tab R (>= 3.5.0)\cr
#'Imports: \tab cluster, data.table, graphics, GPArotation, grDevices, NbClust,  pastecs, psych, stats,
#'utils\cr
#'}
#'
#'
#'@author Marie-Pierre Sylvestre, Dan Vatnik, Gillis Delmas TCHOUANGUE DINKOU
#'
#'Maintainer: Gillis Delmas TCHOUANGUE DINKOU <gillisdla2@gmail.com>
#'
#'@references 
#'\enumerate{
#'  
#'  \item Sylvestre MP, et al. (2006). Classification of patterns of delirium severity scores over time in an elderly population. International Psychogeriatrics, 18(4), 667-680. doi:10.1017/S1041610206003334.
#'  
#'  \item Leffondree, K. et al. (2004). Statistical measures were proposed for identifying longitudinal patterns of change in quantitative health indicators. Journal of Clinical Epidemiology, 57, 1049-1062. doi : 10.1016/j.jclinepi.2004.02.012.
#'}
#'
#'@seealso 
#'\code{\link[NbClust]{NbClust}}
#'\code{\link[stats]{kmeans}}
#'
#'@examples 
#'
#'# Setup data and time
#'data = example.data$data
#'time = example.data$time
#'
# Run step1measures, step2factors and step3clusters
#'s1 = step1measures(data,time, ID=TRUE)
#'s2 = step2factors(s1)
#'s3 = step3clusters(s2)

# Print and plot "traj object"
#'s3
#'plot(s3)
#'
#'@keywords internal
NULL