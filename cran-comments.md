Build ID:

traj_1.2.tar.gz-4ee2ff4128654f61a8dc5aaf22d13738

Platform:

Ubuntu Linux 20.04.1 LTS, R-release, GCC

Submitted:

1 hour 16 minutes 18.2 seconds ago

Build time:

1 hour 34.1 seconds

WARNINGS:
* checking CRAN incoming feasibility ... WARNING
Maintainer: ‘Marie-Pierre Sylvestre <marie-pierre.sylvestre@umontreal.ca>’
 
Insufficient package version (submitted: 1.2, existing: 1.2)
 
New maintainer:
  Marie-Pierre Sylvestre <marie-pierre.sylvestre@umontreal.ca>
Old maintainer(s):
  Dan Vatnik <dan.vatnik@gmail.com>
NOTES:
* checking top-level files ... NOTE
Non-standard file/directory found at top level:
  ‘cran-comments.md’
* checking R code for possible problems ... [7s/27s] NOTE
Found if() conditions comparing class() to string:
File ‘traj/R/step2factors.R’: if (class(discard) == "character") ...
File ‘traj/R/step3clusters.R’: if (class(data) != "data.frame") ...
Use inherits() (or maybe is()) instead.
* checking examples ... [32s/119s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                user system elapsed
step3clusters 10.197  0.012  39.776
wrapperTraj   10.109  0.012  37.391
step2factors   9.609  0.009  35.417
