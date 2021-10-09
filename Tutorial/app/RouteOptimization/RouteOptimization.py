import src.PyplantFEMClass.plantFEM

obj=src.PyplantFEMClass.plantFEM.routeOpt()
# Project=[WriteYourProjectDirectoryPath] (without / in the last)
# In [WriteYourProjectDirectory], please set a file named "Addresslist.txt"
obj.getOptimizedRoute(Project="Tutorial/RouteOptimization")