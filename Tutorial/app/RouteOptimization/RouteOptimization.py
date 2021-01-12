import src.PySiCroFClass.SiCroF

obj=src.PySiCroFClass.SiCroF.routeOpt()
# Project=[WriteYourProjectDirectoryPath] (without / in the last)
# In [WriteYourProjectDirectory], please set a file named "Addresslist.txt"
obj.getOptimizedRoute(Project="Tutorial/RouteOptimization")