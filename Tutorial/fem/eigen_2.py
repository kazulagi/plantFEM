import numpy as np
import scipy.sparse
import scipy.linalg
import scipy.sparse.linalg
import pandas as pd


K = np.loadtxt("K_dense.csv",dtype="float32",delimiter=",")
M = np.loadtxt("M_dense.csv",dtype="float32",delimiter=",")

K_crs = scipy.sparse.csr_matrix(K)
M_crs = scipy.sparse.csr_matrix(M)

print(np.array_equal(K, K.T))
print(np.array_equal(M, M.T))

eig_val,eig_vec  = scipy.sparse.linalg.eigsh(K_crs,k=100,M=M_crs,which='SM')
print(eig_val )
#eig_val,eig_vec  = scipy.linalg.eigh(a=K,b=M)

sorted_val = np.sort(np.sqrt(np.abs(eig_val))/3.14159/2.0 )

#for mode_id in range(len(eig_vec[:,1])):
#    f = open("U_"+str(mode_id)+".txt","w")
#    
#    mode_vec = eig_vec [:, mode_id]
#    #mode_vec = eig_vec [mode_id, :]
#    
#    for i in range(len(mode_vec) ):
#        f.write(str(mode_vec[i])+"\n")
#    f.close()

f = open("eigen_val.txt","w")
for i in range(len(sorted_val) ):
    f.write(str(sorted_val[i] )+"\n")
    print(str(sorted_val[i] ))
f.close()
