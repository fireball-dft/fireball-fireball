from math import *
import numpy as np
path="/home/accounts/ahernandez/thunder7.0/fireball-fireball/tests/10.graphene/001.inp"
myfile= open(path, 'w')
a= 1.42 #a constant in Amstrongs
basis1= [0.0, 0.0, 0.0]
basis2= [a, 0.0, 0.0]
a1= [a + 0.5*a, -sqrt(3)*0.5*a, 0.0 ]
a2= [a + 0.5*a,  sqrt(3)*0.5*a, 0.0 ]
a3= [0.0, 0.0, 0.0]
myfile.write('  3            \n')
myfile.write("  %f    %f    %f\n" %( a1[0], a1[1], a1[2]))
myfile.write("  %f    %f    %f\n" %( a2[0], a2[1], a2[2]))
myfile.write("  %f    %f    %f\n" %( a3[0], a3[1], a3[2]))
myfile.write("  1\n")
myfile.write("  0.000  0.000  0.000   1.000\n")
for n in range(1):
    for m in range(1):
        posi1= np.add(basis1, np.add(np.multiply(n,a1), np.multiply(m,a2)))
        posi2= np.add(basis2, np.add(np.multiply(n,a1), np.multiply(m,a2)))
        myfile.write("  6    %f    %f    %f\n" %( posi1[0], posi1[1], posi1[2]))
        myfile.write("  6    %f    %f    %f\n" %( posi2[0], posi2[1], posi2[2]))
        
