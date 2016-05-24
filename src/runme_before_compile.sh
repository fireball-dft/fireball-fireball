#!/bin/bash

MASTER_BRANCH=master

for i in a.GLOBAL b.FUNCTIONS c.SYSTEM d.FUNCTIONS_EXTRA e.FDATA g.XC_FUNCTIONALS h.SOLVESH i.GRID j.ASSEMBLERS k.DASSEMBLERS l.SCF m.MD n.NAC p.HARRIS q.DOGS o.OUTPUT u.UTIL include libs Makefile MACHINES
do
    if [ ! -e $i ]
    then
	ln -s ../../fireball-${MASTER_BRANCH}/src/$i
    else
	rm $i
	ln -s ../../fireball-${MASTER_BRANCH}/src/$i
    fi
done
