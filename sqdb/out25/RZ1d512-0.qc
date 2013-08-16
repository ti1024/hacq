# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/512)-i*sin(Pi/512)*Z
# Distance between unitary and approximation:0.00235108
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:60
# T and T^{Dagger} gates:24
# Hadamard gates:24
# Phase and Phase^{Dagger} gates:2
# Pauli gates:10
# Total cost: 24330
# Number of iterations:0
# Approximation time(seconds):0.00266104
# Decomposition time(seconds):0.000254375
# Reduction:0
# sde(abs(z)^2):25
.v 1
.i 1
.o 1
BEGIN RZ1d512(1)
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T* 1
Z 1
H 1
T 1
H 1
T 1
H 1
T 1
P 1
H 1
T 1
H 1
P* 1
END RZ1d512
BEGIN
RZ1d512 1
END
