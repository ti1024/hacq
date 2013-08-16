# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/64)-i*sin(Pi/64)*Z
# Distance between unitary and approximation:0.0750157
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:42
# T and T^{Dagger} gates:18
# Hadamard gates:17
# Phase and Phase^{Dagger} gates:2
# Pauli gates:5
# Total cost: 18255
# Number of iterations:1
# Approximation time(seconds):0.00136864
# Decomposition time(seconds):0.00021149
# Reduction:14
# sde(abs(z)^2):18
.v 1
.i 1
.o 1
BEGIN RZ1d64(1)
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
H 1
T 1
H 1
T 1
H 1
T 1
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
P 1
H 1
T 1
P 1
H 1
T 1
END RZ1d64
BEGIN
RZ1d64 1
END
