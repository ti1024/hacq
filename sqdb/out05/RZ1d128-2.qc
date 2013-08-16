# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/128)-i*sin(Pi/128)*Z
# Distance between unitary and approximation:0.0120978
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:121
# T and T^{Dagger} gates:50
# Hadamard gates:51
# Phase and Phase^{Dagger} gates:4
# Pauli gates:16
# Total cost: 50686
# Number of iterations:2
# Approximation time(seconds):0.00561127
# Decomposition time(seconds):0.000422247
# Reduction:44
# sde(abs(z)^2):52
.v 1
.i 1
.o 1
BEGIN RZ1d128(1)
P 1
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
T 1
H 1
T 1
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
T 1
H 1
T* 1
Z 1
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
T 1
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
P 1
H 1
T 1
P 1
H 1
P* 1
END RZ1d128
BEGIN
RZ1d128 1
END
