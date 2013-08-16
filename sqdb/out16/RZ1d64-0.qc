# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/64)-i*sin(Pi/64)*Z
# Distance between unitary and approximation:0.0300459
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:41
# T and T^{Dagger} gates:16
# Hadamard gates:15
# Phase and Phase^{Dagger} gates:2
# Pauli gates:8
# Total cost: 16238
# Number of iterations:0
# Approximation time(seconds):6.1577e-005
# Decomposition time(seconds):0.00020196
# Reduction:0
# sde(abs(z)^2):16
.v 1
.i 1
.o 1
BEGIN RZ1d64(1)
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
T* 1
Z 1
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
Z 1
END RZ1d64
BEGIN
RZ1d64 1
END
