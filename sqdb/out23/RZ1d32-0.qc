# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/32)-i*sin(Pi/32)*Z
# Distance between unitary and approximation:0.00260866
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:52
# T and T^{Dagger} gates:22
# Hadamard gates:21
# Phase and Phase^{Dagger} gates:1
# Pauli gates:8
# Total cost: 22258
# Number of iterations:0
# Approximation time(seconds):0.000750295
# Decomposition time(seconds):0.000233482
# Reduction:0
# sde(abs(z)^2):22
.v 1
.i 1
.o 1
BEGIN RZ1d32(1)
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
T 1
H 1
T 1
H 1
T 1
P* 1
H 1
T 1
Z 1
END RZ1d32
BEGIN
RZ1d32 1
END
