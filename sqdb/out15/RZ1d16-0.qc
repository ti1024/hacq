# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/16)-i*sin(Pi/16)*Z
# Distance between unitary and approximation:0.0258999
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:34
# T and T^{Dagger} gates:14
# Hadamard gates:14
# Phase and Phase^{Dagger} gates:1
# Pauli gates:5
# Total cost: 14185
# Number of iterations:0
# Approximation time(seconds):5.7179e-005
# Decomposition time(seconds):0.000186199
# Reduction:0
# sde(abs(z)^2):15
.v 1
.i 1
.o 1
BEGIN RZ1d16(1)
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
Z 1
H 1
P* 1
END RZ1d16
BEGIN
RZ1d16 1
END
