# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/16)-i*sin(Pi/16)*Z
# Distance between unitary and approximation:0.0213647
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:74
# T and T^{Dagger} gates:28
# Hadamard gates:28
# Phase and Phase^{Dagger} gates:4
# Pauli gates:14
# Total cost: 28454
# Number of iterations:1
# Approximation time(seconds):0.00199614
# Decomposition time(seconds):0.000277833
# Reduction:24
# sde(abs(z)^2):29
.v 1
.i 1
.o 1
BEGIN RZ1d16(1)
P 1
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
P 1
H 1
T 1
P 1
H 1
T 1
X 1
P* 1
END RZ1d16
BEGIN
RZ1d16 1
END
