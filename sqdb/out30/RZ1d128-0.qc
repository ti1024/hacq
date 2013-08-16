# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/128)-i*sin(Pi/128)*Z
# Distance between unitary and approximation:0.000959916
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:72
# T and T^{Dagger} gates:28
# Hadamard gates:29
# Phase and Phase^{Dagger} gates:1
# Pauli gates:14
# Total cost: 28344
# Number of iterations:0
# Approximation time(seconds):0.125011
# Decomposition time(seconds):0.000286996
# Reduction:0
# sde(abs(z)^2):30
.v 1
.i 1
.o 1
BEGIN RZ1d128(1)
P 1
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
END RZ1d128
BEGIN
RZ1d128 1
END
