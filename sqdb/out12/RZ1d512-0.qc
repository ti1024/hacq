# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/512)-i*sin(Pi/512)*Z
# Distance between unitary and approximation:0.00433875
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:0
# T and T^{Dagger} gates:0
# Hadamard gates:0
# Phase and Phase^{Dagger} gates:0
# Pauli gates:0
# Total cost: 0
# Number of iterations:0
# Approximation time(seconds):4.2884e-005
# Decomposition time(seconds):0.00241802
# Reduction:0
# sde(abs(z)^2):0
.v 1
.i 1
.o 1
BEGIN RZ1d512(1)
Id 1
END RZ1d512
BEGIN
RZ1d512 1
END
