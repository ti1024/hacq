# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/2048)-i*sin(Pi/2048)*Z
# Distance between unitary and approximation:0.00108469
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:0
# T and T^{Dagger} gates:0
# Hadamard gates:0
# Phase and Phase^{Dagger} gates:0
# Pauli gates:0
# Total cost: 0
# Number of iterations:0
# Approximation time(seconds):1.1363e-005
# Decomposition time(seconds):5.5713e-005
# Reduction:0
# sde(abs(z)^2):0
.v 1
.i 1
.o 1
BEGIN RZ1d2048(1)
Id 1
END RZ1d2048
BEGIN
RZ1d2048 1
END
