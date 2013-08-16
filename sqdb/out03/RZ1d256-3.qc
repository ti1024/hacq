# Computed by SQCT, based on arXiv:1206.5236
# Published at: http://code.google.com/p/sqct/
# Symbolic form of unitary to approximate:Id*cos(Pi/256)-i*sin(Pi/256)*Z
# Distance between unitary and approximation:0.00867745
# (Formula (1) in http://arxiv.org/abs/quant-ph/0411206)
# Total number of gates:0
# T and T^{Dagger} gates:0
# Hadamard gates:0
# Phase and Phase^{Dagger} gates:0
# Pauli gates:0
# Total cost: 0
# Number of iterations:3
# Approximation time(seconds):0.0189817
# Decomposition time(seconds):5.4614e-005
# Reduction:160
# sde(abs(z)^2):0
.v 1
.i 1
.o 1
BEGIN RZ1d256(1)
Id 1
END RZ1d256
BEGIN
RZ1d256 1
END
