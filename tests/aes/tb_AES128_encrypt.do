# ModelSim do script to test AES128_encrypt.sv using tb_AES128_encrypt.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog AES128_encrypt.sv
vlog tb_AES128_encrypt.sv
# point the simulator at the compiled design
vsim work.tb_AES128_encrypt
# run simulation for 200 million cycles
run 200000000
quit
