# ModelSim do script to test AES128_decrypt.sv using tb_AES128_decrypt.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog AES128_decrypt.sv
vlog tb_AES128_decrypt.sv
# point the simulator at the compiled design
vsim work.tb_AES128_decrypt
# run simulation for 200 million cycles
run 200000000
quit
