# ModelSim do script to test addkey.sv using tb_addkey.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog addkey.sv
vlog tb_addkey.sv
# point the simulator at the compiled design
vsim work.tb_addkey
# run simulation for 200 million cycles
run 200000000
quit
