# ModelSim do script to test newkey.sv using tb_newkey.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog newkey.sv
vlog tb_newkey.sv
# point the simulator at the compiled design
vsim work.tb_newkey
# run simulation for 200 million cycles
run 200000000
quit
