# ModelSim do script to test mixcoloumns.sv using tb_mixcoloumns.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog mixcoloumns.sv
vlog tb_mixcoloumns.sv
# point the simulator at the compiled design
vsim work.tb_mixcoloumns
# run simulation for 200 million cycles
run 200000000
quit
