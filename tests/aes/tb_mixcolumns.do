# ModelSim do script to test mixcolumns.sv using tb_mixcolumns.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog mixcolumns.sv
vlog tb_mixcolumns.sv
# point the simulator at the compiled design
vsim work.tb_mixcolumns
# run simulation for 200 million cycles
run 200000000
quit
