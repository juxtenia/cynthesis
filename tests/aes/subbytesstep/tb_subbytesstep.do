# ModelSim do script to test subbytesstep.sv using tb_subbytesstep.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog subbytesstep.sv
vlog tb_subbytesstep.sv
# point the simulator at the compiled design
vsim work.tb_subbytesstep
# run simulation for 200 million cycles
run 200000000
quit