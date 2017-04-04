# ModelSim do script to test shiftrowsstep.sv using tb_shiftrowsstep.sv
#
# set up the "work" library
vlib work
# compile our SystemVerilog files
vlog shiftrowsstep.sv
vlog tb_shiftrowsstep.sv
# point the simulator at the compiled design
vsim work.tb_shiftrowsstep
# run simulation for 200 million cycles
run 200000000
quit
