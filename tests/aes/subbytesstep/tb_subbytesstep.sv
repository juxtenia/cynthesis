/*
 * Executes a couple of tests on the bitcounter
 * Based on the 15/16 ECAD labs test for the rotary encoder
 */

`timescale 1ns/1ns

module tb_subbytesstep
    (
	    output logic 		clk,
	    output logic 		rst,
	    output logic [127:0] 	in,
            output logic [127:0] 	key,
	    output logic [127:0] 	count
    );

    logic started;
    logic finished;
    logic finishfollow;

    logic [127:0] testinputs [3:0];
    logic [127:0] testoutputs [3:0];

    subbytesstep dut (
		.clk(clk),
		.rst(rst),
		.start(started),
		.in(in),
                .key(key),
		.finish(finished),
		.subbytesstep(count)
	);

	int numerr;
	int testno;
	bit nexttest;
	bit endtest;

	// initialise clock and generate a reset pulse
	initial begin
                key = 128'h3c4fcf098815f7aba6d2ae2816157e2b;
		testinputs[0] = 128'h2a179373117e3de9969f402ee2bec16b;            
		testoutputs[0] = 128'he5f0dc8f82f3271e90db093198ae787f;
		testinputs[1] = 128'h518eaf45ac6fb79e9cac031e578a2dae;
		testoutputs[1] = 128'hd119796e91a8a90bde917b725b7ed8e4;
		testinputs[2] = 128'hef520a1a19c1fbe511e45ca3461cc830;
		testoutputs[2] = 128'hdf0067a2d4780fd982694a0a5a9ce804;
		testinputs[3] = 128'h10376ce67b412bad179b4fdf45249ff6;
		testoutputs[3] = 128'hca9a508e2183f195f014849e6e36db42;

		clk = 1;
		rst = 1;
        started = 0;
		numerr = 0;
		endtest = 0;
		nexttest = 0;
		testno = 0;
		in = 2'b00;
		#20 rst = 0;

		$display("%010t ---------- Start simulation. ----------", $time);

		nexttest = 1;
	end

	// oscilate the clock
	always #5 clk = !clk;
	// output checking
	always @ (posedge clk) begin 
	    finishfollow <= finished;
	    if(finished && !finishfollow) begin
                #10
	        $display("%010t ---------- input was %h result should be %h, is %h ----------", $time, in, testoutputs[testno], count);
		    if (count != testoutputs[testno]) numerr = numerr + 1;
		    testno = testno + 1;
		    if(testno > 3) endtest = 1;
		    else nexttest = 1;
	    end
	end
	//Start a testrun
	always @ (posedge nexttest) begin
		#20
		nexttest = 0;
		in = testinputs[testno];
		started = 1;
		#20 started = 0;
	end
	//Errors
	always @ (numerr) $display(" - ERROR");
	//Termination
	always @ (endtest) begin
		if (numerr == 0) $display("SUCCESS");
		else $display("FAILED with %d errors", numerr);
		$finish();
	end
endmodule
/*
 * Executes a couple of tests on the bitcounter
 * Based on the 15/16 ECAD labs test for the rotary encoder
 */

`timescale 1ns/1ns

module tb_subbytesstep
    (
	    output logic 		clk,
	    output logic 		rst,
	    output logic [127:0] 	in,
            output logic [127:0] 	key,
	    output logic [127:0] 	count
    );

    logic started;
    logic startfollow;
    logic finished;
    logic finishfollow;

    logic [127:0] testinputs [3:0];
    logic [127:0] testoutputs [3:0];

    subbytesstep dut (
		.clk(clk),
		.rst(rst),
		.start(started),
		.in(in),
                .key(key),
		.finish(finished),
		.subbytesstep(count)
	);

	int numerr;
	int testno;
	bit nexttest;
	bit endtest;

	// initialise clock and generate a reset pulse
	initial begin
                key = 128'h3c4fcf098815f7aba6d2ae2816157e2b;
		testinputs[0] = 128'h2a179373117e3de9969f402ee2bec16b;            
		testoutputs[0] = 128'he5f0dc8f82f3271e90db093198ae787f;
		testinputs[1] = 128'h518eaf45ac6fb79e9cac031e578a2dae;
		testoutputs[1] = 128'hd119796e91a8a90bde917b725b7ed8e4;
		testinputs[2] = 128'hef520a1a19c1fbe511e45ca3461cc830;
		testoutputs[2] = 128'hdf0067a2d4780fd982694a0a5a9ce804;
		testinputs[3] = 128'h10376ce67b412bad179b4fdf45249ff6;
		testoutputs[3] = 128'hca9a508e2183f195f014849e6e36db42;

		clk = 1;
		rst = 1;
        started = 0;
		numerr = 0;
		endtest = 0;
		nexttest = 0;
		testno = 0;
		in = 2'b00;
		#20 rst = 0;

		$display("%010t ---------- Start simulation. ----------", $time);

		nexttest = 1;
	end

	// oscilate the clock
	always #5 clk = !clk;
	// output checking
	always @ (posedge clk) begin 
            finishfollow <= finished && started && startfollow;
            startfollow <= started;
            if(finished && started && startfollow && !finishfollow) begin
                #10
	        $display("%010t ---------- input was %h result should be %h, is %h ----------", $time, in, testoutputs[testno], count);
		    if (count != testoutputs[testno]) numerr = numerr + 1;
		    testno = testno + 1;
		    if(testno > 3) endtest = 1;
		    else nexttest = 1;
	    end
	end
	//Start a testrun
	always @ (posedge nexttest) begin
                #20
                started = 0;
                nexttest = 0;
                #20
                in = testinputs[testno];
                started = 1;
        end
	//Errors
	always @ (numerr) $display(" - ERROR");
	//Termination
	always @ (endtest) begin
		if (numerr == 0) $display("SUCCESS");
		else $display("FAILED with %d errors", numerr);
		$finish();
	end
endmodule
