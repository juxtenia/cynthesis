/*
 * Executes a couple of tests on the bitcounter
 * Based on the 15/16 ECAD labs test for the rotary encoder
 */

`timescale 1ns/1ns

module tb_bitcount
    (
	    output logic 		clk,
	    output logic 		rst,
	    output logic [31:0] 	in,
	    output logic [31:0] 	count
    );

    logic started;
    logic startfollow;
    logic finished;
    logic finishfollow;

    logic [31:0] testinputs [8:0];
    logic [31:0] testoutputs [8:0];

    bitcount dut (
		.clk(clk),
		.rst(rst),
		.start(started),
		.in(in),
		.finish(finished),
		.bitcount(count)
	);

	int numerr;
	int testno;
	bit nexttest;
	bit endtest;

	// initialise clock and generate a reset pulse
	initial begin
		testinputs[0] = 0;
		testoutputs[0] = 0;
		testinputs[1] = 32'b10000000_00000000_00000000_00000000;
		testoutputs[1] = 1;
		testinputs[2] = 32'b10000000_00000000_00001000_00000000;
		testoutputs[2] = 2;
		testinputs[3] = 32'b10000100_00001000_00001000_00000100;
		testoutputs[3] = 5;
		testinputs[4] = 32'b10010000_00010000_11001000_00111100;
		testoutputs[4] = 10;
		testinputs[5] = 32'b11111000_01010010_01001010_00100010;
		testoutputs[5] = 13;
		testinputs[6] = 32'b11111111_00000000_11111111_00000000;
		testoutputs[6] = 16;
		testinputs[7] = 32'b11111111_11111111_11111111_11111111;
		testoutputs[7] = 32;	
		testinputs[8] = 32'b00000000_11111111_00000000_11111111;
                testoutputs[8] = 16;


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
	        $display("%010t ---------- input was %d result should be %d, is %d ----------", $time, in, testoutputs[testno], count);
		    if (count != testoutputs[testno]) numerr = numerr + 1;
		    testno = testno + 1;
		    if(testno > 7) endtest = 1;
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
