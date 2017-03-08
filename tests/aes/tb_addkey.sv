/*
 * Executes a couple of tests on the bitcounter
 * Based on the 15/16 ECAD labs test for the rotary encoder
 */

`timescale 1ns/1ns

module tb_addkey
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

    addkey dut (
		.clk(clk),
		.rst(rst),
		.start(started),
		.in(in),
                .key(key),
		.finish(finished),
		.addkey(count)
	);

	int numerr;
	int testno;
	bit nexttest;
	bit endtest;

	// initialise clock and generate a reset pulse
	initial begin
                key = 128'h2b7e151628aed2a6abf7158809cf4f3c;
		testinputs[0] = 128'h6bc1bee22e409f96e93d7e117393172a;            
		testoutputs[0] = 128'h40bfabf406ee4d3042ca6b997a5c5816;
		testinputs[1] = 128'hae2d8a571e03ac9c9eb76fac45af8e51;
		testoutputs[1] = 128'h85539f4136ad7e3a35407a244c60c16d;
		testinputs[2] = 128'h30c81c46a35ce411e5fbc1191a0a52ef;
		testoutputs[2] = 128'h1bb609508bf236b74e0cd49113c51dd3;
		testinputs[3] = 128'hf69f2445df4f9b17ad2b417be66c3710;
		testoutputs[3] = 128'hdde13153f7e149b106dc54f3efa3782c;

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
