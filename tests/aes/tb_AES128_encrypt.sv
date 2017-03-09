/*
 * Executes a couple of tests on the bitcounter
 * Based on the 15/16 ECAD labs test for the rotary encoder
 */

`timescale 1ns/1ns

module tb_AES128_encrypt
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

    AES128_encrypt dut (
		.clk(clk),
		.rst(rst),
		.start(started),
		.in(in),
                .key(key),
		.finish(finished),
		.AES128_encrypt(count)
	);

	int numerr;
	int testno;
	bit nexttest;
	bit endtest;

	// initialise clock and generate a reset pulse
	initial begin
                key = 128'h2b7e151628aed2a6abf7158809cf4f3c;
		testinputs[0] = 128'h6bc1bee22e409f96e93d7e117393172a;            
		testoutputs[0] = 128'h3ad77bb40d7a3660a89ecaf32466ef97;
		testinputs[1] = 128'hae2d8a571e03ac9c9eb76fac45af8e51;
		testoutputs[1] = 128'hf5d3d58503b9699de785895a96fdbaaf;
		testinputs[2] = 128'h30c81c46a35ce411e5fbc1191a0a52ef;
		testoutputs[2] = 128'h43b1cd7f598ece23881b00e3ed030688;
		testinputs[3] = 128'hf69f2445df4f9b17ad2b417be66c3710;
		testoutputs[3] = 128'h7b0c785e27e8ad3f8223207104725dd4;

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
