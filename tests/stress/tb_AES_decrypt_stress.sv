`timescale 1ns/1ns

module tb_AES_decrypt_stress(output reg clk, output reg rst, output reg [127:0] out);
	reg next;
	wire finished;
	wire [127:0] input_buffer;
	wire [127:0] key_buffer;
	psuedorandom128 input_source (.clk(clk),.rst(rst),.out(input_buffer));
	psuedorandom128 key_source (.clk(clk),.rst(rst),.out(key_buffer));
	AES128_decrypt decrypt (.clk(clk),.rst(rst),.start(next),.in(input_buffer),
		.key(key_buffer),.finish(finished),.AES128_decrypt(out));

        initial begin
                rst = 1;
                clk = 0;
                #20 rst = 0;
        end

        always #5 clk = !clk;

	always_ff @(posedge clk or posedge rst) begin 
		if(rst) begin
			next <= 1;
		end else begin
			next <= finished;
		end
	end

endmodule // tb_AES_decrypt_stress
