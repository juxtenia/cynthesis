module AES_encrypt_stress(input clk, input rst, output reg [127:0] out);
	reg next;
	wire finished;
	wire [127:0] input_buffer;
	wire [127:0] key_buffer;
	psuedorandom128 input_source (.clk(clk),.rst(rst),.out(input_buffer));
	psuedorandom128 input_source (.clk(clk),.rst(rst),.out(key_buffer));
	AES128_encrypt encrypt (.clk(clk),.rst(rst),.start(next),.in(input_buffer),
		.key(key_buffer),.finish(finished),.AES128_encrypt(out));

	always_ff @(posedge clk or posedge rst) begin 
		if(rst) begin
			next <= 0;
		end else begin
			next <= finished;
		end
	end

endmodule // AES_encrypt_stress