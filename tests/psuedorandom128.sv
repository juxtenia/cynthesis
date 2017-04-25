module psuedorandom128(input clk, input rst, output reg [127:0] out);

    assign ready = count == 128;

    always @(posedge clk or posedge rst) begin 
    	if(rst) begin
    		out <= 15'h7f1a; 
    	end else begin
   			out[127:1] <= out[126:0];
   			out[0] <= out[14] ^ out[13];
    	end
    end

endmodule // psuedorandom128