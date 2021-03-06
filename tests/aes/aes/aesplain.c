#include <stdint.h>
#include <string.h>
#include <stdio.h>

// The number of rounds in AES Cipher.
#ifndef Nr
#define Nr 10
#endif

struct state {
	uint8_t s_0_0;
	uint8_t s_0_1;
	uint8_t s_0_2;
	uint8_t s_0_3;
	uint8_t s_1_0;
	uint8_t s_1_1;
	uint8_t s_1_2;
	uint8_t s_1_3;
	uint8_t s_2_0;
	uint8_t s_2_1;
	uint8_t s_2_2;
	uint8_t s_2_3;
	uint8_t s_3_0;
	uint8_t s_3_1;
	uint8_t s_3_2;
	uint8_t s_3_3;
};

struct row {
	uint8_t r_0;
	uint8_t r_1;
	uint8_t r_2;
	uint8_t r_3;
};

const uint8_t sbox[256] =   {
  //0     1    2      3     4    5     6     7      8    9     A      B    C     D     E     F
  0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab, 0x76,
  0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0,
  0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
  0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2, 0xeb, 0x27, 0xb2, 0x75,
  0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84,
  0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
  0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8,
  0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5, 0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2,
  0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
  0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb,
  0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79,
  0xe7, 0xc8, 0x37, 0x6d, 0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
  0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a,
  0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e,
  0xe1, 0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
  0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb, 0x16 };

const uint8_t rsbox[256] =
{ 0x52, 0x09, 0x6a, 0xd5, 0x30, 0x36, 0xa5, 0x38, 0xbf, 0x40, 0xa3, 0x9e, 0x81, 0xf3, 0xd7, 0xfb,
  0x7c, 0xe3, 0x39, 0x82, 0x9b, 0x2f, 0xff, 0x87, 0x34, 0x8e, 0x43, 0x44, 0xc4, 0xde, 0xe9, 0xcb,
  0x54, 0x7b, 0x94, 0x32, 0xa6, 0xc2, 0x23, 0x3d, 0xee, 0x4c, 0x95, 0x0b, 0x42, 0xfa, 0xc3, 0x4e,
  0x08, 0x2e, 0xa1, 0x66, 0x28, 0xd9, 0x24, 0xb2, 0x76, 0x5b, 0xa2, 0x49, 0x6d, 0x8b, 0xd1, 0x25,
  0x72, 0xf8, 0xf6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xd4, 0xa4, 0x5c, 0xcc, 0x5d, 0x65, 0xb6, 0x92,
  0x6c, 0x70, 0x48, 0x50, 0xfd, 0xed, 0xb9, 0xda, 0x5e, 0x15, 0x46, 0x57, 0xa7, 0x8d, 0x9d, 0x84,
  0x90, 0xd8, 0xab, 0x00, 0x8c, 0xbc, 0xd3, 0x0a, 0xf7, 0xe4, 0x58, 0x05, 0xb8, 0xb3, 0x45, 0x06,
  0xd0, 0x2c, 0x1e, 0x8f, 0xca, 0x3f, 0x0f, 0x02, 0xc1, 0xaf, 0xbd, 0x03, 0x01, 0x13, 0x8a, 0x6b,
  0x3a, 0x91, 0x11, 0x41, 0x4f, 0x67, 0xdc, 0xea, 0x97, 0xf2, 0xcf, 0xce, 0xf0, 0xb4, 0xe6, 0x73,
  0x96, 0xac, 0x74, 0x22, 0xe7, 0xad, 0x35, 0x85, 0xe2, 0xf9, 0x37, 0xe8, 0x1c, 0x75, 0xdf, 0x6e,
  0x47, 0xf1, 0x1a, 0x71, 0x1d, 0x29, 0xc5, 0x89, 0x6f, 0xb7, 0x62, 0x0e, 0xaa, 0x18, 0xbe, 0x1b,
  0xfc, 0x56, 0x3e, 0x4b, 0xc6, 0xd2, 0x79, 0x20, 0x9a, 0xdb, 0xc0, 0xfe, 0x78, 0xcd, 0x5a, 0xf4,
  0x1f, 0xdd, 0xa8, 0x33, 0x88, 0x07, 0xc7, 0x31, 0xb1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xec, 0x5f,
  0x60, 0x51, 0x7f, 0xa9, 0x19, 0xb5, 0x4a, 0x0d, 0x2d, 0xe5, 0x7a, 0x9f, 0x93, 0xc9, 0x9c, 0xef,
  0xa0, 0xe0, 0x3b, 0x4d, 0xae, 0x2a, 0xf5, 0xb0, 0xc8, 0xeb, 0xbb, 0x3c, 0x83, 0x53, 0x99, 0x61,
  0x17, 0x2b, 0x04, 0x7e, 0xba, 0x77, 0xd6, 0x26, 0xe1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0c, 0x7d };


// The round constant word array, Rcon[i], contains the values given by 
// x to th e power (i-1) being powers of x (x is denoted as {02}) in the field GF(2^8)
// Note that i starts at 1, not 0).
const uint8_t Rcon[255] = {
  0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 
  0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 
  0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 
  0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 
  0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 
  0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 
  0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 
  0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 
  0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 
  0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 
  0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 
  0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 
  0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 
  0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 
  0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 
  0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb  };


#define getSBoxValue(num) (sbox[num])

#define getSBoxInvert(num) (rsbox[num])

#define rotWord(k0,k1,k2,k3) {.r_0=k1, .r_1=k2, .r_2=k3, .r_3=k0}

#define subword(k) {.r_0 = getSBoxValue(k.r_0), .r_1 = getSBoxValue(k.r_1), .r_2 = getSBoxValue(k.r_2), .r_3 = getSBoxValue(k.r_3)}

#define subbytes(s) { \
  .s_0_0 = getSBoxValue(s.s_0_0), \
  .s_0_1 = getSBoxValue(s.s_0_1), \
  .s_0_2 = getSBoxValue(s.s_0_2), \
  .s_0_3 = getSBoxValue(s.s_0_3), \
  .s_1_0 = getSBoxValue(s.s_1_0), \
  .s_1_1 = getSBoxValue(s.s_1_1), \
  .s_1_2 = getSBoxValue(s.s_1_2), \
  .s_1_3 = getSBoxValue(s.s_1_3), \
  .s_2_0 = getSBoxValue(s.s_2_0), \
  .s_2_1 = getSBoxValue(s.s_2_1), \
  .s_2_2 = getSBoxValue(s.s_2_2), \
  .s_2_3 = getSBoxValue(s.s_2_3), \
  .s_3_0 = getSBoxValue(s.s_3_0), \
  .s_3_1 = getSBoxValue(s.s_3_1), \
  .s_3_2 = getSBoxValue(s.s_3_2), \
  .s_3_3 = getSBoxValue(s.s_3_3) }

#define invsubbytes(s) { \
  .s_0_0 = getSBoxInvert(s.s_0_0), \
  .s_0_1 = getSBoxInvert(s.s_0_1), \
  .s_0_2 = getSBoxInvert(s.s_0_2), \
  .s_0_3 = getSBoxInvert(s.s_0_3), \
  .s_1_0 = getSBoxInvert(s.s_1_0), \
  .s_1_1 = getSBoxInvert(s.s_1_1), \
  .s_1_2 = getSBoxInvert(s.s_1_2), \
  .s_1_3 = getSBoxInvert(s.s_1_3), \
  .s_2_0 = getSBoxInvert(s.s_2_0), \
  .s_2_1 = getSBoxInvert(s.s_2_1), \
  .s_2_2 = getSBoxInvert(s.s_2_2), \
  .s_2_3 = getSBoxInvert(s.s_2_3), \
  .s_3_0 = getSBoxInvert(s.s_3_0), \
  .s_3_1 = getSBoxInvert(s.s_3_1), \
  .s_3_2 = getSBoxInvert(s.s_3_2), \
  .s_3_3 = getSBoxInvert(s.s_3_3) }

#define shiftrows(s) { \
	.s_0_0 = s.s_0_0, .s_0_1 = s.s_1_1, .s_0_2 = s.s_2_2, .s_0_3 = s.s_3_3, \
	.s_1_0 = s.s_1_0, .s_1_1 = s.s_2_1, .s_1_2 = s.s_3_2, .s_1_3 = s.s_0_3, \
	.s_2_0 = s.s_2_0, .s_2_1 = s.s_3_1, .s_2_2 = s.s_0_2, .s_2_3 = s.s_1_3, \
	.s_3_0 = s.s_3_0, .s_3_1 = s.s_0_1, .s_3_2 = s.s_1_2, .s_3_3 = s.s_2_3 }

#define invshiftrows(s) { \
  .s_0_0 = s.s_0_0, .s_0_1 = s.s_3_1, .s_0_2 = s.s_2_2, .s_0_3 = s.s_1_3, \
  .s_1_0 = s.s_1_0, .s_1_1 = s.s_0_1, .s_1_2 = s.s_3_2, .s_1_3 = s.s_2_3, \
  .s_2_0 = s.s_2_0, .s_2_1 = s.s_1_1, .s_2_2 = s.s_0_2, .s_2_3 = s.s_3_3, \
  .s_3_0 = s.s_3_0, .s_3_1 = s.s_2_1, .s_3_2 = s.s_1_2, .s_3_3 = s.s_0_3 }

#define pairwisexor(s) { \
	.s_0_0 = s.s_0_0 ^ s.s_0_1, .s_0_1 = s.s_0_1 ^ s.s_0_2, .s_0_2 = s.s_0_2 ^ s.s_0_3, .s_0_3 = s.s_0_3 ^ s.s_0_0, \
	.s_1_0 = s.s_1_0 ^ s.s_1_1, .s_1_1 = s.s_1_1 ^ s.s_1_2, .s_1_2 = s.s_1_2 ^ s.s_1_3, .s_1_3 = s.s_1_3 ^ s.s_1_0, \
	.s_2_0 = s.s_2_0 ^ s.s_2_1, .s_2_1 = s.s_2_1 ^ s.s_2_2, .s_2_2 = s.s_2_2 ^ s.s_2_3, .s_2_3 = s.s_2_3 ^ s.s_2_0, \
	.s_3_0 = s.s_3_0 ^ s.s_3_1, .s_3_1 = s.s_3_1 ^ s.s_3_2, .s_3_2 = s.s_3_2 ^ s.s_3_3, .s_3_3 = s.s_3_3 ^ s.s_3_0 }

#define xtime(x) ((x<<1) ^ (((x>>7) & 1) * 0x1b))

#define multiply(x, y)                                \
      (  ((y & 1) * x) ^                              \
      ((y>>1 & 1) * xtime(x)) ^                       \
      ((y>>2 & 1) * xtime(xtime(x))) ^                \
      ((y>>3 & 1) * xtime(xtime(xtime(x)))) ^         \
      ((y>>4 & 1) * xtime(xtime(xtime(xtime(x))))))   \

#define rowxor(s) { \
	.r_0 = s.s_0_0 ^ s.s_0_1 ^ s.s_0_2 ^ s.s_0_3, \
	.r_1 = s.s_1_0 ^ s.s_1_1 ^ s.s_1_2 ^ s.s_1_3, \
	.r_2 = s.s_2_0 ^ s.s_2_1 ^ s.s_2_2 ^ s.s_2_3, \
	.r_3 = s.s_3_0 ^ s.s_3_1 ^ s.s_3_2 ^ s.s_3_3 }

#define staterowxor(s,k) { \
	.s_0_0 = s.s_0_0 ^ k.r_0, .s_0_1 = s.s_0_1 ^ k.r_0, .s_0_2 = s.s_0_2 ^ k.r_0, .s_0_3 = s.s_0_3 ^ k.r_0, \
	.s_1_0 = s.s_1_0 ^ k.r_1, .s_1_1 = s.s_1_1 ^ k.r_1, .s_1_2 = s.s_1_2 ^ k.r_1, .s_1_3 = s.s_1_3 ^ k.r_1, \
	.s_2_0 = s.s_2_0 ^ k.r_2, .s_2_1 = s.s_2_1 ^ k.r_2, .s_2_2 = s.s_2_2 ^ k.r_2, .s_2_3 = s.s_2_3 ^ k.r_2, \
	.s_3_0 = s.s_3_0 ^ k.r_3, .s_3_1 = s.s_3_1 ^ k.r_3, .s_3_2 = s.s_3_2 ^ k.r_3, .s_3_3 = s.s_3_3 ^ k.r_3 }

#define statestatexor(s,t) { \
	.s_0_0 = s.s_0_0 ^ t.s_0_0, .s_0_1 = s.s_0_1 ^ t.s_0_1, .s_0_2 = s.s_0_2 ^ t.s_0_2, .s_0_3 = s.s_0_3 ^ t.s_0_3, \
	.s_1_0 = s.s_1_0 ^ t.s_1_0, .s_1_1 = s.s_1_1 ^ t.s_1_1, .s_1_2 = s.s_1_2 ^ t.s_1_2, .s_1_3 = s.s_1_3 ^ t.s_1_3, \
	.s_2_0 = s.s_2_0 ^ t.s_2_0, .s_2_1 = s.s_2_1 ^ t.s_2_1, .s_2_2 = s.s_2_2 ^ t.s_2_2, .s_2_3 = s.s_2_3 ^ t.s_2_3, \
	.s_3_0 = s.s_3_0 ^ t.s_3_0, .s_3_1 = s.s_3_1 ^ t.s_3_1, .s_3_2 = s.s_3_2 ^ t.s_3_2, .s_3_3 = s.s_3_3 ^ t.s_3_3 }

#define mapxtime(s) { \
  .s_0_0 = xtime(s.s_0_0), \
  .s_0_1 = xtime(s.s_0_1), \
  .s_0_2 = xtime(s.s_0_2), \
  .s_0_3 = xtime(s.s_0_3), \
  .s_1_0 = xtime(s.s_1_0), \
  .s_1_1 = xtime(s.s_1_1), \
  .s_1_2 = xtime(s.s_1_2), \
  .s_1_3 = xtime(s.s_1_3), \
  .s_2_0 = xtime(s.s_2_0), \
  .s_2_1 = xtime(s.s_2_1), \
  .s_2_2 = xtime(s.s_2_2), \
  .s_2_3 = xtime(s.s_2_3), \
  .s_3_0 = xtime(s.s_3_0), \
  .s_3_1 = xtime(s.s_3_1), \
  .s_3_2 = xtime(s.s_3_2), \
  .s_3_3 = xtime(s.s_3_3) }

#define invmixcolumns(s) { \
  .s_0_0 = multiply(s.s_0_0, 0x0e) ^ multiply(s.s_0_1, 0x0b) ^ multiply(s.s_0_2, 0x0d) ^ multiply(s.s_0_3, 0x09), \
  .s_0_1 = multiply(s.s_0_0, 0x09) ^ multiply(s.s_0_1, 0x0e) ^ multiply(s.s_0_2, 0x0b) ^ multiply(s.s_0_3, 0x0d), \
  .s_0_2 = multiply(s.s_0_0, 0x0d) ^ multiply(s.s_0_1, 0x09) ^ multiply(s.s_0_2, 0x0e) ^ multiply(s.s_0_3, 0x0b), \
  .s_0_3 = multiply(s.s_0_0, 0x0b) ^ multiply(s.s_0_1, 0x0d) ^ multiply(s.s_0_2, 0x09) ^ multiply(s.s_0_3, 0x0e), \
  .s_1_0 = multiply(s.s_1_0, 0x0e) ^ multiply(s.s_1_1, 0x0b) ^ multiply(s.s_1_2, 0x0d) ^ multiply(s.s_1_3, 0x09), \
  .s_1_1 = multiply(s.s_1_0, 0x09) ^ multiply(s.s_1_1, 0x0e) ^ multiply(s.s_1_2, 0x0b) ^ multiply(s.s_1_3, 0x0d), \
  .s_1_2 = multiply(s.s_1_0, 0x0d) ^ multiply(s.s_1_1, 0x09) ^ multiply(s.s_1_2, 0x0e) ^ multiply(s.s_1_3, 0x0b), \
  .s_1_3 = multiply(s.s_1_0, 0x0b) ^ multiply(s.s_1_1, 0x0d) ^ multiply(s.s_1_2, 0x09) ^ multiply(s.s_1_3, 0x0e), \
  .s_2_0 = multiply(s.s_2_0, 0x0e) ^ multiply(s.s_2_1, 0x0b) ^ multiply(s.s_2_2, 0x0d) ^ multiply(s.s_2_3, 0x09), \
  .s_2_1 = multiply(s.s_2_0, 0x09) ^ multiply(s.s_2_1, 0x0e) ^ multiply(s.s_2_2, 0x0b) ^ multiply(s.s_2_3, 0x0d), \
  .s_2_2 = multiply(s.s_2_0, 0x0d) ^ multiply(s.s_2_1, 0x09) ^ multiply(s.s_2_2, 0x0e) ^ multiply(s.s_2_3, 0x0b), \
  .s_2_3 = multiply(s.s_2_0, 0x0b) ^ multiply(s.s_2_1, 0x0d) ^ multiply(s.s_2_2, 0x09) ^ multiply(s.s_2_3, 0x0e), \
  .s_3_0 = multiply(s.s_3_0, 0x0e) ^ multiply(s.s_3_1, 0x0b) ^ multiply(s.s_3_2, 0x0d) ^ multiply(s.s_3_3, 0x09), \
  .s_3_1 = multiply(s.s_3_0, 0x09) ^ multiply(s.s_3_1, 0x0e) ^ multiply(s.s_3_2, 0x0b) ^ multiply(s.s_3_3, 0x0d), \
  .s_3_2 = multiply(s.s_3_0, 0x0d) ^ multiply(s.s_3_1, 0x09) ^ multiply(s.s_3_2, 0x0e) ^ multiply(s.s_3_3, 0x0b), \
  .s_3_3 = multiply(s.s_3_0, 0x0b) ^ multiply(s.s_3_1, 0x0d) ^ multiply(s.s_3_2, 0x09) ^ multiply(s.s_3_3, 0x0e), }

void printstate(char *str, struct state s){
	printf("%s%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x\n", str,
	    s.s_0_0, s.s_0_1, s.s_0_2, s.s_0_3, s.s_1_0, s.s_1_1, s.s_1_2, s.s_1_3, 
	    s.s_2_0, s.s_2_1, s.s_2_2, s.s_2_3, s.s_3_0, s.s_3_1, s.s_3_2, s.s_3_3 );
}

struct state AES128_encrypt(struct state in, struct state key);
struct state AES128_decrypt(struct state in, struct state key);

inline struct state AES128_encrypt(struct state in, struct state key)
{
  uint8_t round = 0;

  // Add the First round key to the state before starting the rounds.
  struct state t1 = statestatexor(in, key);
  in = t1;
  
  // There will be Nr rounds.
  // The first Nr-1 rounds are identical.
  // These Nr-1 rounds are executed in the loop below.
  for(round = 1; round < Nr; round++)
  {
  	//Subbytes
    struct state t2 = subbytes(in);
    //Shift rows
    struct state t3 = shiftrows(t2);
    //Mix columns
    struct state t4 = pairwisexor(t3);
    struct state t5 = mapxtime(t4);
    struct row xors = rowxor(t3);
    struct state xormask = staterowxor(t5,xors);
    struct state t6 = statestatexor(t3,xormask);
    //Generate new key
    struct row s1 = rotWord(key.s_3_0, key.s_3_1, key.s_3_2, key.s_3_3);
    struct row scramble = subword(s1);
    scramble.r_0 ^= Rcon[round];
    key.s_0_0 ^= scramble.r_0; key.s_0_1 ^= scramble.r_1; key.s_0_2 ^= scramble.r_2; key.s_0_3 ^= scramble.r_3;
    key.s_1_0 ^= key.s_0_0; key.s_1_1 ^= key.s_0_1; key.s_1_2 ^= key.s_0_2; key.s_1_3 ^= key.s_0_3;
    key.s_2_0 ^= key.s_1_0; key.s_2_1 ^= key.s_1_1; key.s_2_2 ^= key.s_1_2; key.s_2_3 ^= key.s_1_3;
    key.s_3_0 ^= key.s_2_0; key.s_3_1 ^= key.s_2_1; key.s_3_2 ^= key.s_2_2; key.s_3_3 ^= key.s_2_3;
    //Add round key
    struct state t7 = statestatexor(t6, key);
    in = t7;
  }
  
  // The last round is given below.
  // The MixColumns function is not here in the last round.
  struct state t8 = subbytes(in);
  struct state t9 = shiftrows(t8);
  struct row s1 = rotWord(key.s_3_0, key.s_3_1, key.s_3_2, key.s_3_3);
  struct row scramble = subword(s1);
  scramble.r_0 ^= Rcon[round];
  key.s_0_0 ^= scramble.r_0; key.s_0_1 ^= scramble.r_1; key.s_0_2 ^= scramble.r_2; key.s_0_3 ^= scramble.r_3;
  key.s_1_0 ^= key.s_0_0; key.s_1_1 ^= key.s_0_1; key.s_1_2 ^= key.s_0_2; key.s_1_3 ^= key.s_0_3;
  key.s_2_0 ^= key.s_1_0; key.s_2_1 ^= key.s_1_1; key.s_2_2 ^= key.s_1_2; key.s_2_3 ^= key.s_1_3;
  key.s_3_0 ^= key.s_2_0; key.s_3_1 ^= key.s_2_1; key.s_3_2 ^= key.s_2_2; key.s_3_3 ^= key.s_2_3;
  struct state t10 = statestatexor(t9, key);
  return t10;
}

inline struct state AES128_decrypt(struct state in, struct state key)
{
  uint8_t round = 0;

  for(round = 1; round <= Nr; round++)
  {
    //Generate new key
    struct row s1 = rotWord(key.s_3_0, key.s_3_1, key.s_3_2, key.s_3_3);
    struct row scramble = subword(s1);
    scramble.r_0 ^= Rcon[round];
    key.s_0_0 ^= scramble.r_0; key.s_0_1 ^= scramble.r_1; key.s_0_2 ^= scramble.r_2; key.s_0_3 ^= scramble.r_3;
    key.s_1_0 ^= key.s_0_0; key.s_1_1 ^= key.s_0_1; key.s_1_2 ^= key.s_0_2; key.s_1_3 ^= key.s_0_3;
    key.s_2_0 ^= key.s_1_0; key.s_2_1 ^= key.s_1_1; key.s_2_2 ^= key.s_1_2; key.s_2_3 ^= key.s_1_3;
    key.s_3_0 ^= key.s_2_0; key.s_3_1 ^= key.s_2_1; key.s_3_2 ^= key.s_2_2; key.s_3_3 ^= key.s_2_3;
  }

  // Add the First round key to the state before starting the rounds.
  struct state t1 = statestatexor(in, key);
  in = t1;

  // There will be Nr rounds.
  // The first Nr-1 rounds are identical.
  // These Nr-1 rounds are executed in the loop below.
  for(round=Nr-1;round>0;round--)
  {
    //Inv Shift Rows
    struct state t2 = invshiftrows(in);
    //Inv Sub bytes
    struct state t3 = invsubbytes(t2);
    //Generate new key
    key.s_3_0 ^= key.s_2_0; key.s_3_1 ^= key.s_2_1; key.s_3_2 ^= key.s_2_2; key.s_3_3 ^= key.s_2_3;
    key.s_2_0 ^= key.s_1_0; key.s_2_1 ^= key.s_1_1; key.s_2_2 ^= key.s_1_2; key.s_2_3 ^= key.s_1_3;
    key.s_1_0 ^= key.s_0_0; key.s_1_1 ^= key.s_0_1; key.s_1_2 ^= key.s_0_2; key.s_1_3 ^= key.s_0_3;
    struct row s1 = rotWord(key.s_3_0, key.s_3_1, key.s_3_2, key.s_3_3);
    struct row scramble = subword(s1);
    scramble.r_0 ^= Rcon[round+1];
    key.s_0_0 ^= scramble.r_0; key.s_0_1 ^= scramble.r_1; key.s_0_2 ^= scramble.r_2; key.s_0_3 ^= scramble.r_3;
    //Add round key
    struct state t4 = statestatexor(t3, key);
    //Inv Mix Columns
    struct state t5 = invmixcolumns(t4);
    in = t5;
  }
  
  // The last round is given below.
  // The MixColumns function is not here in the last round.
  //Inv Shift Rows
  struct state t6 = invshiftrows(in);
  //Inv Sub bytes
  struct state t7 = invsubbytes(t6);
  //Generate new key
  key.s_3_0 ^= key.s_2_0; key.s_3_1 ^= key.s_2_1; key.s_3_2 ^= key.s_2_2; key.s_3_3 ^= key.s_2_3;
  key.s_2_0 ^= key.s_1_0; key.s_2_1 ^= key.s_1_1; key.s_2_2 ^= key.s_1_2; key.s_2_3 ^= key.s_1_3;
  key.s_1_0 ^= key.s_0_0; key.s_1_1 ^= key.s_0_1; key.s_1_2 ^= key.s_0_2; key.s_1_3 ^= key.s_0_3;
  struct row s1 = rotWord(key.s_3_0, key.s_3_1, key.s_3_2, key.s_3_3);
  struct row scramble = subword(s1);
  scramble.r_0 ^= Rcon[round+1];
  key.s_0_0 ^= scramble.r_0; key.s_0_1 ^= scramble.r_1; key.s_0_2 ^= scramble.r_2; key.s_0_3 ^= scramble.r_3;
  //Add round key
  struct state t8 = statestatexor(t7, key);
  return t8;
}

static void test_encrypt_ecb_verbose(void)
{
    // Example of more verbose verification
    uint8_t i;

    // 128bit key
    struct state key =        { (uint8_t) 0x2b, (uint8_t) 0x7e, (uint8_t) 0x15, (uint8_t) 0x16, (uint8_t) 0x28, (uint8_t) 0xae, (uint8_t) 0xd2, (uint8_t) 0xa6, (uint8_t) 0xab, (uint8_t) 0xf7, (uint8_t) 0x15, (uint8_t) 0x88, (uint8_t) 0x09, (uint8_t) 0xcf, (uint8_t) 0x4f, (uint8_t) 0x3c };
    // 512bit text
    struct state plain_text[4] = {{ (uint8_t) 0x6b, (uint8_t) 0xc1, (uint8_t) 0xbe, (uint8_t) 0xe2, (uint8_t) 0x2e, (uint8_t) 0x40, (uint8_t) 0x9f, (uint8_t) 0x96, (uint8_t) 0xe9, (uint8_t) 0x3d, (uint8_t) 0x7e, (uint8_t) 0x11, (uint8_t) 0x73, (uint8_t) 0x93, (uint8_t) 0x17, (uint8_t) 0x2a},
                                  { (uint8_t) 0xae, (uint8_t) 0x2d, (uint8_t) 0x8a, (uint8_t) 0x57, (uint8_t) 0x1e, (uint8_t) 0x03, (uint8_t) 0xac, (uint8_t) 0x9c, (uint8_t) 0x9e, (uint8_t) 0xb7, (uint8_t) 0x6f, (uint8_t) 0xac, (uint8_t) 0x45, (uint8_t) 0xaf, (uint8_t) 0x8e, (uint8_t) 0x51},
                                  { (uint8_t) 0x30, (uint8_t) 0xc8, (uint8_t) 0x1c, (uint8_t) 0x46, (uint8_t) 0xa3, (uint8_t) 0x5c, (uint8_t) 0xe4, (uint8_t) 0x11, (uint8_t) 0xe5, (uint8_t) 0xfb, (uint8_t) 0xc1, (uint8_t) 0x19, (uint8_t) 0x1a, (uint8_t) 0x0a, (uint8_t) 0x52, (uint8_t) 0xef},
                                  { (uint8_t) 0xf6, (uint8_t) 0x9f, (uint8_t) 0x24, (uint8_t) 0x45, (uint8_t) 0xdf, (uint8_t) 0x4f, (uint8_t) 0x9b, (uint8_t) 0x17, (uint8_t) 0xad, (uint8_t) 0x2b, (uint8_t) 0x41, (uint8_t) 0x7b, (uint8_t) 0xe6, (uint8_t) 0x6c, (uint8_t) 0x37, (uint8_t) 0x10}};

    // print text to encrypt, key and IV
    printf("ECB encrypt verbose:\n\n");
    printf("plain text:\n");
    for(i = (uint8_t) 0; i < (uint8_t) 4; ++i)
    {
        printstate("", plain_text[i]);
    }
    printf("\n");

    printstate("key:\n",key);
    printf("\n");

    // print the resulting cipher as 4 x 16 byte strings
    printf("ciphertext:\n");
    for(i = 0; i < 4; ++i)
    {
        printstate("", AES128_encrypt(plain_text[i], key));
    }
    printf("\n");
}

void test_encrypt_ecb(void)
{
  struct state key = {0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c};
  struct state in  = {0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93, 0x17, 0x2a};
  struct state out = {0x3a, 0xd7, 0x7b, 0xb4, 0x0d, 0x7a, 0x36, 0x60, 0xa8, 0x9e, 0xca, 0xf3, 0x24, 0x66, 0xef, 0x97};

  struct state result = AES128_encrypt(in, key);

  printf("ECB encrypt: ");

  if(0 == memcmp((char*) &out, (char*) &result, 16))
  {
    printf("SUCCESS!\n");
  }
  else
  {
    printf("FAILURE!\n");
    printstate("Expected:\n", out);
    printstate("Result:\n", result);
  }
}

static void test_decrypt_ecb(void)
{
  struct state key = {0x2b, 0x7e, 0x15, 0x16, 0x28, 0xae, 0xd2, 0xa6, 0xab, 0xf7, 0x15, 0x88, 0x09, 0xcf, 0x4f, 0x3c};
  struct state in  = {0x3a, 0xd7, 0x7b, 0xb4, 0x0d, 0x7a, 0x36, 0x60, 0xa8, 0x9e, 0xca, 0xf3, 0x24, 0x66, 0xef, 0x97};
  struct state out = {0x6b, 0xc1, 0xbe, 0xe2, 0x2e, 0x40, 0x9f, 0x96, 0xe9, 0x3d, 0x7e, 0x11, 0x73, 0x93, 0x17, 0x2a};

  struct state result = AES128_decrypt(in, key);

  printf("ECB decrypt: ");

  if(0 == memcmp((char*) &out, (char*) &result, 16))
  {
    printf("SUCCESS!\n");
  }
  else
  {
    printf("FAILURE!\n");
    printstate("Expected:\n", out);
    printstate("Result:\n", result);
  }
}

int main(void)
{
    test_encrypt_ecb();
    test_decrypt_ecb();
    test_encrypt_ecb_verbose();
    
    return 0;
}
