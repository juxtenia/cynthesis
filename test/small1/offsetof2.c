#include "testharness.h"
#include <stddef.h>

struct foo {
  struct bar {
    int a[8];
    int b;
  } f1;
  struct baz {
    int c[4];
  } f2[2];
};


int main() {
  if(offsetof(struct foo, f1.b) != 8 * sizeof(int)) E(1);

  if(offsetof(struct foo, f1.a[2]) != 2 * sizeof(int)) E(2);

  if(offsetof(struct foo, f2[1].c[3])
     != sizeof(struct bar) + sizeof(struct baz) + 3 * sizeof(int)) E(3);

  // Now do the same with WILD pointers
  SUCCESS;
}
