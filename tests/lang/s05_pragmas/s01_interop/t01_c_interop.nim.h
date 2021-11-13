#ifndef __C_HEADER_H_
#define __C_HEADER_H_

#include <stdarg.h>

struct NoTypedef {
  int field1;
  int field2;
  int __field3;
};

typedef struct WithTypedef {
  int field1;
  const int field2;
  int __field3;
} WithTypedef;

void c_void() {}

int c_return_int() { return 12; }

int c_increment_by_two(int arg) { (void)(arg); return arg + 2; }

int c_sum_variadic(int count,  ...) {
    int result = 0;

    va_list args;
    va_start(args, count);

    for (int i = 0; i < count; ++i) {
        result += va_arg(args, int);
    }

    va_end(args);

    return result;
}

struct NoTypedef c_return_no_typedef(int f1, int f2, int f3) {
  struct NoTypedef result;
  result.field1 = f1;
  result.field2 = f2;
  result.__field3 = f3;

  return result;
}

WithTypedef c_return_with_typedef(int f1, int f2, int f3) {
  WithTypedef result = {.field1 = f1, .field2 = f2, .__field3 = f3};
  return result;
}



#endif // __C_HEADER_H_
