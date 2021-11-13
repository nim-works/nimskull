#ifndef __CXX_HEADER_H_
#define __CXX_HEADER_H_

struct CxxStruct {};

struct CxxForward;

struct __cxx_internal{
  int __field;
  CxxForward* forward;
};

struct CxxComplete {
  int field1;
  int field2;

  const int selfSizeof() {
    return sizeof(*this);
  }
};

struct TestOperators {
  int value;

  int& operator[](int idx) {
    value += idx;
    return value;
  }
};

TestOperators operator+(TestOperators lhs, TestOperators rhs) {
  return {lhs.value + rhs.value};
}




template <typename T1, typename T2 = int>
struct CxxTemplate {
  T1 field1;
  T2 field2;

  CxxTemplate() {}
  CxxTemplate(T1 arg1, T2 arg2) : field1(arg1), field2(arg2) {}
  CxxTemplate(T1 arg1) : field1(arg1), field2(12) {}
};

struct CxxWithDefault {
  int field = 12;
};


struct CxxNoDefault {
  int field;
  CxxNoDefault(int arg) {}

  const int getField() { return field; }
  void setField(int val) { field = val; }

};

#endif // __CXX_HEADER_H_
