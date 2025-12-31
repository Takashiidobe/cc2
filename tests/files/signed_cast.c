int main() {
  signed char c = (signed char)255;
  signed short s = (signed short)65535;
  signed int i = (signed int)0xffffffff;
  signed long l = (signed long)1;
  signed d = -1;

  if (c != -1) return 1;
  if (s != -1) return 2;
  if (i != -1) return 3;
  if (l != 1) return 4;
  if (d != -1) return 5;

  return 0;
}
