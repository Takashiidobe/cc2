int main() {
  enum { A = 1 + 2, B = A + 4, C = (A ? B : 0), D = ~0, E = (1 << 5) };
  if (A != 3) return 1;
  if (B != 7) return 2;
  if (C != 7) return 3;
  if (D != -1) return 4;
  if (E != 32) return 5;

  enum { X = 3, Y = X * 2 + 1, Z = (Y >= 7) ? Y : 0 };
  if (X != 3) return 6;
  if (Y != 7) return 7;
  if (Z != 7) return 8;

  return 0;
}
