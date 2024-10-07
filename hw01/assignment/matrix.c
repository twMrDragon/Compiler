
#include <stdio.h>

// problem: given the following 15x15 matrix, find the maximum sum of matrix
// elements with each element being the only one in his row and column

#define N 15

const int m[N][N] = {
  { 7, 53, 183, 439, 863, 497, 383, 563, 79, 973, 287, 63, 343, 169, 583 },
  { 627, 343, 773, 959, 943, 767, 473, 103, 699, 303, 957, 703, 583, 639, 913 },
  { 447, 283, 463, 29, 23, 487, 463, 993, 119, 883, 327, 493, 423, 159, 743 },
  { 217, 623, 3, 399, 853, 407, 103, 983, 89, 463, 290, 516, 212, 462, 350 },
  { 960, 376, 682, 962, 300, 780, 486, 502, 912, 800, 250, 346, 172, 812, 350 },
  { 870, 456, 192, 162, 593, 473, 915, 45, 989, 873, 823, 965, 425, 329, 803 },
  { 973, 965, 905, 919, 133, 673, 665, 235, 509, 613, 673, 815, 165, 992, 326 },
  { 322, 148, 972, 962, 286, 255, 941, 541, 265, 323, 925, 281, 601, 95, 973 },
  { 445, 721, 11, 525, 473, 65, 511, 164, 138, 672, 18, 428, 154, 448, 848 },
  { 414, 456, 310, 312, 798, 104, 566, 520, 302, 248, 694, 976, 430, 392, 198 },
  { 184, 829, 373, 181, 631, 101, 969, 613, 840, 740, 778, 458, 284, 760, 390 },
  { 821, 461, 843, 513, 17, 901, 711, 993, 293, 157, 274, 94, 192, 156, 574 },
  { 34, 124, 4, 878, 450, 476, 712, 914, 838, 669, 875, 299, 823, 329, 699 },
  { 815, 559, 813, 459, 522, 788, 168, 586, 966, 232, 308, 833, 251, 631, 107 },
  { 813, 883, 451, 509, 615, 77, 281, 613, 459, 205, 380, 274, 302, 35, 805 }
};

// solution: function f(i, c) below computes the maximal sum for the
// subset of rows i..N and the subset of columns corresponding to bits
// set in c

#define L 4  // no smaller than log(N)

// to memoize function f
static int memo[1 << (N + L)];

// invariant: 0 <= i <= N, 0 <= c < 2^N, and C has exactly N-i bits set
int f(int i, int c) {
  if (i == N)
    return 0;
  int key = c << L | i;
  int r = memo[key];
  if (r != 0)
    return r;
  int s = 0, j = 0;
  for (j = 0; j < N; j++) {
    int col = 1 << j;
    if ((c & col) == 0)
      continue;
    int x = m[i][j] + f(i + 1, c - col);
    if (x > s) s = x;
  }
  memo[key] = s;
  return s;
}

// solving the problem means calling f with all rows and columns
int main() {
  printf("solution = %d\n", f(0, (1 << N) - 1));
  return 0;
}

/*
Local Variables:
compile-command: "gcc matrix.c && ./a.out"
End:
*/
