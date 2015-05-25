int main() {
int[][] a;
int[][] b = new int[1][1];
b[0][0] = 2;
a = b;
printInt(a[0][0]);
return 0;
}
