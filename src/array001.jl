int main() {

  double[][] a = new double[2][3];

  //int j;
  printInt(123);
  a[0][1] = 4.0;

  int i=0;
  while (i<a.length) {
    int j=0;
    while (j<a[0].length) {
       a[i][j] = 5.0;
       j++;
    }
    i++;
  }
  printInt(123);
  int k=0;
  while (k<a.length) {
    int m=0;
    while (m<a[0].length) {
       printDouble(a[k][m]);
       m++;
    }
    k++;
  }
  
  /*
  for (double[] x : a)
  {
    for (double y : x) printDouble(y);
  }
  //printInt(a[0][1]);

  while (j<a.length) {
     a[j] = j;
     j++;
  }

  for (int x : a) 
     printInt(x);

  int x = 45;
  printInt(x);
*/
  return 0;
}
