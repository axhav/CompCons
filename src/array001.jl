int main() {

  double[][] a = new double[2][3];

  //int j;
  a[0][1] = 4.0;
  
  
  for (double[] x : a)
  {
    for (double y : x) printDouble(y);
  }
  //printInt(a[0][1]);
/*
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
