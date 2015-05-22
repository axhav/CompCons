int main() {

  double[][] a = new double[2][3];
  printDouble(a[0][0]);
  incFstIndex(a);
  printDouble(a[0][0]);

  double [][] b = createNewArray(a);
  printDouble(b[0][0]);
  
  // Test three dimension array works.
  int [][][] c = new int[2][3][4];
  c[1][1][1] = 5;
  printInt(c[1][1][1]);
  return 0;
}

// Test that input array works for function.
void incFstIndex (double [][] a){
    a[0][0] = a[0][0] + 1.0;
}

// Test that functions can return array types correct.
double [][] createNewArray (double [][] a){
    double [][] res = new double [a . length][a[0].length];

    int i = 0 ;
    for (double n : a){
        int j = 0;
        for (double m : a[i]){
            res [i][j] = 2.0 + m ;
            j++;
        }
        i ++ ;
     }
     return res;
}
