int main() {

  double[][] a = new double[2][3];
  a[0][1] = 4.0;
  printDouble(a[0][0]);
  incFstIndex(a);
  printDouble(a[0][0]);
  
  double [][] b = addTwoArray(a);
  printDouble(b[0][0]); 
  return 0;
}

void incFstIndex (double [][] a){
    a[0][0] = a[0][0] + 1.0;
}

double [][] addTwoArray (double [][] a){
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
