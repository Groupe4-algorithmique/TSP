#include <Rcpp.h>
#include<vector>
using namespace Rcpp;

// [[Rcpp::export]] //mandatory to export the function
void swap(NumericVector v,unsigned int i,unsigned int j)
{
  int tmp = v[i];
  v[i] = v[j];
  v[j] = tmp;
}



/*** R
a= c(1,2,3)
swap(a,1,2)
*/
