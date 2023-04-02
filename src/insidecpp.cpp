#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
bool insidecpp (
        const NumericVector &xy,
        const int    n1,
        const int    n2,
        const NumericMatrix &poly)
{// Author: Efford Murray, GPL license
    // Is point xy inside poly?
    // Based on contribution on s-news list by Peter Perkins 23/7/96
    // We assume poly is closed, and in col-major order (x's then y's)
    
    double theta = 0;
    double cutoff = 1e-6;
    int k;
    int ns;
    double N;
    double d;
    ns = n2 - n1 + 1;   // number of selected points 
    std::vector<double> temp((ns+1) * 2);
    
    // get & translate to coords centered at each test point 
    for (k=0; k < ns; k++)
    {
        temp[k]      = poly(k + n1,0) - xy[0];    // x 
        temp[k + ns] = poly(k + n1,1) - xy[1];    // y 
    }
    
    for (k=0; k < (ns-1); k++)
    {
        N = temp[k] * temp[k+1 + ns] - temp[k + ns] * temp[k+1];
        d = temp[k] * temp[k+1]      + temp[k + ns] * temp[k+1 + ns];
        if (fabs(d)>0) { N = N/fabs(d);  d = d/fabs(d); }
        theta += atan2(N, d);
    }
    theta = fabs(theta);
    return (fabs(theta - 2* M_PI) < cutoff);    // M_PI is cmath.h constant 
}