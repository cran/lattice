#include <R.h>
#include <Rinternals.h>


/* #define addToList(x, y) \ */
/* do { \ */
/*      double __x__ = (x); \ */
/*      double __y__ = (y); \ */
/*      if (nMax == n) { \ */
/* 	  nMax *= 2; \ */
/* 	  listx = Realloc(listx, nMax, double); \ */
/* 	  listy = Realloc(listy, nMax, double); \ */
/*      } \ */
/*      listx[n] = __x__; \ */
/*      listy[n] = __y__; \ */
/*      n++; \ */
/* } while(0) */


#define add2List(x1, y1, x2, y2) \
do { \
     double __x1__ = (x1); \
     double __y1__ = (y1); \
     double __x2__ = (x2); \
     double __y2__ = (y2); \
     if (nMax == n) { \
	  nMax *= 2; \
	  listx1 = Realloc(listx1, nMax, double); \
	  listy1 = Realloc(listy1, nMax, double); \
	  listx2 = Realloc(listx2, nMax, double); \
	  listy2 = Realloc(listy2, nMax, double); \
     } \
     listx1[n] = __x1__; \
     listy1[n] = __y1__; \
     listx2[n] = __x2__; \
     listy2[n] = __y2__; \
     n++; \
} while(0)








SEXP cont(SEXP mArg, SEXP uxArg, SEXP uyArg, SEXP atArg, SEXP nxArg, SEXP nyArg)
{
     double *m;
     double *ux;
     double *uy;
     double at;
     SEXP ans, tmpSexp;
     double *listx1, *listx2;
     double *listy1, *listy2;
     double *finalx1, *finalx2;
     double *finaly1, *finaly2;
     int nx, ny, n, nMax, i, j, k, l, next, last, start, value;
     int index = 0;
     double x1, y1, x2, y2;
     int isx1, isx2, isy1, isy2, istotal;

     m = REAL(mArg);
     ux = REAL(uxArg);
     uy = REAL(uyArg);
     at = asReal(atArg);

     nx = asInteger(nxArg);
     ny = asInteger(nyArg);
     nMax = nx + ny;

     listx1 = Calloc(nMax, double);
     listy1 = Calloc(nMax, double);
     listx2 = Calloc(nMax, double);
     listy2 = Calloc(nMax, double);
     n = 0;

     for (i = 0; i< nx-1; i++)
	  for (j = 0; j< ny-1; j++)
	  {
	       isx1 = isx2 = isy1 = isy2 = 0;
	       if ( (m[i * ny + j] - at) * (m[i * ny + j + 1] - at) <= 0)
	       {
		    if (m[i * ny + j] == m[i * ny + j + 1]) {
			 add2List(ux[i], uy[j], ux[i], uy[j+1]);
		    }
		    else {
			 y1 = uy[j] + (uy[j+1]-uy[j]) * 
			      (at - m[i * ny + j])/(m[i * ny + j + 1]-m[i * ny + j]);
			 isy1 = 1;
		    }
	       }
	       if ( (m[i * ny + j] - at) * (m[(i+1) * ny + j] - at) <= 0)
	       {
		    if (m[i * ny + j] == m[(i+1) * ny + j]) {
			 add2List(ux[i], uy[j], ux[i+1], uy[j]);
		    }
		    else {
			 x1 = ux[i] + (ux[i+1]-ux[i]) * 
			      (at - m[i * ny + j])/(m[(i+1) * ny + j]-m[i * ny + j]);
			 isx1 = 1;
		    }
	       }
	       if ( (m[(i+1) * ny + j] - at) * (m[(i+1) * ny + j + 1] - at) <= 0)
	       {
		    if (m[(i+1) * ny + j] == m[(i+1) * ny + j + 1]) {
			 add2List(ux[i+1], uy[j], ux[i+1], uy[j+1]);
		    }
		    else {
			 y2 = uy[j] + (uy[j+1]-uy[j]) * 
			      (at - m[(i+1) * ny + j])/(m[(i+1) * ny + j + 1]-m[(i+1) * ny + j]);
			 isy2 = 1;
		    }
	       }
	       if ( (m[i * ny + j + 1] - at) * (m[(i+1) * ny + j + 1] - at) <= 0)
	       {
		    if (m[i * ny + j + 1] == m[(i+1) * ny + j + 1]) {
			 add2List(ux[i], uy[j+1], ux[i+1], uy[j+1]);
		    }
		    else {
			 x2 = ux[i] + (ux[i+1]-ux[i]) * 
			      (at - m[i * ny + j + 1])/(m[(i+1) * ny + j + 1]-m[i * ny + j + 1]);
			 isx2 = 1;
		    }
	       }
	       istotal = isx1 + isx2 + isy1 + isy2;
	       if (istotal == 2) {
		    if (isx1 && isx2) add2List(x1, uy[j], x2, uy[j+1]);
		    if (isy1 && isy2) add2List(ux[i], y1, ux[i+1], y2);
		    if (isx1 && isy1) add2List(x1, uy[j], ux[i], y1);
		    if (isx1 && isy2) add2List(x1, uy[j], ux[i+1], y2);
		    if (isx2 && isy1) add2List(ux[i], y1, x2, uy[j+1]);
		    if (isx2 && isy2) add2List(x2, uy[j+1], ux[i+1], y2);
	       }
	       else if (istotal == 4) {
		    if (x1 <= x2) {
			 add2List(x1, uy[j], ux[i], y1);
			 add2List(x2, uy[j+1], ux[i+1], y2);
		    }
		    else {
			 add2List(x1, uy[j], ux[i+1], y2);
			 add2List(x2, uy[j+1], ux[i], y1);
		    }
	       }
	  }

     /* List of x,y-s created, now to create list of adjacent points */
     ans = PROTECT(allocVector(VECSXP, 4));
     tmpSexp = allocVector(REALSXP, n);
     SET_VECTOR_ELT(ans, 0, tmpSexp);
     finalx1 = REAL(tmpSexp);
     tmpSexp = allocVector(REALSXP, n);
     SET_VECTOR_ELT(ans, 1, tmpSexp);
     finaly1 = REAL(tmpSexp);

     tmpSexp = allocVector(REALSXP, n);
     SET_VECTOR_ELT(ans, 2, tmpSexp);
     finalx2 = REAL(tmpSexp);
     tmpSexp = allocVector(REALSXP, n);
     SET_VECTOR_ELT(ans, 3, tmpSexp);
     finaly2 = REAL(tmpSexp);

     for (i=0; i<n; i++) {
	  finalx1[i] = listx1[i];
	  finaly1[i] = listy1[i];
	  finalx2[i] = listx2[i];
	  finaly2[i] = listy2[i];
     }

     Free(listx1);
     Free(listy1);
     Free(listx2);
     Free(listy2);
     UNPROTECT(1);
     return ans;
}




