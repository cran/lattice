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


/* int anyLeft(int *done, int n) { */
/*      int i; */
/*      for (i=0; i<n; i++) */
/* 	  if (done[i]==0) return i; */
/*      return -1; */
/* } */





/* void marknn(double *x, double *y, int *done, int current, int value, int n, double mdist)  */
/* { */
/*      double tmp, dist; */
/*      int i; */
/*      for (i = 0; i<n; i++) */
/* 	  if (done[i] == 0) { */
/* 	       tmp = x[current] - x[i]; */
/* 	       dist = tmp * tmp; */
/* 	       tmp = y[current] - y[i]; */
/* 	       dist += tmp * tmp; */
/* 	       if (dist < mdist) { */
/* 		    done[i] = value; */
/* 		    marknn(x, y, done, i, value, n, mdist); */
/* 	       } */
/* 	  } */
/*      return; */
/* } */






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
     //tmpSexp = getAttrib(mArg, R_DimSymbol);
     //nx = INTEGER(tmpSexp)[0];
     //ny = INTEGER(tmpSexp)[1];
     nx = asInteger(nxArg);
     ny = asInteger(nyArg);
     nMax = nx + ny;
     //fprintf(stderr, "%d  at:%f\n", nMax, at);
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









/* SEXP old_cont(SEXP mArg, SEXP uxArg, SEXP uyArg, SEXP atArg, SEXP nxArg, SEXP nyArg) */
/* { */
/*      double *m; */
/*      double *ux; */
/*      double *uy; */
/*      double at; */
/*      SEXP ans, tmpSexp; */
/*      double *listx; */
/*      double *listy; */
/*      int *done; */
/*      double *finalx; */
/*      double *finaly; */
/*      int nx, ny, n, nMax, i, j, k, l, next, last, start, value; */
/*      int index = 0; */
/*      double distCutoff, tmp; */
     
/*      m = REAL(mArg); */
/*      ux = REAL(uxArg); */
/*      uy = REAL(uyArg); */
/*      at = asReal(atArg); */
/*      //tmpSexp = getAttrib(mArg, R_DimSymbol); */
/*      //nx = INTEGER(tmpSexp)[0]; */
/*      //ny = INTEGER(tmpSexp)[1]; */
/*      nx = asInteger(nxArg); */
/*      ny = asInteger(nyArg); */
/*      nMax = nx + ny; */
/*      //fprintf(stderr, "%d  at:%f\n", nMax, at); */
/*      listx = Calloc(nMax, double); */
/*      listy = Calloc(nMax, double); */
/*      n = 0; */

/*      for (i = 0; i< nx; i++) */
/* 	  for (j = 0; j< ny-1; j++) */
/* 	  { */
/* 	       if ( (m[i * ny + j] - at) * (m[i * ny + j + 1] - at) <= 0) */
/* 	       { */
/* 		    if (m[i * ny + j] == m[i * ny + j + 1]) { */
/* 			 addToList(ux[i], uy[j]); */
/* 			 addToList(ux[i], uy[j+1]); */
/* 		    } */
/* 		    else  */
/* 			 addToList( ux[i], uy[j] + (uy[j+1]-uy[j]) *  */
/* 				    (at - m[i * ny + j])/(m[i * ny + j + 1]-m[i * ny + j])  ); */
/* 	       } */
/* 	  } */

/*      for (j = 0; j< ny; j++) */
/* 	  for (i = 0; i< nx-1; i++) */
/* 	  { */
/* 	       if ( (m[i * ny + j] - at) * (m[(i+1) * ny + j] - at) <= 0) */
/* 	       { */
/* 		    if (m[i * ny + j] == m[(i + 1) * ny + j]) { */
/* 			 addToList(ux[i], uy[j]); */
/* 			 addToList(ux[i+1], uy[j]); */
/* 		    } */
/* 		    else  */
/* 			 addToList(ux[i] + (ux[i+1]-ux[i]) *  */
/* 				   (at - m[i * ny + j])/(m[(i + 1) * ny + j]-m[i * ny + j]), uy[j]  ); */
/* 	       } */
/* 	  } */


/*      /\* List of x,y-s created, now to create list of adjacent points *\/ */
/*      ans = PROTECT(allocVector(VECSXP, 3)); */
/*      tmpSexp = allocVector(REALSXP, n); */
/*      SET_VECTOR_ELT(ans, 0, tmpSexp); */
/*      finalx = REAL(tmpSexp); */
/*      tmpSexp = allocVector(REALSXP, n); */
/*      SET_VECTOR_ELT(ans, 1, tmpSexp); */
/*      finaly = REAL(tmpSexp); */
/*      tmpSexp = allocVector(INTSXP, n); */
/*      SET_VECTOR_ELT(ans, 2, tmpSexp); */
/*      done = INTEGER(tmpSexp); */
     
/*      //fprintf(stderr, "%d\n", nMax); */
/*      //fprintf(stderr, "Total points : %d\n", n); */

/*      tmp = ux[1]-ux[0]; */
/*      distCutoff = tmp * tmp; */
/*      tmp = uy[1]-uy[0]; */
/*      distCutoff += tmp * tmp; */
     

/*      for (i=0; i<n; i++) { */
/* 	  done[i] = 0; */
/* 	  finalx[i] = listx[i]; */
/* 	  finaly[i] = listy[i]; */
/*      } */
     

/*      value = 1; */
/*      while ((start = anyLeft(done, n)) != -1) { */
/* 	  done[start] = value; */
/* 	  marknn(listx, listy, done, start, value, n, distCutoff); */
/* 	  value++; */
/* 	  fprintf(stderr, "value %d, dist %f\n", value, distCutoff); */
/*      } */


/* /\*      while ((start = anyLeft(done, n)) != -1) { *\/ */

/* /\* 	  done[start] = 2; *\/ */
	  
/* /\* 	  last = -1; // last keeps track of last visited point *\/ */
/* /\* 	  k = start; // k keeps track of current point *\/ */
/* /\* 	  next = -1; *\/ */
/* /\* 	  while (next != start) { *\/ */
	       
/* /\* 	       double minDist = distCutoff * 4;  //R_PosInf; *\/ */
/* /\* 	       double dist; *\/ */


/* /\* 	       finalx[index] = listx[k]; *\/ */
/* /\* 	       finaly[index] = listy[k]; *\/ */
/* /\* 	       index++; *\/ */
	       
/* /\* 	       if (index == n) break; // means that only this one point was left. anyLeft should be 0 now *\/ */

/* /\* 	       //fprintf(stderr, "Going l=%d, k=%d, start=%d, next=%d, index=%d\n", l, k, start, next, index); *\/ */

/* /\* 	       next = k; *\/ */
	       
/* /\* 	       // figure out next point, store index in next *\/ */
/* /\* 	       for (l=0; l<n; l++) { *\/ */
/* /\* 		    if (l!=k) { *\/ */
/* /\* 			 if (done[l] == 1) { *\/ */
/* /\* 			      ;//do nothing *\/ */
/* /\* 			 } *\/ */
/* /\* 			 else if (done[l] == 2) { //either start or some previous start. Ignore latter *\/ */
/* /\* 			      if (l == start && last != start) { // wouldn't want it for the first point after start *\/ */
/* /\* 				   tmp = listx[k]-listx[l]; *\/ */
/* /\* 				   dist = tmp * tmp; *\/ */
/* /\* 				   tmp = listy[k]-listy[l]; *\/ */
/* /\* 				   dist += tmp * tmp; *\/ */
/* /\* 				   if (dist < minDist) { *\/ */
/* /\* 					next = l; *\/ */
/* /\* 					minDist = dist; *\/ */
/* /\* 				   } *\/ */
/* /\* 			      } *\/ */
/* /\* 			 } *\/ */
/* /\* 			 else { // done[l] == 0 *\/ */
/* /\* 			      tmp = listx[k]-listx[l]; *\/ */
/* /\* 			      dist = tmp * tmp; *\/ */
/* /\* 			      tmp = listy[k]-listy[l]; *\/ */
/* /\* 			      dist += tmp * tmp; *\/ */
/* /\* 			      if (dist < minDist) { *\/ */
/* /\* 				   next = l; *\/ */
/* /\* 				   minDist = dist; *\/ */
/* /\* 			      } *\/ */
/* /\* 			 } *\/ */
/* /\* 		    } *\/ */
/* /\* 	       } *\/ */

/* /\* 	       //if (next == -1 || next == k) next = start; *\/ */
/* /\* 	       if (next == k) next = start; *\/ */
/* /\* 	       else if (done[next] == 0) done[next] = 1; *\/ */
/* /\* 	       last = k; *\/ */
/* /\* 	       k = next;	        *\/ */
/* /\* 	  } *\/ */
	  
/* /\*      } *\/ */

/*      //fprintf(stderr, "  reached here\n"); */

/*      Free(listx); */
/*      //fprintf(stderr, "a\n"); */
/*      Free(listy); */
/*      //fprintf(stderr, "b\n"); */
/*      UNPROTECT(1); */
/*      //fprintf(stderr, "c\n"); */
/*      return ans; */
/* } */
