#include <R.h>
#include <Rinternals.h>





static double calculateCosOfAngleOfReflection(double *x, double *y, double *z,
					      double *ls) 
{
    /* Bad name, change later. The idea is, given the quadrilateral
     * defined by x, y, z, find the cosine of half the angle between a
     * point in the z-axis at infinite distance and the ray reflected
     * by the plane defined by the first three points, where the ray
     * comes from an infinitely distant source in the direction of
     * ls. This means that the result would be the same for any
     * section in a given plane. Could use improvement */

     double x1, x2, y1, y2, z1, z2, cos1, cos2, ans, xm, ym, zm;

     xm = (x[0] + x[1] + x[2] + x[3])/4;
     ym = (y[0] + y[1] + y[2] + y[3])/4;
     zm = (z[0] + z[1] + z[2] + z[3])/4;

     ans = 0;

     /* Theory : We now want the normal to the plane defined by 2 of
      * these points and the average of all four, passing through the
      * average. This has direction (y1 z2 - y2 z1, z1 x2 - z2 x1, x1
      * y2 - x2 y1). We use this to calculate cos of the final angle.
      * Don't feel like writing all the details, but hint : cos1 = cos
      * of angle between z axis and light source, cos2 = angle between
      * z-axis and normal at origin. After all 4 successive pairs,
      * combine these 4 */


     x1 = x[0] - xm;
     y1 = y[0] - ym;
     z1 = z[0] - zm;

     x2 = x[1] - xm;
     y2 = y[1] - ym;
     z2 = z[1] - zm;

     cos1 = ls[2]; /* 0.ls[0] + 0.ls[1] + 1.ls[2] */
     cos2 = x1 * y2 - x2 * y1;
     /* Needs to be normalised */
     cos2 /= sqrt((x1 * y2 - x2 * y1) * (x1 * y2 - x2 * y1) +
		  (y1 * z2 - y2 * z1) * (y1 * z2 - y2 * z1) +
		  (z1 * x2 - z2 * x1) * (z1 * x2 - z2 * x1)); 

     ans += cos2 * cos2 * ((1 + cos1)/2) + 
	     (1 - cos2 * cos2) * ((1 - cos1)/2) + 
	     cos2 * sqrt((1 - cos2 * cos2) * (1 - cos1 * cos1));


     x1 = x[1] - xm;
     y1 = y[1] - ym;
     z1 = z[1] - zm;

     x2 = x[2] - xm;
     y2 = y[2] - ym;
     z2 = z[2] - zm;

     cos1 = ls[2]; /* 0.ls[0] + 0.ls[1] + 1.ls[2] */
     cos2 = x1 * y2 - x2 * y1;
     /* Needs to be normalised */
     cos2 /= sqrt((x1 * y2 - x2 * y1) * (x1 * y2 - x2 * y1) +
		  (y1 * z2 - y2 * z1) * (y1 * z2 - y2 * z1) +
		  (z1 * x2 - z2 * x1) * (z1 * x2 - z2 * x1)); 

     ans += cos2 * cos2 * ((1 + cos1)/2) + 
	     (1 - cos2 * cos2) * ((1 - cos1)/2) + 
	     cos2 * sqrt((1 - cos2 * cos2) * (1 - cos1 * cos1));


     x1 = x[2] - xm;
     y1 = y[2] - ym;
     z1 = z[2] - zm;

     x2 = x[3] - xm;
     y2 = y[3] - ym;
     z2 = z[3] - zm;

     cos1 = ls[2]; /* 0.ls[0] + 0.ls[1] + 1.ls[2] */
     cos2 = x1 * y2 - x2 * y1;
     /* Needs to be normalised */
     cos2 /= sqrt((x1 * y2 - x2 * y1) * (x1 * y2 - x2 * y1) +
		  (y1 * z2 - y2 * z1) * (y1 * z2 - y2 * z1) +
		  (z1 * x2 - z2 * x1) * (z1 * x2 - z2 * x1)); 

     ans += cos2 * cos2 * ((1 + cos1)/2) + 
	     (1 - cos2 * cos2) * ((1 - cos1)/2) + 
	     cos2 * sqrt((1 - cos2 * cos2) * (1 - cos1 * cos1));



     x1 = x[3] - xm;
     y1 = y[3] - ym;
     z1 = z[3] - zm;

     x2 = x[0] - xm;
     y2 = y[0] - ym;
     z2 = z[0] - zm;

     cos1 = ls[2]; /* 0.ls[0] + 0.ls[1] + 1.ls[2] */
     cos2 = x1 * y2 - x2 * y1;
     /* Needs to be normalised */
     cos2 /= sqrt((x1 * y2 - x2 * y1) * (x1 * y2 - x2 * y1) +
		  (y1 * z2 - y2 * z1) * (y1 * z2 - y2 * z1) +
		  (z1 * x2 - z2 * x1) * (z1 * x2 - z2 * x1)); 

     ans += cos2 * cos2 * ((1 + cos1)/2) + 
	     (1 - cos2 * cos2) * ((1 - cos1)/2) + 
	     cos2 * sqrt((1 - cos2 * cos2) * (1 - cos1 * cos1));

     return ans/4;
}






void wireframePanelCalculations(SEXP xArg, SEXP yArg, SEXP zArg, SEXP rotArg, 
				SEXP zaArg, SEXP zbArg, 
				SEXP nxArg, SEXP nyArg, SEXP ngArg,
				SEXP lsArg,
				SEXP env)
{
     /* Arguments supplied */
     double *x;     /* increasing vector of unique x values                      */
     double *y;     /* increasing vector of unique y values                      */
     double *z;     /* long vector of z values, length = nx * ny * ng            */
     double *rot;   /* rotation matrix 4x4 (homogeneous coordinates, as vector)  */
     double za, zb; /* for perspective calculations                              */
     int nx, ny, ng;/* length of x, y and number of groups (conceptually ncol(z))*/  
     double *ls;    /* light source coordinates (norm 1)                         */

     /* Other variables */
     SEXP call, sHeights, sxx, syy, szz, smisc;
     double *heights, *xx, *yy, *zz, *misc;
     int *horder;
     int nh, i;

     x = REAL(xArg);
     y = REAL(yArg);
     z = REAL(zArg);
     rot = REAL(rotArg);

     za = asReal(zaArg);
     zb = asReal(zbArg);
     
     ls = REAL(lsArg);
     
     nx = asInteger(nxArg);
     ny = asInteger(nyArg);
     ng = asInteger(ngArg);

     /* heights corresponds to all the rectangles in the grid. The
      * indexing is conceptually according to the lower left corner of
      * the rectangle. Thus, the length of heights is
      * (nx-1)*(ny-1)*ng. Given i, would later need to figure out
      * which rectangle this corresponds to  */

     nh = (nx-1) * (ny-1) * ng;
     sHeights = PROTECT(allocVector(REALSXP, nh));
     heights = REAL(sHeights);
     
     /* printf("\nStarting height calculation..."); */


     for (i = 0; i < nh; i++) {
         double tx, ty, tz, th;
	 int txi, tyi, tgi, ti;

	 tgi = i / ((nx-1)*(ny-1)); /* group index             */
	 ti =  i % ((nx-1)*(ny-1)); /* height index w/in group */
	 tyi = ti % (ny-1);         /* y index                 */
	 txi = ti / (ny-1);         /* x index                 */


	 /* (0,0)  corner */
	 tx = x[txi];
	 ty = y[tyi];
	 tz = z[tgi * nx * ny + txi * ny + tyi];
	 heights[i] = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);


	 /* (1,0) corner */
	 tx = x[txi + 1];
	 /* ty = y[tyi]; */
	 tz = z[tgi * nx * ny + (txi + 1) * ny + tyi];
	 th = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);
	 if (th > heights[i]) heights[i] = th;


	 /* (1,1) corner */
	 /* tx = x[txi + 1]; */
	 ty = y[tyi + 1];
	 tz = z[tgi * nx * ny + (txi + 1) * ny + tyi + 1];
	 th = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);
	 if (th > heights[i]) heights[i] = th;


	 /* (0,1) corner */
	 tx = x[txi];
	 /* ty = y[tyi + 1]; */
	 tz = z[tgi * nx * ny + txi * ny + tyi + 1];
	 th = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) 
		 / (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);
	 if (th > heights[i]) heights[i] = th;
     }

     /* printf("\nFinished height calculation, ordering..."); */
     
     call = PROTECT(lang2(install("order"), sHeights));
     sHeights = eval(call, R_GlobalEnv);
     UNPROTECT(2);
     horder = INTEGER(PROTECT(sHeights));

     /* printf("\nFinished ordering, proceeding..."); */


     sxx = PROTECT(allocVector(REALSXP, 4));
     syy = PROTECT(allocVector(REALSXP, 4));
     szz = PROTECT(allocVector(REALSXP, 4));
     smisc = PROTECT(allocVector(REALSXP, 3));
     xx = REAL(sxx);
     yy = REAL(syy);
     zz = REAL(szz);

     misc = REAL(smisc); 
     /* 0: cos angle with normal 
	1: original z-height averaged
	2: group indicator
     */

     /*
     for (i = 0; i < 16; i++) 
	     printf("\nrot.mat: %f", rot[i]);

     printf("\na : %f   zb : %f", za, zb);

     printf("\n nx = %d, ny = %d", nx, ny);
     printf("\nGroup\tx\ty\n");
     */


     call = PROTECT(lang4(install("wirePolygon"), sxx, syy, smisc));

     for (i = 0; i < nh; i++) {
         double tx, ty, tz, txx, tyy, tzz, tmp;
	 int txi, tyi, tgi, ti;

	 ti = horder[i] - 1;

	 tgi = ti / ((nx-1)*(ny-1)); /* group index             */
	 ti =  ti % ((nx-1)*(ny-1)); /* height index w/in group */
	 tyi = ti % (ny-1);          /* y index                 */
	 txi = ti / (ny-1);          /* x index                 */


	 /*
	 printf("\n%d\t%d\t%d\t(%d,%d,%d,%d)", tgi, txi, tyi,
		tgi * nx * ny + txi * ny + tyi,
		tgi * nx * ny + (txi + 1) * ny + tyi,
		tgi * nx * ny + (txi + 1) * ny + tyi + 1,
		tgi * nx * ny + txi * ny + tyi + 1);
	 */

	 misc[2] = (double) tgi + 1;
	 misc[1] = 0;

	 /* (0,0) corner */
	 tx = x[txi];
	 ty = y[tyi];
	 tz = z[tgi * nx * ny + txi * ny + tyi];

	 misc[1] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 txx *= (za + zb * tzz);
	 tyy *= (za + zb * tzz);


	 /*printf("\n%f\t%f\t%f\t%f\t%f\t%f", tx, ty, tz, txx, tyy, tzz);*/


	 xx[0] = txx;
	 yy[0] = tyy;
	 zz[0] = tzz;

	 /* (1,0) corner */
	 tx = x[txi + 1];
	 ty = y[tyi];
	 tz = z[tgi * nx * ny + (txi + 1) * ny + tyi];

	 misc[1] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 txx *= (za + zb * tzz);
	 tyy *= (za + zb * tzz);

	 xx[1] = txx;
	 yy[1] = tyy;
	 zz[1] = tzz;




	 /* (1,1) corner */
	 tx = x[txi + 1];
	 ty = y[tyi + 1];
	 tz = z[tgi * nx * ny + (txi + 1) * ny + tyi + 1];

	 misc[1] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 txx *= (za + zb * tzz);
	 tyy *= (za + zb * tzz);

	 xx[2] = txx;
	 yy[2] = tyy;
	 zz[2] = tzz;




	 /* (0,1) corner */
	 tx = x[txi];
	 ty = y[tyi + 1];
	 tz = z[tgi * nx * ny + txi * ny + tyi + 1];

	 misc[1] += tz;
	 tmp = (rot[3] * tx + rot[7] * ty + rot[11] * tz + rot[15]);

	 txx = (rot[0] * tx + rot[4] * ty + rot[8] * tz + rot[12]) / tmp;
	 tyy = (rot[1] * tx + rot[5] * ty + rot[9] * tz + rot[13]) / tmp;
	 tzz = (rot[2] * tx + rot[6] * ty + rot[10] * tz + rot[14]) / tmp;

	 txx *= (za + zb * tzz);
	 tyy *= (za + zb * tzz);

	 xx[3] = txx;
	 yy[3] = tyy;
	 zz[3] = tzz;



	 /* Done with all four corners */

	 misc[1]/=4;
	 misc[0] = calculateCosOfAngleOfReflection(xx, yy, zz, ls);

	 eval(call, env);



     }

     UNPROTECT(6);
     return;
}




