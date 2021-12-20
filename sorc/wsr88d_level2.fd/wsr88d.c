/*
 * level II radar decoder, see the following documentation
 * https://svn.ncep.noaa.gov/vc/Documents/level2_radar/Radar_Lev2_ReDesign.doc
 */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <time.h>
#include <bzlib.h>
#include <unistd.h>
#define ABS(a)  ((a)>0?(a):(-1.0*a))

#ifdef UNDERSCORE
   #define build10_recomb build10_recomb_
   #define bufrdtab bufrdtab_
#endif

void print_version(char *pgm, FILE *file)
{
	fprintf(file,"%s version 1.0.0\n",pgm); 
	fprintf(file,"\tSystems Integration Branch\n");
	fprintf(file,"\tNCEP Central Operations\n");
	fprintf(file,"\tFebruary 06, 2008\n");
	fflush(file);
}
void print_usage(FILE *file)
{
	fprintf(file,"usage: /nwprod/decoders/wsr88d_level2/exec/wsr88d_level2 -uvh <options> -olunout -iinfile\n");
	fprintf(file,"This reads radar compressed data and decompresses it\n");
	fprintf(file,"then performs quality control analysis and writes it to bufr\n");
	fprintf(file,"example:\t /nwprod/decoders/wsr88d_level2/exec/wsr88d_level2 -o 71 -i KDLH_20071029_134950.bz2\n");
	fprintf(file,"example:\t /nwprod/decoders/wsr88d_level2/exec/wsr88d_level2 < KDLH_20071029_134950.bz2\n");
	fflush(file);
}
void print_help(FILE *file)
{
	fprintf(file, "usage: wsr88d_level2 [-uvh] <option option_parameter> where\n");
	fprintf(file, "                -o lunout	[default uses 71]\n");
	fprintf(file, "                -i infile	[default uses stdin]\n");
	fprintf(file, "                -f fix path	[default is /nwprod/decoders/decod_shared/fix]\n");
        fprintf(file, "                -c fixlut path      [default is /nwprod/decoders/wsr88d_level2/fix\n");
	fprintf(file, "                -n nexrad	[default is /nwprod/decoders/decod_shared/dictionaries/nexrad.tbl]\n");
	fprintf(file, "                -s src path	[default is /dcom/us007003/level2_src]\n");
	fprintf(file, "                -r radar_name	[no default]\n");
	fprintf(file, "                -x X_size	[no default]\n"); /* e.g. 1201 */
	fprintf(file, "                -y Y_size	[no default]\n"); /* e.g. 821  */
	fprintf(file, "                -u		[usage]\n");
	fprintf(file, "                -v		[version]\n");
	fprintf(file, "                -h		[help]\n");
	fflush(file);
}
int mygets(pch,fpin)
	char *pch;
	FILE *fpin;
{
	int ch;
	while((ch = fgetc(fpin)) != (int)'\n' && ch != EOF)
	{
		*pch++ = (char)ch;
	}
	*pch = 0;
	return(ch);
}
int  cmpr(p1, p2)
	char *p1, *p2;
{
	while( *p1 != 0 && *p2 != 0 && (*p1 == *p2) )
	{
		p1++;
		p2++;
	}
	if(*p1 == *p2)    return(1);
	return(-1);
	}
void gregorian_date (jd, year,month,day)
	signed int jd;
	int *year,*month,*day;
{
	int i,j,k,l,n;
	l = jd+68569;
	n = 4*l/146097;
	l = l-(146097*n+3)/4;
	i = 4000*(l+1)/1461001;
	l = l-1461*i/4+31;
	j = 80*l/2447;
	k = l-2447*j/80;
	l = j/11;
	j = j+2-12*l;
	i = 100*(n-49)+i+l;
	*year = i;
	*month = j;
	*day = k;
}
#ifdef LITTLE_ENDIAN
unsigned int reverse_order(unsigned n)
{
	int i;
	unsigned int ibuf;
	signed char *p, *q;
	p=(signed char *)&ibuf;
	q=(signed char *)&n; q+=3;
	for(i=0;i<4;i++)
		*p++=*q--;
	*p=0;
	return(ibuf);
}
#endif

void build10_recomb(int *pnum_elevation, int *, int *, int *, double *, int, char *, char *, int, int);
void read_msg31(char *buf, int size, int *pnum_elevation, int wsr88d_type);
char * basename (const char *fname);

void (*wsr88d_funcs[])() =
{ 
        0,
	0,
	0,
	build10_recomb,
	build10_recomb,
	0
};

int reverse_int4(int n);
int main ( int argc, char **argv )
{
	int ss,mm,hh,dd,mon,yy;
	int ifnd,ch,wn,rn,d,b,c,i,iret,oin,r,sz,nb;
	int lunout,wsr88d_type,x,y,atoi();
	int isz;
        unsigned int osz;
	int found;
	char wsr88d_filename[128];
	char infn[128], oufn[128];
	char header[32];
	char *p, *q, version[4],radar_name[8],volumeid[8], *tbasename;
	char *pb, *pb2, *sbuf,*dbuf;
	char buf[262144];
	char nexrad[256],src[256],fix[128],fixlut[128],ffix[256],xsiz[32],ysiz[32];
	double vid, atof();
	FILE *ofix;
	signed int julian_date;
	signed int milli_seconds;

        static int *pnum_elevation, num_elevation;
        num_elevation = 0;
        pnum_elevation = &num_elevation;

        sbuf = NULL;
        dbuf = NULL;

	wn=rn=d=b=c=0; oin=0; found=0;
	x=y=-1;
	lunout=71;
        sprintf(fix,"/nwprod/decoders/decod_shared/fix");
        sprintf(fixlut,"/nwprod/decoders/wsr88d_level2/fix");
        sprintf(nexrad,"/nwprod/decoders/decod_shared/dictionaries/nexrad.tbl");

	sprintf(src,"/dcom/us007003/level2_src");
	while ((i = getopt(argc, argv, "dvuhi:o:f:c:x:y:s:n:r:w:")) != -1) {
		switch (i) {
			case 'v' :
				print_version(argv[0],stderr);
				return(0);
				break;
			case 'u' :
				print_usage(stderr);
				return(0);
				break;
			case 'h' :
				print_help(stderr);
				return(0);
				break;
			case 'd' :
				d++;
				break;
			case 's' :
				sprintf(src,"%s",optarg);
				break;
			case 'n' :
				sprintf(nexrad,"%s",optarg);
				break;
			case 'i' :
				sprintf(infn,"%s",optarg); b++;
				break;
			case 'o' :
				sprintf(oufn,"%s",optarg); c++;
				lunout=atoi(oufn);
				break;
			case 'r' :
				sprintf(radar_name,"%s",optarg); rn++;
				break;
			case 'w' : /* wsr88d radar file name incoming from ldm */
				sprintf(wsr88d_filename,"%s",optarg); wn++;
				break;
			case 'f' :
				/* testing use /meso/noscrub/wx22hl/level2_para/fix */
				sprintf(fix,"%s",optarg);
				break;
                        case 'c' :
                                /* testing use /meso/noscrub/wx22hl/level2_para/fix/SRC_LUT */
                                sprintf(fixlut,"%s",optarg);
                                break;
			case 'x' :
				sprintf(xsiz,"%s",optarg); x=atoi(xsiz);
				break;
			case 'y' :
				sprintf(ysiz,"%s",optarg); y=atoi(ysiz);
				break;
		}
	}

	if(b != 0) { /* user passed the filename */
		oin = open(infn,O_RDONLY);
		if(oin < 0)
		{
			if(d != 0) perror(infn);
			print_usage(stderr);
			return(1);
		}
	}
	r = read(oin,header,24); header[24]=0;/* e.g. AR2V0001.220..5...80KABR */
	version[0]=header[7]; version[1]=0;
	wsr88d_type = atoi(&version[0]); /* header[7] == version */

	if(b != 0) { /* user passed the filename */
                tbasename = basename(infn);
                tbasename += 11;
		p=&volumeid[0]; q=tbasename;/* KDLH_20071029_134950.bz2 */
		*p++=*q++; *p++=*q++; q++; /* day */
		*p++=*q++; *p++=*q++;      /* hour */
		*p++=*q++; *p++=*q++;      /* minute */
		volumeid[6]=0;
		vid=atof(volumeid);
	}
	else if (wn != 0) { /* user passed the wsr88d filename */
                tbasename = basename(wsr88d_filename);
                tbasename += 11;
		p=&volumeid[0]; q=tbasename;/* NOP3_yyyymmdd_hhmmss.bz2 */
		*p++=*q++; *p++=*q++; q++; /* day */
		*p++=*q++; *p++=*q++;      /* hour */
		*p++=*q++; *p++=*q++;      /* minute */
		volumeid[6]=0;
		vid=atof(volumeid);
	}
	else { /* get the volume id from the integer data within the header */
		/*
		 * char [12] = "AR2V000%.1d..%.3d"// %.1d is version, %.3d is the volume number
		 * int*2 unused;
		 * int*2 julian_date; 	// Modified Julian date (day 1 is 1/1/70)
		 * int*4 millisecs_past_midnight; // docs may be wrong
		 * char [4] = "NOP3" // station name C position 20-23
		 * sample header as follows
		 * 012345678901234567890123, C position starts at 0
		 * AR2V0003.220..5...80NOP3
		 */

		/* what day of the month is it */
		p = (char *)&julian_date; /* value is in days */
		*p++ = header[14]; *p = header[15];
		/*fprintf(stdout,"days since Jan 1 1970 = %d\t",(int)julian_date);*/
		julian_date = julian_date + 2440587.5;
		gregorian_date(julian_date,&yy,&mon,&dd);

		/* what hour and minute of the day is it */
		p = (char *)&milli_seconds; /* time past midnight */
		*p++ = header[16]; *p++ = header[17]; *p++ = header[18]; *p = header[19];

		ss = (int)(milli_seconds/1000);
		hh = (int)(ss/3600); /* hours */
		mm = (int)((ss-hh*3600)/60); /* minutes */
		ss = ss - hh*3600 - mm*60; /* we don't use it but we could */
		/*fprintf(stdout,"Hours=%d, Minutes=%d, Seconds=%d\n",hh,mm,ss);*/
		/* now put together the day-hour-minutes (ddhhmm) */
		/* vid = dd + hh + mm and preserve the zeros */
		if(dd < 10) {
			sprintf(&volumeid[1],"%1d",dd); volumeid[0]='0';
		}
		else {
			sprintf(&volumeid[0],"%2d",dd);
		}
		if(hh < 10) {
			sprintf(&volumeid[3],"%1d",hh); volumeid[2]='0';
		}
		else {
			sprintf(&volumeid[2],"%2d",hh);
		}
		if(mm < 10) {
			sprintf(&volumeid[5],"%1d",mm); volumeid[4]='0';
		}
		else {
			sprintf(&volumeid[4],"%2d",mm);
		}
	}
	volumeid[6]=0;
	if(rn != 0) /* user passed the radar name as an argument */
	{
		infn[4]=0; /* let user override name with -r option */
		sprintf(radar_name,"%s",&infn[0]); /* filename 0-3 is name */
	}
	else /* other wise get it from the 24 byte header */
		sprintf(radar_name,"%s",&header[20]); /* header 20-23 is name */


/* vid is used by Shun in the bufr output - new in 2007 */

	/*
	 * now check for the fix tables under /nwprod/fix
	 */
	if (wsr88d_type >= 0 && wsr88d_type <= 7)
	{
		/* if user did not pass the x and y dimensions then parse mosaic.table */
		if(x == -1 || y == -1)
		{
			sprintf(ffix,"%s/%s",fixlut,"mosaic.table");
			ofix = fopen(ffix,"r");
			if(ofix == NULL)
			{
				close(oin);
				perror(ffix);
				print_usage(stderr);
				return(2);
			}
			ch = mygets(buf,ofix); /* get the first line */
	
		/* sample mosaic table entry
	RADAR001 KABR        601  601  31  0.01  0.01  45.4558  -98.4131   1         -1
	  */
	
			ch = mygets(buf,ofix); /* get the x and y from the fix file */
			while(ch != EOF)
			{
				p = buf;
				while(*p != ' ' && *p != 0) p++;
				while(*p == ' ') p++;
				q = p; while(*q != ' ' && *q != 0) q++; *q++=0;
				if(cmpr(radar_name,p) != 1)
				{
					ch = mygets(buf,ofix);
					continue;
				}
				found=1; break;
			}
			fclose(ofix);
			if(found != 1)
			{
				close(oin);
				perror(ffix);
				print_usage(stderr);
				return(3);
			}
			p=q;
			while(*p == ' ') p++;
			q = p; while(*q != ' ' && *q != 0) q++; *q++=0;
			x=atoi(p);
			p=q;
			while(*p == ' ') p++;
			q = p; while(*q != ' ' && *q != 0) q++; *q++=0;
			y=atoi(p);
		}
	}/* end of the mosaic fix table by Shun Liu for build 8 */

	bufrdtab(nexrad,radar_name,&ifnd);
	if(ifnd != 1)
	{
		close(oin);
                printf("can not find radar name: %s in %s\n", radar_name, nexrad);  
		printf("put the full path to the nexrad.tbl and check whether there is 24 bytes header in input file.\n");
		print_usage(stderr);
                exit (0);
		return(4);
	}

	r = read(oin,&sz,4); isz=sz;
#ifdef LITTLE_ENDIAN
	isz=reverse_order(isz);
#endif
	isz=sz=ABS(isz);
        if (sbuf != NULL)
            free(sbuf);
        if (isz == 0 )
        {
           printf("Error, wrong data in input file \n");
           exit (0); 
        }
        sbuf = malloc(isz);

        if (dbuf != NULL)
            free(dbuf);
        osz=204800;
        dbuf = malloc(osz);

	/* metadata */
	r = read(oin,sbuf,sz);/* if we used the metadata we would uncomment the next line */
	/*iret = BZ2_bzBuffToBuffDecompress(dbuf,&osz,sbuf,isz,0,0);*/

	/*
	 * the data comes in packets whose size is defined by an integer
	 * all records are some multiple of 2432
	 */
	r = read(oin,&sz,4); isz=sz;
#ifdef LITTLE_ENDIAN
	isz=reverse_order(isz);
#endif
	isz=sz=ABS(isz);

	pb = NULL;
	nb = 0;

	while(r > 0) { /* read and uncompress each successive record */
                if (isz == 0 )
                {
                   printf("Error, wrong data in input file \n");
                   exit (0); 
                }
                sbuf = realloc(sbuf, isz);
		r = read(oin,sbuf,sz);
/*
                dbuf = realloc(dbuf, osz);
*/
                iret = BZ2_bzBuffToBuffDecompress(dbuf,&osz,sbuf,sz,0,0);
                while (iret == BZ_OUTBUFF_FULL) {
                /*       Increase dbuf size.     */
                   osz = (int)  (osz * 2);
                   if (osz <= 0 )
                   {
                       printf("Error, data do not compressed properly \n");
                       exit (0);
                   } 
                   dbuf = realloc(dbuf, osz);
                   iret = BZ2_bzBuffToBuffDecompress(dbuf,&osz,sbuf,sz,0,0);
                }
		if(iret == 0) {

		    pb2 = realloc( pb, nb+osz );
		    if ( pb2 == NULL ) {
			fprintf( stdout, "Memory allocation failure!\n" );
		    }
		    else {  /* append contents of buf2 to pb */
		        pb = pb2;
			p = &pb[nb]; q = dbuf;
			for ( i=0; i < osz; i++ ) {
			  *p++ = *q++;
			}
			nb += osz;
		    }
		}
		r = read(oin,&sz,4); 
/*
                if (sz < 0 ) sz = -1 * sz;
*/
                isz=sz;
#ifdef LITTLE_ENDIAN
		isz=reverse_int4(isz);
#endif
		isz=sz=ABS(isz);
	}
        if (sbuf != NULL)
           free(sbuf);
        if (dbuf != NULL)
           free(dbuf);
	if ( nb > 0 ) { 
	    printf("fix = >%s<; length = %d\n", fix, strlen(fix) );
	    printf("radar_name = >%s<; length = %d\n", radar_name, strlen(radar_name) );
	    printf("wsr88d_type = %d\n", wsr88d_type );
            if ( wsr88d_type >= 2 && wsr88d_type <= 7 )
            {
               read_msg31(pb, nb, pnum_elevation, wsr88d_type);
               nb = strlen(radar_name);
               close(oin);
               wsr88d_funcs[3](pnum_elevation,&lunout,&x,&y,&vid,&nb,fix,fixlut,strlen(fix),strlen(fixlut));
            }
            else 
               wsr88d_funcs[1](&lunout,&nb,&x,&y,&vid,radar_name,fix,fixlut,pb,
						strlen(radar_name),strlen(fix),strlen(fixlut),nb);
        }
        if (pb != NULL)
	    free(pb);

	/* Close input wsr88d file */
	if(b != 0) close(oin);
/*
        sleep (10);
         printf("end normall\n");
*/
	return ( 0 );
}
