/*
 * purpose to read the new NOP3 data sets and confirm that 
 * the packets can be read and uncompressed
 */
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <time.h>

#define ABS(a)  ((a)>0?(a):(-1.0*a))
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
int main ( int argc, char **argv )
{
	int ss,mm,hh,dd,mon,yy;
	int	rn,d,b,c,i,iret,oin,r,sz,nb;
	int wsr88d_type,atoi();
	unsigned int	isz,osz;
	char header[32], infn[128];
	char *p, *q, version[4],radar_name[8],volumeid[8];
	char *pb, *pb2;
	char buf[262144],buf2[1050624];
	size_t sizet;
	signed int julian_date;
	signed int milli_seconds;
	rn=d=b=c=0; oin=0;
	while ((i = getopt(argc, argv, "i:")) != -1) {
		switch (i) {
			case 'i' :
				sprintf(infn,"%s",optarg); b++;
				break;
			default :
				break;
		}
	}

	if(b != 0)
	{
		oin = open(infn,O_RDONLY);
		if(oin < 0)
		{
			if(d != 0) perror(infn);
			return(1);
		}
	}
	r = read(oin,header,24); header[24]=0;/* e.g. AR2V0001.220..5...80KABR */
	version[0]=header[7]; version[1]=0;
	wsr88d_type = atoi(&version[0]); /* header[7] == version */

	if(b != 0) { /* user passed the filename */
		/* 012345678901234567890123
		 * KDLH_20071029_134950.bz2
		 */
		p=&volumeid[0]; q=&infn[11];/* KDLH_20071029_134950.bz2 */
		*p++=*q++; *p++=*q++; q++; /* day */
		*p++=*q++; *p++=*q++;      /* hour */
		*p++=*q++; *p++=*q++;      /* minute */
		volumeid[6]=0;
	}
	else
	{
		/* build8 header is composed of the following
		 * char [12] = "ARCHIVE2.%.3d"  	// %.3d is the volume (sequence) number
		 * sample headers BUILD10 output                                    ||
	NOP3_20071018_094014.bz2
	41 52 32 56 30 30 30 33 2e 32 32 30  0  0 35 ed  2 13 38 30 AR2V0003.220..5...80
	4e 4f 50 33  0  0 23 cf 42 5a 68 34 31 41 59 26 53 59 60 74 NOP3..#.BZh41AY&SY`t
	NOP3_20080131_103710.bz2
	41 52 32 56 30 30 30 33 2e 31 38 36  0  0 36 56  2 47 57 f0 AR2V0003.186..6V.GW.
	4b 44 4c 48  0  0 18 7b 42 5a 68 34 31 41 59 26 53 59 f4 25 KDLH...{BZh41AY&SY.%
	NOP3_20080131_174651.bz2
	41 52 32 56 30 30 30 33 2e 32 36 34  0  0 36 56  3 d0 ba f8 AR2V0003.264..6V....
	4e 4f 50 33  0  0 18 85 42 5a 68 34 31 41 59 26 53 59 36 27 NOP3....BZh41AY&SY6'
		 * char [12] = "AR2V000%.1d..%.3d"// %.1d is version, %.3d is the volume number
		 * int*2 unused;
		 * int*2 julian_date; 	// Modified Julian date (day 1 is 1/1/70)
		 * int*4 millisecs_past_midnight; // docs may be wrong
		 * char [4] = "NOP3" // station name C position 20-23
		 * sample header as follows
		 * 012345678901234567890123, C position starts at 0
		 * AR2V0003.220..5...80NOP3
		 */
		p = (char *)&julian_date;/* days since 1970 */
		*p++ = header[12]; *p++ = header[13]; *p++ = header[14]; *p = header[15];
		fprintf(stdout,"days since Jan 1 1970 = %d\t",(int)julian_date);
		p = (char *)&milli_seconds;
		*p++ = header[16]; *p++ = header[17]; *p++ = header[18]; *p = header[19];
		fprintf(stdout,"seconds since midnight = %d\n",milli_seconds/1000);

		julian_date = julian_date + 2440587.5;
		gregorian_date(julian_date,&yy,&mon,&dd);

		fprintf(stdout,"Year=%d, Month=%d, Day=%d\n",yy,mon,dd);

		/*
		 * ./nop3 < NOP3_20071018_094014.bz2
		 * seconds since midnight = 34814
		 * Hours=9, Minutes=0, Seconds=2414
		 * should be 2454402.5
		 */
		ss = (int)(milli_seconds/1000);
		hh = (int)(ss/3600); /* hours */
		mm = (int)((ss-hh*3600)/60); /* minutes */
		ss = ss - hh*3600 - mm*60;
		fprintf(stdout,"Hours=%d, Minutes=%d, Seconds=%d\n",hh,mm,ss);
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
		volumeid[6]=0;
	}
	sprintf(radar_name,"%s",&header[20]); /* header 20-23 is name */
	fprintf(stdout,"radar_name=%s, version=%s, volid=%s\n",radar_name,version,volumeid);

	if(b != 0) close(oin);
	return ( 0 );

}
