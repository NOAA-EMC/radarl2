/*  Byte swap functions used by message31_decoder.c */
/*  Shucai Guan 2012/12 */ 
#include <stdio.h>
#include <string.h>

int reverse_int4(int n)
{
        int i;
        int ibuf;
        signed char *p, *q;
        p=(signed char *)&ibuf;
        q=(signed char *)&n; q+=3;
        for(i=0;i<4;i++)
                *p++=*q--;
        return(ibuf);
}

void swp_int4(int k, int *n)
{
        int i;
        for( i = 0; i < k; i++ )
        n[i] = reverse_int4(n[i]);
        return;
}


short reverse_int2(short n)
{
        int i;
        short ibuf;
        signed char *p, *q;
        p=(signed char *)&ibuf;
        q=(signed char *)&n; q+=1;
        for(i=0;i<2;i++)
                *p++=*q--;
        return(ibuf);
}

void swp_int2(int k, short *n)
{
        int i;
        for( i = 0; i < k; i++ )
        n[i] = reverse_int2(n[i]);
        return;
}

float reverse_float(float value)
{
        int temp =  htonl(*(unsigned int*)&value);
        return *(float*)&temp;
}

/*
float reverse_float(const float fn)
{
	float retVal;
        char *floatToConvert = ( char* ) & fn;
        char *returnFloat = ( char* ) & retVal;

        returnFloat[0] = floatToConvert[3];
        returnFloat[1] = floatToConvert[2];
        returnFloat[2] = floatToConvert[1];
        returnFloat[3] = floatToConvert[0];
        return retVal;
}
*/

void swp_float(int k, float *n)
{
        int i;
        for( i = 0; i < k; i++ )
        n[i] = reverse_float(n[i]);
        return;
}

double reverse_double(double n)
{
        int i;
        double ibuf;
        signed char *p, *q;
        p=(signed char *)&ibuf;
        q=(signed char *)&n; q+=7;
        for(i=0;i<8;i++)
                *p++=*q--;
        return(ibuf);
}

