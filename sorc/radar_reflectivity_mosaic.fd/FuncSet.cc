/*---------------------------------------------------------------------------
	Program:	FuncSet.cc
	Function:	Small functions
	Content:	print_line()
			print_star()
			print_time()
			SetCurrentTime()
			Make_Time()
	Author:		Fang Zhao (10/09/2001)
	Modification:	
-------------------------------------------------------------*/
#include <ctime>
#include <iostream>
#include <cmath>
#include <cstring>
#include <cstdlib>
#include <cstdio>

#include "DefineAndStruc.h"
#include "FuncSet.h"

using namespace std;

void GetTimeStep(Cur_Time, Cur_Time &, int);

void print_line()
{
    cout<<"---------------------------------------------------"<<endl<<endl;
}

void print_star()
{
    cout<<"***************************************************"<<endl<<endl;
}

void print_time(char *text)
{
    time_t lt;
    struct tm *ptr;
    char now_time[127];

    lt = time(NULL);
    ptr = localtime(&lt);
 
    sprintf(now_time,"%04d-%02d-%02d-%02d:%02d:%02d", ptr->tm_year+1900,
                     ptr->tm_mon+1, ptr->tm_mday, ptr->tm_hour, ptr->tm_min,
                     ptr->tm_sec);
    now_time[strlen(now_time)]='\0';
    cout<<text<<"   :   "<<now_time<<endl<<endl;
}

void SetCurrentTime(Cur_Time *cur_time)
{
    time_t lt;
    struct tm *ptr;

    lt = time(NULL);
    ptr = gmtime(&lt);
 
    cur_time->year = ptr->tm_year+1900;
    cur_time->mon = ptr->tm_mon+1;
    cur_time->day = ptr->tm_mday;
    cur_time->hour = ptr->tm_hour;
    cur_time->min = ptr->tm_min;
}

time_t Make_Time(struct tm *tim)
{
    time_t timer=0;
    int i;
    int maxd[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

    for(i=1970-1900;i<tim->tm_year;i++)
    {
        timer += 31536000l;
        if ((i%4)==0) timer += 86400l;
    }

    if ((tim->tm_year%4)==0) maxd[1] = 29;
    for(i=1;i<tim->tm_mon;i++)
        timer += (maxd[i-1]*86400l);
    for(i=1;i<tim->tm_mday;i++)
        timer += 86400l;
    for(i=0;i<tim->tm_hour;i++)
        timer += 3600l;
    for(i=0;i<tim->tm_min;i++)
        timer += 60l;
    for(i=0;i<tim->tm_sec;i++)
        timer ++;

    return timer;
}

void GetTimeStep(Cur_Time init_time, Cur_Time &cur_time, int seconds)
{
    cout<<"Init time: "<<init_time.year<<"/"<<init_time.mon<<"/"
        <<init_time.day<<"/"<<init_time.hour<<endl;

    cout<<"time_lag= "<<seconds<<endl;

    struct tm *time_tmp;
    time_tmp = new (struct tm);
 
    time_tmp->tm_year = init_time.year - 1900;
    time_tmp->tm_mon = init_time.mon;
    time_tmp->tm_mday = init_time.day;
    time_tmp->tm_hour = init_time.hour;
    time_tmp->tm_min = init_time.min;
    time_tmp->tm_sec = 0;

    time_t lt;
    lt = Make_Time(time_tmp); 
    delete time_tmp;
    lt += seconds;

    struct tm *ptr;
    ptr = gmtime(&lt);

    cur_time.year = ptr->tm_year + 1900;
    cur_time.mon = ptr->tm_mon + 1;
    cur_time.day = ptr->tm_mday;
    cur_time.hour = ptr->tm_hour;
    cur_time.min = ptr->tm_min;
    cout<<"Final time: "<<cur_time.year<<"/"<<cur_time.mon<<"/"
        <<cur_time.day<<"/"<<cur_time.hour<<endl;

    return;
}
