#include <stdio.h> 
#include <sys/resource.h> 

double get_memory_(void)                                                                                                         

{
  long Kbytes;
  double Mbytes;
  struct rusage RU;

  getrusage(RUSAGE_SELF, &RU);

  Kbytes = RU.ru_maxrss;

  Mbytes = ((double) Kbytes) / 1024.0;

  return Mbytes;
}

