      SUBROUTINE VADLSF(NAZIMS,NRADIALS,MISSING,AZM,VE,DNPT,
     $                           CF1,CF2,CF3)
c.********************************************************************
c.                    M O D U L E  P R O L O G U E
c.
c.  MODULE FUNCTION:
c.
c.     This module least squares fits a sine-wave curve to velocity
c.     data points. Data used to perform the fitting is in the form
c.     of Doppler velocity v.s. azimuth angle for a specific slant
c.     range.
c.
c.  MODULES CALLED: NONE.
c.
c.  PARAMETERS:          (*:  G = GLOBAL, C = COMMON, P = PASSED)
c.
c.    *   INPUT    TYPE        DESCRIPTION
c.    -   -----    ----        -----------
c.    P   AZM      R*4         The azimuth angles of the radials between
c.                             AZM_BEG & AZM_END, in degrees. RNG:[0,360].
c.    P   DNPT     I*4         Number of data points used to perform the least
c.                             squares fitting, Dummy variable.
c.                             Rng:[0,NAZIMS].
c.    P   MISSING  R*4         A variable indicating a particular piece of
c.                             data is missing.
c.    P   NAZIMS   I*4         A parameter used to dimension AZM() and VE().
c.    P   NRADIALS I*4         A variable indicating the number of radials of
c.                             data available for least squares fitting. Rng:
c.                             [0,NAZIMS]
c.    P   VE       R*4         Array of Doppler velocities at slant range,
c.                             VAD_RNG and azimuth angles, AZM_BEG, AZM-END,
c.                             within the current elevation scan.
c.
c.    *   OUTPUT  TYPE        DESCRIPTION
c.    -   ------  ----        -----------
c.    P   CF1     R*4         Fourier coefficient (zeroth harmonic.
c.                            Rng:[-100,+100]
c.    P   CF2     R*4         Fourier coefficient (real part of first
c.                            harmonic). Rng:[-100,+100]
c.    P   CF3     R*4         Fourier coefficient (imaginary part of first
c.                            harmonic. Rng:[-100,+100]
c.    P   DNPT    I*4         Number of data points used to perform the least
c.                            squares fitting, Dummy variable. Rng:[0,NAZIMS].
c.
c.    *   ACTUAL ARGUMENTS  TYPE        DESCRIPTION
c.    -   ----------------  ----        -----------
c.
c.  DATABASE/FILE REFERENCE:  None
c.
c.  INTERNAL TABLES/WORK AREA:
c.
c.    NAME      TYPE        DESCRIPTION
c.    ----      ----        -----------
c.    AZM_RAD   R*4         The azimuth angle of a radial in radians.
c.                          Rng:[0,2*PI].
c.    CCJ_Q4    C*8         Intermediate value representing the conjugate of
c.                          Q4 (used to compute the least squares fitted
c.                          harmonic coefficients.
c.    COS_AZ    R*4         The cosine of the azimuth angle for a particular
c.                          radial. Rng:[-1,1]
c.    DTR       R*4         Degrees to radians converstion factor
c.                          (0.017453...).
c.    I         I*4         Index, loop control.
c.    INT_COEFF C*8         Used to compute the fourier coefficient.
c.    Q0        C*8         Intermediate used to compute the least squares
c.                          fitted harmonic coefficients.
c.    Q1        C*8         Intermediate used to compute the least squares
c.                          fitted harmonic coefficients.
c.    Q2        C*8         Intermediate variable used to compute the least
c.                          squares fitted harmonic coefficients.
c.    Q3        C*8         Intermediate variable used to compute the least
c.                          squares fitted harmonic coefficients.
c.    Q4        C*8         Intermediate variable used to compute the least
c.                          squares fitted harmonic coefficients.
c.    Q5        C*8         Intermediate variable used to compute the least
c.                          squares fitted harmonic coefficients.
c.    QQ        C*8         Intermediate variable to reduce some calculations
c.                          and eliminate complex zero divide and floating
c.                          point overflows.
c.    QQ_INT    C*8         Intermediate variable to reduct some calculations
c.                          and eliminate complex zero divide and floating
c.                          point overflows.
c.    SIN_AZ    R*4         The sine of the azimuth angle for a particular
c.                          radial. Rng:[-1,1]
c.    SUM_Q0R   R*4         The variables
c.    SUM_Q3I   R*4         SUM_Q0R -> SUM_Q5R
c.    SUM_Q3R   R*4         are summation
c.    SUM_Q4I   R*4         variables used to
c.    SUM_Q4R   R*4         compute the real and
c.    SUM_Q5I   R*4         imaginary parts of the
c.    SUM_Q5R   R*4         variables Q0 -> Q6.
c.    TWO_N     I*4         Two times the number of data points used to
c.                          perform the least squares fitting.
c.                          Rng:[0,2*NAZIMS]
c.
c.  GLOBAL BLOCKS REFERENCED:
c.
c.
c.  COMMON BLOCKS REFERENCED:
c.
c.
c.  ERROR CONDITIONS:  NONE
c.
c.  ASSUMPTIONS/RESTRICTIONS: NONE
c.
c.  DEVIATION FROM STANDARDS:  None
c.
c.  COMPILATION INSTRUCTIONS:
c.
c.        THIS MODULE IS COMPILED USING THE COMP17.CSS
c.
c.  LINKAGE INSTRUCTIONS:
c.
c.        THIS MODULE IS LINKED USING THE LINK17.CSS
c.
c.  MISC:  This software uses the MKS units system.
c.            If not enough data exist to perform the least squares
c.            fitting, CF1 CF2 and CF3 are returned set to missing
c.
c.
c.*******************************************************************

      IMPLICIT NONE
      INTEGER NAZIMS,NRADIALS,DNPT,I
      REAL MISSING,AZM(NAZIMS),VE(NAZIMS),CF1,CF2,CF3
      COMPLEX Q0,Q5,Q4,Q3,Q2,Q1,CCJ_Q4,INT_COEFF,QQ,QQ_INT
      REAL SUM_Q0R,SUM_Q5R,SUM_Q5I,SUM_Q4R,SUM_Q4I,SUM_Q3R,SUM_Q3I,
     $     SIN_AZ,COS_AZ,AZM_RAD
      INTEGER TWO_N
C
C* DTR is the degrees to radians conversion.
C
      REAL DTR
      PARAMETER (DTR=0.01745329)
C
C* zero out variables used for summations.
C
      DNPT=0
C
C* the following variables are used to get the real and
C* imaginary parts of the variables Q3 through Q5.
C
      SUM_Q0R=0
      SUM_Q5R=0
      SUM_Q5I=0
      SUM_Q4R=0
      SUM_Q4I=0
      SUM_Q3R=0
      SUM_Q3I=0
C
C* perform summations for all good data points.
C
      DO 10 I=1,NRADIALS
        IF(ABS(VE(I)).LT.MISSING-0.1) THEN
C
C* convert azimuth angle from degrees to radians.
C
          AZM_RAD=AZM(I)*DTR
C
C* compute sine and cosine of azimuth angle since it is used
C* several times.
C
          SIN_AZ=SIN(AZM_RAD)
          COS_AZ=COS(AZM_RAD)
C
C* incriment number of good data points.
C
          DNPT=DNPT+1
C
C* perform summations used to construct complex variables Q3 -> Q5.
C
          SUM_Q0R=SUM_Q0R+VE(I)
          SUM_Q5R=SUM_Q5R+COS(2*AZM_RAD)
          SUM_Q5I=SUM_Q5I+SIN(2*AZM_RAD)
          SUM_Q4R=SUM_Q4R+COS_AZ
          SUM_Q4I=SUM_Q4I+SIN_AZ
          SUM_Q3R=SUM_Q3R+VE(I)*COS_AZ
          SUM_Q3I=SUM_Q3I+VE(I)*SIN_AZ
        END IF
 10   CONTINUE
C
C* if there is at least one good data point, complete calculations.
C
      IF(DNPT.GT.0) THEN
        TWO_N=2*DNPT
        Q0=CMPLX(SUM_Q0R/DNPT)
        Q5=CMPLX(SUM_Q5R/TWO_N,-SUM_Q5I/TWO_N)
        Q4=CMPLX(SUM_Q4R/TWO_N,SUM_Q4I/TWO_N)
        Q3=CMPLX(SUM_Q3R/DNPT,-SUM_Q3I/DNPT)
C
C* compute conjucate of Q4 since it is used several times
C
        CCJ_Q4=CONJG(Q4)
C* compute QQ (intermediate step) to save computations and to avoid
C* zero divide errors and floating point overflow errors
        QQ=Q4-1/(4*CCJ_Q4)
        IF (QQ.NE.0.0) THEN
          Q2=(CCJ_Q4-Q5/(2*CCJ_Q4))/QQ
          Q1=(Q0-Q3/(2*CCJ_Q4))/QQ
C
C* compute INT_COEFF since it is used several times.
C
C* compute QQ_INT here, to avoid a zero divisor error and subsequent pause
C
          QQ_INT=(1-(CABS(Q2))**2)
          IF (QQ_INT.NE.0.0) THEN
             INT_COEFF=(Q1-Q2*CONJG(Q1))/(1-(CABS(Q2))**2)
             CF3=IMAG(INT_COEFF)
             CF2=REAL(INT_COEFF)
             CF1=REAL(Q0)-2*REAL(INT_COEFF*Q4)
C
C* OTHERWISE, FOURIER COEFFICIENTS ARE MISSING
C
           ELSE
             CF1=MISSING
             CF2=MISSING
             CF3=MISSING
           END IF
C
C* OTHERWISE, FOURIER COEFFICIENTS ARE MISSING
C
        ELSE
          CF1=MISSING
          CF2=MISSING
          CF3=MISSING
        END IF
      ELSE
C
C* if you are here, there were not enough data points to
C* compute the coefficients CF1, CF2, CF3.
C
        CF1=MISSING
        CF2=MISSING
        CF3=MISSING
      END IF
      RETURN
      END
