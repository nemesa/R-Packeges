# PPBF - ProcessPrestoBlueFiles
R package to process result files from a Plate Reader

## Install and use devtools
``` r
install.packages("devtools")
library(devtools)
```

## Install and use the package
``` r
install_github("nemesa/R-Packeges/PPBF")
library(PPBF)

parseResult<-PPBF.Run("c:/_WORK_/R/File/PrestoBlue_Sample.csv")
print(parseResult$RowNum)
print(parseResult$ColNum)
print(parseResult$Data)
```
## Console result:
```
> library(PPBF)
> parseResult<-PPBF.Run("c:/_WORK_/R/File/PrestoBlue_Sample.csv")
> print(parseResult$RowNum)
[1] 8
> print(parseResult$ColNum)
[1] 12
> print(parseResult$Data)
  X00 X01 X02 X03 X04 X05 X06 X07 X08 X09 X10 X11 X12
1   A 101 102 103 104 105 106 107 108 109 110 111 112
2   B 201 202 203 204 205 206 207 208 209 210 211 212
3   C 301 302 303 304 305 306 307 308 309 310 311 312
4   D 401 402 403 404 405 406 407 408 409 410 411 412
5   E 501 502 503 504 505 506 507 508 509 510 511 512
6   F 601 602 603 604 605 606 607 608 609 610 611 612
7   G 701 702 703 704 705 706 707 708 709 710 711 712
8   H 801 802 803 804 805 806 807 808 809 810 811 812
```

## Example file's content (PrestoBlue_Sample.csv):
```
Plate information 
--- more text ---
No background information available.
Results for Meas A -  (RFU)
,01,02,03,04,05,06,07,08,09,10,11,12,
A,101,102,103,104,105,106,107,108,109,110,111,112,
B,201,202,203,204,205,206,207,208,209,210,211,212,
C,301,302,303,304,305,306,307,308,309,310,311,312,
D,401,402,403,404,405,406,407,408,409,410,411,412,
E,501,502,503,504,505,506,507,508,509,510,511,512,
F,601,602,603,604,605,606,607,608,609,610,611,612,
G,701,702,703,704,705,706,707,708,709,710,711,712,
H,801,802,803,804,805,806,807,808,809,810,811,812,

--- more text ---

Number of rows,,,,8
Number of columns,,,,12
Number of the wells in the plate,,,,96
Height of the plate,,,,14.35 mm
Diameter of the well,,,,6.5 mm
Volume of the well,,,,300 µl

--- more text ---

Exported with EnSpire Workstation version 4.10.3005.1440
```

