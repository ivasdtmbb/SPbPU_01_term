#!/bin/bash

INITFILES=/home/ivasdtmbb/00_SPBPU_Politech_mycode/digital_literacy/lab02_files/
RESULTFILE=/home/ivasdtmbb/00_SPBPU_Politech_mycode/digital_literacy/base.txt

VALIDEMAIL_2=^(?:(?!.*?[.]{2})[a-zA-Z0-9](?:[a-zA-Z0-9.+!%-]{1,64}|)|\"[a-zA-Z0-9.+!% -]{1,64}\")@[a-zA-Z0-9][a-zA-Z0-9.-]+(.[a-z]{2,}|.[0-9]{1,})$
VALIDEMAIL=[a-zA-Z]+[a-zA-Z0-9.-]*@[[a-zA-Z0-9]{2,}\.[a-zA-Z]{2}-?]+


MYVALIDEMAIL="[a-z][a-z0-9]*[.-]{0,1}[a-z0-9]*@[a-z0-9]+.[a-z0-9]+"


find $FILES -name "***" -exec grep -Eioha $VALIDEMAIL  '{}' \; | tr [:upper:] [:lower:] | sort #> ~/base.txt

#at now + 1 minute




