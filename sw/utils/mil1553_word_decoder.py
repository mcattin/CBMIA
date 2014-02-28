#!   /usr/bin/env   python
#    coding: utf8

# Copyright CERN, 2011
# Author: Matthieu Cattin <matthieu.cattin@cern.ch>
# Licence: GPL v2 or later.
# Website: http://www.ohwr.org

import sys
import time
import os


# Command word fields
RT_POS = 11
RT_MSK = 0xF800
TR_POS = 10
TR_MSK = 0x0400
SA_POS = 5
SA_MSK = 0x03E0
WC_POS = 0
WC_MSK = 0x001F

# Status word fields
#RT_POS = 11
#RT_MSK = 0xF800
ME_POS = 10
ME_MSK = 0x0400
INS_POS = 9
INS_MSK = 0x0200
SR_POS = 8
SR_MSK = 0x0100
RESERVED_POS = 5
RESERVED_MSK = 0x00E0
BCR_POS = 4
BCR_MSK = 0x0010
BUY_POS = 3
BUY_MSK = 0x0008
SF_POS = 2
SF_MSK = 0x0004
DBC_POS = 1
DBC_MSK = 0x0002
TF_POS = 0
TF_MSK = 0x0001


def word_decode():
    word_type = ""
    while ((word_type != "C") and (word_type != "S")) :
    	word_type = raw_input("Command word [c] or status word [s] ?")
	word_type = word_type.upper()
        print " "

    word = int(raw_input("word: "),16)

    if word_type == "C":
        print "RT : %d" % ((word & RT_MSK) >> RT_POS)
        print "TR : %d" % ((word & TR_MSK) >> TR_POS)
        print "SA : %d" % ((word & SA_MSK) >> SA_POS)
        print "WC : %d" % ((word & WC_MSK) >> WC_POS)
    elif word_type == "S":
        print "RT : %d" % ((word & RT_MSK) >> RT_POS)
        print "ME : %d" % ((word & ME_MSK) >> ME_POS)
        print "INS : %d" % ((word & INS_MSK) >> INS_POS)
        print "SR : %d" % ((word & SR_MSK) >> SR_POS)
        print "BCR : %d" % ((word & BCR_MSK) >> BCR_POS)
        print "BUY : %d" % ((word & BUY_MSK) >> BUY_POS)
        print "SF : %d" % ((word & SF_MSK) >> SF_POS)
        print "DBC : %d" % ((word & DBC_MSK) >> DBC_POS)
        print "TF : %d" % ((word & TF_MSK) >> TF_POS)
    else:
        print "[ERROR] Unknown word type!"


if __name__ == '__main__' :

    while 1:
        word_decode()
