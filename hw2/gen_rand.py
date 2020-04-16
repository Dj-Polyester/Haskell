from random import randint
#last var to choose from
RANGE = 3 

def genrand(randexprnum,rangenum,threshold):
    global RANGE
    binaryops=["+","*"]
    unaryops=["","-"]
    brackets=["","(",")"]
    vars=["x","((x+31)*69)","(x+31)","y"]
    exprnum = randint(1,randexprnum)

    
    bracket = brackets[randint(0,1)]
    if bracket=="(":
        randexpr="("
        bracketcount=1
    elif bracket=="":
        randexpr=""
        bracketcount=0
    
    for _ in range(exprnum):
        anum=randint(1,rangenum)
        if anum > threshold and randint(0,1):
            anum = vars[randint(0,RANGE)]
        randunop = unaryops[randint(0,1)]
        randbinop = binaryops[randint(0,1)]

        #açık, kapalı olmaz
        randexpr+=str(randunop)
        #açık
        if bracketcount==0:
            bracket = brackets[randint(0,1)]
            randexpr+=str(bracket)
            if bracket=="(":
                bracketcount+=1
        randexpr+=str(anum)
        #kapalı
        if bracketcount!=0:
            bracket = brackets[randint(0,1)*2]
            randexpr+=str(bracket)
            if bracket==")":
                bracketcount-=1
        randexpr+=str(randbinop)
    anum=randint(1,rangenum)
    randexpr+=str(anum)
    if bracketcount!=0:
        randexpr+=")"
    return randexpr
