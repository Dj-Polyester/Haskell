from random import randint

RANGE = 2

def genterm(var,rangenum,mindeg):
    global RANGE
    opbrackets=["","("]
    opbracket = opbrackets[randint(0,1)]
    clbracket= "" if opbracket=="" else ")"

    degree = randint(mindeg,mindeg+RANGE)
    #set coefficient
    coefficient = randint(-rangenum,rangenum)

    if degree:
        if coefficient:
            coefficient=str(coefficient)
            if coefficient=="1":
                coefficient = var
            elif coefficient=="-1":
                coefficient = "-"+var
                if randint(0,1):
                    coefficient="("+coefficient+")"
            else:
                if randint(0,1):
                    coefficient="("+coefficient+")"
            coefficient += ("*"+var)
            
            opbracket+=coefficient
            for _ in range(1,degree):
                opbracket+="*"
                opbracket+=var

            opbracket+=clbracket
            return (opbracket,degree)    
        return ("",0)    
    else:
        if coefficient:
            if randint(0,1):
                coefficient="("+str(coefficient)+")"
            return (str(coefficient),0)
    return ("",0)
    

def genpoly(randexprnum,rangenum):
    vars=["x","a","y"]
    exprnum = randint(1,randexprnum)
    opbrackets=["","("]
    randexpr = opbrackets[randint(0,1)]
    clbracket = ("" if randexpr=="" else ")")
    var = vars[randint(0,2)]

    term,degree=genterm(var,rangenum,0)
    for _ in range(exprnum):
        randexpr+=term
        randexpr+="+"
        term,degree=genterm(var,rangenum,degree+1)
        

    tmp=list(randexpr)
    tmp.pop()
    randexpr = "".join(tmp)
    randexpr+=clbracket
    return randexpr