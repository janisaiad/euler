def dépasse():
    from math import log10
    phi = 0.5*(1+5**0.5)
    i= 0
    x=0
    while x<=999:  # le log donne le nombre de chiffre moins 1
        i+=1
        x=i*log10((2/(5**0.5))*phi)+log10((2/(5**0.5))*(1-(-1/(phi*phi))**i))
    return i


def naif():
    from math import log10
    a,b,i=0,1,1
    while log10(b)<999:
        a,b=b,a+b
        i+=1
    return i

# 4782