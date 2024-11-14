def power_mod(a,b,n):
    q=1
    for _ in range(b):
        q=(a*q)%n
    return q
