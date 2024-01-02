

def est_premier(n):
    for k in range(2,int(n**0.5)):
        if n%k==0:
            return False
    return True

def largest_prime_factor(n):
    m=[]
    for k in range(1,int(n**0.5)+1):
        if est_premier(k) and n%k==0:
            if est_premier(n/k):
                m.append(n/k)
            m.append(k)
            print(k)
    return max(m)

print(largest_prime_factor(600851475143))