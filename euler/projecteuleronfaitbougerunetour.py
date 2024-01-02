x,y=10,4


def f(a,b):# a de longueur, b de hauteur, b=4 pour le test
    L=[[(0,0)]]
    for _ in range(a*b-1):
        Q=[]
        for j in L:
            pos=j[-1]
            Q=Q+(evolue(j,pos,a,b))
        L=Q[:]
    m=[]
    for k in L:
        if k[-1]==(a-1,0):
            m.append(k)
    return m



def evolue(x,pos,a,b):# à un chemin et une position on renvoie la liste des chemins possibles depuis
    m,n=pos[0],pos[1]
    if m==0:
        if n==0:
            return [x+[(m+k[0],n+k[1])] for k in {(1,0),(0,1)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
        if n==b-1:
            return  [x+[(m+k[0],n+k[1])] for k in {(1,0),(0,-1)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
        return [x+[(m+k[0],n+k[1])] for k in {(1,0),(0,-1),(0,1)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
    if m==a-1:
        if n==0:
            return [x+[(m+k[0],n+k[1])] for k in {(-1,0),(0,1)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
        
        if n==b-1:
            return [x+[(m+k[0],n+k[1])] for k in {(-1,0),(0,-1)} if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
        return [x+[(m+k[0],n+k[1])] for k in {(-1,0),(0,-1),(0,1)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
    if n==0:
         return [x+[(m+k[0],n+k[1])] for k in {(1,0),(0,1),(-1,0)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
    if n==b-1:
          return [x+[(m+k[0],n+k[1])] for k in {(1,0),(0,-1),(-1,0)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]
    return [x+[(m+k[0],n+k[1])] for k in {(1,0),(0,1),(-1,0),(0,-1)}  if not ((pos[0]+k[0],pos[1]+k[1]) in x)]


# algorithme naïf terminé

# print(f(6,4))






