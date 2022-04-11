# Sourav Das (MSc 1st Sem)

def euler(f, x0, y0, x, n):
    """
    Solves y'(x) = f(x,y) at x, given initial values x0 and y0
    Returns value of y at x
    """
    h = (x - x0)/n
    for _ in range(n):
        yn = y0 + h*f(x0, y0)
        y0 = yn
        x0 = x0 + h

    return yn

def f(x, y): return x*y

# Driver code
x0 = 0
y0 = 1
n = 1000
x = 2
print(f"Value of y({x}) = {euler(f, x0, y0, x, n)}")
