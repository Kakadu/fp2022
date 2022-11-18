def fact(n):
    if n < 0:
        return
    if n == 0 or n == 1:
        return 1
    return n * fact(n-1)


[fact(1), fact(2), fact(3), fact(4), fact(5)]
