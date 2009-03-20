"""Python solutions to projecteuler[1] problems.

    [1]: http://projecteuler.net
"""

def p1(n=1000):
    """Find the sum of all the multiples of 3 or 5 below 1000."""
    return sum(i for i in range(1000) if i % 3 == 0 or i % 5 == 0)

def p2(n=4000000):
    """Find the sum of all the even-valued terms in the Fibonacci sequence which do not exceed four million."""
    def fibs(limit):
        x, y = 1, 2
        while x <= limit:
            yield x
            x, y = y, x+y
    return sum(x for x in fibs(n) if x % 2 == 0)

def p3(n=600851475143):
    """What is the largest prime factor of the number 600851475143 ?"""
    def is_prime(n):
        x = 2
        while x*x < n:
            if n % x == 0: 
                return False
            x += 1
        return True

    def factors(n):
        x = 2
        while x*x < n:
            if n%x == 0:
                yield x, n/x
            x += 1

    max = 1
    for x, y in factors(n):
        if is_prime(y):
            return y
        elif is_prime(x):
            max = x
    return max

def p4():
    """Find the largest palindrome made from the product of two 3-digit numbers."""
    def is_palindrome(n):
        n = str(n)
        return n == n[::-1]
    products = (x*x for x in range(999, 0, -1) for y in range(999, 0, -1))
    seq = (p for p in products if is_palindrome(p))
    return seq.next()

def p5():
    """What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?"""
    primes = [2, 3, 5, 7]
    numbers = [(2, 1)]

import time
def test(m):
    t1 = time.time()
    x = m()
    t = time.time() - t1
    print "%s\t%s\t%0.6f" % (m.__name__, x, t)

if __name__ == "__main__":
    import sys
    g = globals()
    keys = sys.argv[1:] or sorted(k for k in g if k.startswith("p"))
    methods = [g[k] for k in keys]
    for m in methods:
        test(m)
        
