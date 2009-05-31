class A:
    def say_hello(self, value):
        print value, "says hello to Python"

def fib(n):
    if n <= 2: return 1
    return fib(n - 1) + fib(n - 2)
