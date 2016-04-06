def counted(f):
    def wrapped(*args, **kwargs):
        wrapped.calls += 1
        return f(*args, **kwargs)
    wrapped.calls = 0
    return wrapped

class MyList(list):
    @counted
    def pop(self, *args, **kwargs):
        return list.pop(self, *args, **kwargs)

x = MyList([1, 2, 3, 4, 5])
for i in range(3):
    x.pop()

print (x.pop.calls) # prints 3
