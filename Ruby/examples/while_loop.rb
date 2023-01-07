x = 137
d = []
while x > 0
    d = [x % 2] + d
    x = x / 2
end
d