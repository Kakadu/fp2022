U = lambda {|g| g(g)}
fact = U (lambda {|g| lambda {|x| if x == 0 then 1 else x * (g(g))(x - 1) end}})
f1 = fact (5)


def Y (g)
    g (lambda { Y (g) })
end

fact = Y (lambda {|g| lambda {|x| if x == 0 then 1 else x * (g())(x - 1) end}})
f2 = fact (5)

Z = lambda {|g| t = lambda {|x| g(lambda {|v| (x(x))(v)})}; t(t)} 
sum = Z (lambda {|g| lambda {|from| lambda {|to| if from == to then to else from + (g(from + 1))(to) end}}})
[f1, f2, (sum(5))(8)]
