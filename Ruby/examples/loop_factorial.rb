def loop_factorial (i)
    x = 1
    while i > 0
        x = x * i
        i = i - 1
    end
    x
end

puts (loop_factorial(5))