def repeat (n)
    i = 0
    while n > i
        yield (i)
        i = i + 1
    end
end

@x = [1, 2, 3]
repeat (5) { |i| @x = @x + [i] }
@x