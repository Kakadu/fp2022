def loop_factorial (i)
    x = 1
    while i > 0
        x = x * i
        i = i - 1
    end
    x
end

def rec_factorial (i)
    if i <= 1 then i else i * rec_factorial (i - 1) end
end

def map_indexed_array (arr)
    x = []
    i = 0
    while i < arr.length ()
        x = x + [yield (i, arr[i])]
        i = i + 1
    end
    x
end

z = [1, 2, 3]
[loop_factorial (5), rec_factorial (5), map_indexed_array ([1, 2, 3]) {|i, x| x * z[i]}]