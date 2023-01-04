def map_array (arr)
    x = []
    i = 0
    while i < arr.length ()
        x = x + [yield (arr[i])]
        i = i + 1
    end
    x
end

map_array ([1, 2, 3]) {|x| x * x}