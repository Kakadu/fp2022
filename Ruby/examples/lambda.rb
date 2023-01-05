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
map_indexed_array ([4, 5, 6]) {|i, x| x * z[i]}