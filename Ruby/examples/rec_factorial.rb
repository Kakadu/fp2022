def rec_factorial (i)
    if i <= 1 then i else i * rec_factorial (i - 1) end
end

rec_factorial(5)
