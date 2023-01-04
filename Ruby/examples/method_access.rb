class Rectangle
    def initialize (side_a, side_b)
        @side_a = side_a
        @side_b = side_b
    end
    def surface
        @side_a * @side_b
    end
end

Rectangle