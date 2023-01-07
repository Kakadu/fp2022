class Rectangle
    def initialize (side_a, side_b)
        @side_a = side_a
        @side_b = side_b
    end

    def surface
        @side_a * @side_b
    end

    def set_name (name)
        @name = name
    end

    def info
        [@name, surface ()]
    end 
end

rect = Rectangle.new(10, 20)
rect.set_name("House")
rect.info ()