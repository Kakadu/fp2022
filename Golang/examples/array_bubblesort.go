func sort(n []int) []int {
    var isDone = false;

    for !isDone {
        isDone = true;
        var i = 0;
        for i < len(n) - 1 {
            if n[i] > n[i + 1] {
				var temp = n[i];
				n[i] = n[i + 1];
				n[i + 1] = temp;
                isDone = false;
            }
            i = i + 1;
        }
    }

    return n;
}

func makearr() []int {
    var res = []int {};

    var i = 100;
    for i > 0 {
        res = append(res, i);
        i = i - 1;
    }
    return res;
}

func issorted(arr []int) bool {
    if len(arr) == 0 {
        return true;
    }

    var i = 1;
    for i < len(arr) {
        if arr[i - 1] > arr[i] {
            return false;
        }
        i = i + 1;
    }
    return true;
}

func main() {
    var arr = makearr();
    print(arr);
    print("\n");

    var res = sort(arr);
    print(res);
    print("\n");
    
    if issorted(res) {
        print("Sorted!");
    } else {
        print("Failed");
    }
}

