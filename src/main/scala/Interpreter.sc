implicit class Expression(tuple: Product) {

  def eval: Any = tuple match {
    case (s, n: Product, m: Product) => (s, n.eval, m.eval).eval
    case (s, n, m: Product) => (s, n, m.eval).eval
    case (s, n: Product, m) => (s, n.eval, m).eval

    case (s, n: Product, m: Product, o: Product) => (s, n.eval, m.eval, o.eval).eval
    case (s, n, m: Product, o) => (s, n, m.eval, o).eval
    case (s, n: Product, m, o) => (s, n.eval, m, o).eval
    case (s, n, m, o: Product) => (s, n, m, o.eval).eval
    //math
    case ("+", n: Int, m: Int) => n + m
    case ("-", n: Int, m: Int) => n - m
    case ("/", n: Int, m: Int) => n / m
    case ("*", n: Int, m: Int) => n * m
    //string
    case ("+", n: String, m: String) => n + m
    case ("length", n: String) => n.length
    //bool
    case ("==", n: Int, m: Int) => n == m
    case (">", n: Int, m: Int) => n > m
    case (">=", n: Int, m: Int) => n >= m
    case ("<", n: Int, m: Int) => n < m
    case ("<=", n: Int, m: Int) => n <= m
    //val

    //if, for
    case ("if", n: Boolean, m) => if (n) m
    case ("if", n: Boolean, m, o) => if (n) m else o
    //def
  }
}
println("simple functions")
("length", "foobar").eval
println("counting")
("+", 1, 2).eval
//TODO: counting with many params
println("if")
("if", true, 4).eval
("if", false, 4).eval
println("else")
("if", false, 3, 7).eval
println("if with nested param")
("if", ("<", 2, 3), "2 < 3").eval
println("else with nested param")
("if", (">", 2, 3), "2 > 3", "false!").eval
println("counting with nested param")
("+", ("*", 5, 6), 3).eval
