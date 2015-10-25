def eval(expr: List[Any]): String = expr match {
  case fun :: expressions => {
    val parameters: List[String] = expressions.map {
      case list: List[Any] => eval(list)
      case str: String => str
      case _ => "ERROR"
    }
    fun match {
      case "+" => parameters.map(_.toDouble).sum
      case "*" => parameters.map(_.toDouble).product
      case "if" => if (parameters.head.toBoolean) parameters(1) else parameters(2)
      case ">" => parameters.head.toDouble > parameters(1).toDouble
      case "<" => parameters.head.toDouble < parameters(1).toDouble
      case ">=" => parameters.head.toDouble >= parameters(1).toDouble
      case "<=" => parameters.head.toDouble <= parameters(1).toDouble
      case "print" =>
        println(parameters.head)
        parameters.head
      /// i tak dalej ...
      case _ => "ERROR"
    }
  }.toString
}

println("simple functions")
eval(List("+", "4", "7"))
eval(List("*", "4", List("+", "4", "7")))
eval(List("if", List(">", "4", "7"), "wieksze", "mniejsze"))