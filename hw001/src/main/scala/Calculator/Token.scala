package Calculator

private[Calculator] trait Token {}

private[Calculator] trait Operator extends Token {
  val precedence: Int
  object AssociativityOption extends Enumeration {
    type AssociativityOption = Value
    val Left, Right = Value
  }
  val associativity: AssociativityOption.Value
}

private[Calculator] trait UnaryOperator extends Operator {
  def eval(x: Double): Double
}

private[Calculator] trait BinaryOperator extends Operator {
  def eval(x: Double, y: Double): Double
}

private[Calculator] trait Function extends Token {
  def eval(xs: Double*): Double
  val arity: Int
}

private[Calculator] case object Number extends Token {}

/**
  * Left bracket is used as operator with maximal precedence.
  * Way of evaluation is undefined because it should never be called
  */
private[Calculator] case object LeftBracket extends UnaryOperator {
  override def eval(x: Double): Double = ???
  override val precedence: Int = 10
  override val associativity: AssociativityOption.Value = AssociativityOption.Left
}

/**
  * Right bracket is used as operator with minimal precedence.
  * Way of evaluation is undefined because 'eval' method should never be called
  */
private[Calculator] case object RightBracket extends UnaryOperator {
  override def eval(x: Double): Double = ???
  override val precedence: Int = 0
  override val associativity: AssociativityOption.Value = AssociativityOption.Left
}

private[Calculator] case object UnaryMinus extends UnaryOperator {
  override def eval(x: Double): Double = -x
  override val precedence: Int = 1
  override val associativity: AssociativityOption.Value = AssociativityOption.Left
}

private[Calculator] case object Addition extends BinaryOperator {
  override def eval(x: Double, y: Double): Double = x + y
  override val precedence: Int = 1
  override val associativity: AssociativityOption.Value = AssociativityOption.Left
}

private[Calculator] case object Subtraction extends BinaryOperator {
  override def eval(x: Double, y: Double): Double = x - y
  override val precedence: Int = 1
  override val associativity: AssociativityOption.Value = AssociativityOption.Left
}

private[Calculator] case object Multiplication extends BinaryOperator {
  override def eval(x: Double, y: Double): Double = x * y
  override val precedence: Int = 2
  override val associativity: AssociativityOption.Value = AssociativityOption.Left
}

private[Calculator] case object Division extends BinaryOperator {
  override def eval(x: Double, y: Double): Double = x / y
  override val precedence: Int = 2
  override val associativity: AssociativityOption.Value = AssociativityOption.Left
}

private[Calculator] case object Exponentiation extends BinaryOperator {
  override def eval(x: Double, y: Double): Double = math.pow(x, y)
  override val precedence: Int = 3
  override val associativity: AssociativityOption.Value = AssociativityOption.Right
}

private[Calculator] case object Sinus extends Function {
  override def eval(xs: Double*): Double = math.sin(xs(0))
  override val arity: Int = 1
}

private[Calculator] case object Cosinus extends Function {
  override def eval(xs: Double*): Double = math.cos(xs(0))
  override val arity: Int = 1
}

private[Calculator] case object Power extends Function {
  override def eval(xs: Double*): Double = math.pow(xs(0), xs(1))
  override val arity: Int = 2
}

/**
  * Constants are used as 0-arity functions
  */
private[Calculator] case object Pi extends Function {
  override def eval(xs: Double*): Double = math.Pi
  override val arity: Int = 0
}

private[Calculator] case object Comma extends Token {}

