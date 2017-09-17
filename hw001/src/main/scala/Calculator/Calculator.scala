package Calculator

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Calculator {

  /**
    * Split input into tokens and number values and calculate expression value
    * @param input Arithmetic expression represented as String
    * @return Value of expression
    */
  def calculate(input: String): Double = {
    val tokensAndNumbers = Tokenizer.extractTokensAndNumbers(input)
    calculate(tokensAndNumbers._1, tokensAndNumbers._2)
  }

  /**
    * Evaluate top operation in 'operatorsStack'
    * @param operatorsStack Stack containing operators
    * @param numbersStack Stack containing numbers
    */
  private[this] def evaluateOperator(operatorsStack: mutable.ArrayStack[Operator],
                             numbersStack: mutable.ArrayStack[Double]): Unit = {
      try {
        operatorsStack.pop() match {
          case binaryOperator: BinaryOperator =>
            val y = numbersStack.pop()
            val x = numbersStack.pop()
            numbersStack.push(binaryOperator.eval(x, y))
          case unaryOperator: UnaryOperator =>
            val x = numbersStack.pop()
            numbersStack.push(unaryOperator.eval(x))
        }
      } catch {
        case _: NotImplementedError => throw InvalidExpressionException
      }
  }

  /**
    * Calculate arithmetic expression which doesn't contain functions but numbers and operators
    * (an brackets) only
    * @param tokens List of tokens
    * @param numbers List of number values
    * @return Expression value
    */
  private[this] def calculateExpressionWithoutFunctions(tokens: ListBuffer[Token],
                                                        numbers: ListBuffer[Double]): Double = {
    val numbersStack = new mutable.ArrayStack[Double]
    val operatorsStack = new mutable.ArrayStack[Operator]
    var numberPos = 0
    for (token <- tokens) {
      token match {
        case Number =>
          numbersStack.push(numbers(numberPos))
          numberPos += 1
        case operator: Operator =>
          val currentPrecedence = operator.precedence
          while (operatorsStack.nonEmpty && operatorsStack.top != LeftBracket
            && operatorsStack.top.precedence >= currentPrecedence
            && operator.associativity == operator.AssociativityOption.Left) {
            evaluateOperator(operatorsStack, numbersStack)
          }
          token match {
            case RightBracket => operatorsStack.pop()
            case _ => operatorsStack.push(token.asInstanceOf[Operator])
          }
      }
    }
    while (operatorsStack.nonEmpty) {
      evaluateOperator(operatorsStack, numbersStack)
    }
    if (numbersStack.size != 1)
      throw InvalidExpressionException
    numbersStack.head
  }

  /**
    * Calculate arithmetic expression. Calculate function arguments recursively for every
    * functions. Calculate function value for every function.
    * Call 'calculateExpressionWithoutFunctions'.
    * @param tokens List of tokens
    * @param numbers List of number values
    * @return Expression value
    */
  private[this] def calculate(tokens: ListBuffer[Token], numbers: ListBuffer[Double]): Double = {
    try {
      var numberPos = 0
      var tokenPos = 0
      while (tokenPos < tokens.size) {
        tokens(tokenPos) match {
          case Number =>
            numberPos += 1
          case function: Function =>
            function.arity match {
              case 0 =>
                tokens.update(tokenPos, Number)
                numbers.insert(numberPos, function.eval())
                tokenPos += 1
                numberPos += 1
              case _ =>
                val oldTokenPos = tokenPos
                val oldNumberPos = numberPos
                val tokenBorders = new ListBuffer[Int]
                val numberBorders = new ListBuffer[Int]
                var balance = 1
                var commasLeft = 1
                tokenPos += 2
                tokenBorders += tokenPos
                numberBorders += numberPos
                while (tokenBorders.size < function.arity + 1) {
                  while (tokenBorders.size < function.arity && commasLeft > 0
                    || tokenBorders.size == function.arity && balance != 0) {
                    tokens(tokenPos) match {
                      case Comma => commasLeft -= 1
                      case LeftBracket => balance += 1
                      case RightBracket => balance -= 1
                      case Number => numberPos += 1
                      case innerFunction: Function =>
                        commasLeft += innerFunction.arity - 1
                      case _ =>
                    }
                    tokenPos += 1
                  }
                  tokenBorders += tokenPos
                  numberBorders += numberPos
                  commasLeft = 1
                }
                val xs = new ListBuffer[Double]
                for (i <- 0 until function.arity) {
                  xs += calculate(tokens.slice(tokenBorders(i), tokenBorders(i + 1) - 1),
                    numbers.slice(numberBorders(i), numberBorders(i + 1)))
                }
                tokens.remove(oldTokenPos, tokenPos - oldTokenPos)
                tokens.insert(oldTokenPos, Number)
                tokenPos = oldTokenPos + 1
                numbers.remove(oldNumberPos, numberPos - oldNumberPos)
                numbers.insert(oldNumberPos, function.eval(xs: _*))
                numberPos = oldNumberPos + 1
            }
          case _ =>
        }
        tokenPos += 1
      }
      calculateExpressionWithoutFunctions(tokens, numbers)
    }
    catch {
      case _: Exception => throw InvalidExpressionException
    }
  }
}
