package Calculator

object InvalidExpressionException extends Exception {
    override def getMessage: String = "Invalid expression. Try again."
}
