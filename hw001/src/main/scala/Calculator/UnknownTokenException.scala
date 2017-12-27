package Calculator

object UnknownTokenException extends Exception {
  override def getMessage: String = "Unknown token (function/operator/constant)"
}
