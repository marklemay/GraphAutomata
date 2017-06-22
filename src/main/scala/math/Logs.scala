package math

object Logs {
  /** take the log base 2 */
  def log2(x: Double) = if (x <= 0d) { 0d } else { Math.log10(x) / Math.log10(2.0) }
  
  //TODO: does Scala have a library we can call instead?
}