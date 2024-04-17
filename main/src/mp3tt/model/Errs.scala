package mp3tt.model

trait TyCkErr extends Throwable {
  def msg: String
}

case class TypeMismatch(actual: String, expected: String) extends TyCkErr {
  override def msg: String = s"type mismatch: expected $expected, got $actual"
}

case class VariableOutOfScope(name: String) extends TyCkErr {
  override def msg: String = s"variable out of scope: $name"
}