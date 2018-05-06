package ch3

trait Codec[A]{
  def encode(value: A): String
  def decode(value: String): A
  def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      val self = this
    new Codec[B] {
    override def encode(value: B): String = self.encode(enc(value))

    override def decode(value: String): B = dec(self.decode(value))
  }}

}
