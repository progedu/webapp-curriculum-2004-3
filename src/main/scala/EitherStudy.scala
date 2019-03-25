object EitherStudy {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(a) => Left(a)
      case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(a) => Left(a)
      case Right(a) => f(a)
    }
  }

  case class Left[+E](get: E) extends Either[E, Nothing]

  case class Right[+A](get: A) extends Either[Nothing, A]

}
