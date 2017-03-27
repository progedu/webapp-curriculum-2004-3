object EitherStudy {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(l) => Left(l)
      case Right(r) => Right(f(r))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = map(f) match {
      case Left(l) => Left(l)
      case Right(r) => r
    }
  }

  case class Left[+E](get: E) extends Either[E, Nothing]

  case class Right[+A](get: A) extends Either[Nothing, A]

}
