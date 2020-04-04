object EitherStudy {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = {
      this match {
        case Left(get)  => Left(get)
        case Right(get) => Right(f(get))
      }
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
      this match {
        case Left(get)  => Left(get)
        case Right(get) => f(get)
      }
    }
  }

  case class Left[+E](get: E) extends Either[E, Nothing]

  case class Right[+A](get: A) extends Either[Nothing, A]

}
