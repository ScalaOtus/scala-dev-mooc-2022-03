package module2

object homework_hkt_impllicts{

    /**
      * 
      * Доработать сигнатуру tupleF и реализовать его
      * По итогу должны быть возможны подобные вызовы
      *   val r1 = println(tupleF(optA, optB))
      *   val r2 = println(tupleF(list1, list2))
      * 
      */
  
  trait Bindable[F[_], A] {
    def map[B](f: A => B): F[B]
    def flatMap[B](f: A => F[B]): F[B]
  }

  trait BindConv[F[_]] {
    def convert[A](v: F[A]): Bindable[F, A]
  }

  object BindConv {

    def apply[F[_]](implicit ev: BindConv[F]): BindConv[F] = ev

    implicit def convertOpt: BindConv[Option] = new BindConv[Option] {
      override def convert[A](v: Option[A]): Bindable[Option, A] = new Bindable[Option, A] {
        override def map[B](f: A => B): Option[B] = v.map(f)
        override def flatMap[B](f: A => Option[B]): Option[B] = v.flatMap(f)
      }
    }

    implicit def convertList: BindConv[List] = new BindConv[List] {
      override def convert[A](v: List[A]): Bindable[List, A] = new Bindable[List, A] {
        override def map[B](f: A => B): List[B] = v.map(f)
        override def flatMap[B](f: A => List[B]): List[B] = v.flatMap(f)
      }
    }
  }

  implicit class BindConvSyntax[F[_], A](v: F[A]) {
    def convert(implicit ev: BindConv[F]): Bindable[F, A] = ev.convert(v)
  }

  def tupleF[F[_]: BindConv, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
    for {
      a <- fa.convert
      b <- fb.convert
    } yield (a, b)
  }

  val optA: Option[Int] = Some(1)
  val optB: Option[Int] = Some(2)

  val list1 = List(1, 2, 3)
  val list2 = List(4, 5, 6)

  val r1 = println(tupleF(optA, optB))
  val r2 = println(tupleF(list1, list2))
}
