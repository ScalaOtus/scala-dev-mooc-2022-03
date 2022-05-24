package module1

import java.util.UUID
import scala.annotation.tailrec
import java.time.Instant
import scala.language.postfixOps



/**
 * referential transparency
 */
 object referential_transparency{


  case class Abiturient(id: String, email: String, fio: String)

  type Html = String

  sealed trait Notification

  object Notification{
    case class Email(email: String, text: Html) extends Notification
    case class Sms(telephone: String, msg: String) extends Notification
  }


  case class AbiturientDTO(email: String, fio: String, password: String)

  trait NotificationService{
    def sendNotification(notification: Notification): Unit
    def createNotification(abiturient: Abiturient): Notification
  }


  trait AbiturientService{

    def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient
  }

  class AbiturientServiceImpl(val notificationService: NotificationService) extends AbiturientService{
    override def registerAbiturient(abiturientDTO: AbiturientDTO): Abiturient = {
      val notification = Notification.Email("", "")
      val abiturient = Abiturient(UUID.randomUUID().toString, abiturientDTO.email, abiturientDTO.fio)
      //notificationService.sendNotification(notification)
      // save(abiturient)
      abiturient
    }
  }


}


 // recursion

object recursion {

  /**
   * Реализовать метод вычисления n!
   * n! = 1 * 2 * ... n
   */

  def fact(n: Int): Int = {
    var _n = 1
    var i = 2
    while (i <= n){
      _n *= i
      i += 1
    }
    _n
  }


  def factRec(n: Int): Int = {
    if( n <= 0) 1 else n * factRec(n - 1)
  }

  def factTailRec(n: Int): Int = {
    @tailrec
    def loop(n: Int, accum: Int): Int =
      if( n <= 1) accum
      else loop(n - 1, n * accum)
    loop(n, 1)
  }



  /**
   * реализовать вычисление N числа Фибоначчи
   * F0 = 0, F1 = 1, Fn = Fn-1 + Fn - 2
   *
   */


}

object hof{

   trait Consumer{
       def subscribe(topic: String): LazyList[Record]
   }

   case class Record(value: String)

   case class Request()
   
   object Request {
       def parse(str: String): Request = ???
   }

  /**
   *
   * Реализовать ф-цию, которая будет четать записи Request из топика, и сохранять их в базу
   */
   def createRequestSubscription() = {
     val cons : Consumer = ???
     val stream: LazyList[Record] = cons.subscribe("request")
     stream.foreach{ rec =>
       val req = Request.parse(rec.value)
       // save(req)
     }
   }

  def createSubscription(topic: String)(action: Record => Unit) = {
    val cons : Consumer = ???
    val stream: LazyList[Record] = cons.subscribe(topic)
    stream.foreach{ rec =>
     action(rec)
    }
  }

  def createRequestSubscription2() = createSubscription("request"){rec =>
    val req = Request.parse(rec.value)
    // save(req)
  }

  

  // обертки

  def logRunningTime[A, B](f: A => B): A => B = a => {
    val start = System.currentTimeMillis()
    val result: B = f(a)
    val end = System.currentTimeMillis()
    println(end - start)
    result
  }






  // изменение поведения ф-ции

  val arr = Array(1, 2, 3, 4, 5)

  def isOdd(i: Int): Boolean = i % 2 > 0

  
   
  def not[T](f: T => Boolean): T => Boolean = i => ! f(i)
  
  
  lazy val isEven = not(isOdd)




  // изменение самой функции

  // Follow type implementation

  def partial[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  def sum(x: Int, y: Int): Int = x + y

  val res: Int => Int = partial(1, sum)
  res(2) // sum(1, 2)


}






/**
 *  Реализуем тип Option
 */


 object opt {

  /**
   *
   * Реализовать тип Option, который будет указывать на присутствие либо отсутсвие результата
   */

   sealed trait Option[+T]{
      def isEmpty: Boolean = this match {
        case Option.Some(v) => false
        case Option.None => true
      }

      def get: T = this match {
        case Option.Some(v) => v
        case Option.None => throw new Exception("Get on empty Option")
      }

      def map[B](f: T => B): Option[B] = this match {
        case Option.Some(v) => Option.Some(f(v))
        case Option.None => Option.None
      }

      def flatMap[B](f: T => Option[B]): Option[B] = this match {
        case Option.Some(v) => f(v)
        case Option.None => Option.None
      }
    
      def printIfAny(): Unit = this match {
        case Option.Some(v) => println(v)
        case Option.None => ()
      }

      def zip[U](v2: Option[U]): Option[(T, U)] = {
        for {
          v1Out <- this
          v2Out  <- v2
        } yield (v1Out, v2Out)
      }

      def filter(f: T => Boolean): Option[T] = this match {
        case Option.Some(v) => if (f(v)) Option.Some(v) else Option.None
        case Option.None => Option.None
      }
   }

   object Option{
     case class Some[T](v: T) extends Option[T]
     case object None extends Option[Nothing]

     def apply[T](v: T): Option[T] = Some(v)
   }


  /**
   *
   * Реализовать метод printIfAny, который будет печатать значение, если оно есть
   */


  /**
   *
   * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
   */


  /**
   *
   * Реализовать метод filter, который будет возвращать не пустой Option
   * в случае если исходный не пуст и предикат от значения = true
   */

 }

 object list {
   /**
    *
    * Реализовать односвязанный иммутабельный список List
    * Список имеет два случая:
    * Nil - пустой список
    * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
    */

    sealed trait List[+T]{
     def ::[A >: T](elem: A): List[A] = List.::(elem, this)
     
     def mkString[A >: T](sep: String): String = {
      @tailrec
      def loop(accum: String, list: List[A]): String = {
        list match {
          case List.Nil => accum
          case List.::(head, tail) => loop(accum + sep + head.toString, tail)
        }
      }
      val list = this match {
        case List.Nil => this
        case List.::(_, tail) => tail
      }
      val acc = this match {
        case List.Nil => ""
        case List.::(head, _) => head.toString
      }
      loop(acc, list)
    }

    def reverse[A >: T]: List[A] = {
      @tailrec
      def loop(accum: List[A], list: List[A]): List[A] = {
        list match {
          case List.Nil => accum
          case List.::(head, tail) => loop(head::accum, tail)
        }
      }
      loop(List.Nil, this)
    }

    def map[A >: T](f: T => A): List[A] = {
      @tailrec
      def loop(accum: List[A], list: List[T], f: T => A): List[A] = {
        list match {
          case List.Nil => accum
          case List.::(head, tail) => loop(f(head)::accum, tail, f)
        }
      }
      val acc = List.Nil
      val list = loop(acc, this, e => e).asInstanceOf[List[T]]
      loop(acc, list, f)
    }

    def filter[A >: T](f: A => Boolean): List[A] = {
      @tailrec
      def loop(accum: List[A], list: List[A], f: A => Boolean): List[A] = {
        list match {
          case List.Nil => accum
          case List.::(head, tail) if f(head) => loop(head::accum, tail, f)
          case List.::(head, tail) if !f(head) => loop(accum, tail, f)
        }
      }
      val acc = List.Nil
      val list = loop(acc, this, _ => true)
      loop(acc, list, f)
    }
   }

   object List{
     case class ::[A](head: A, tail: List[A]) extends List[A]
     case object Nil extends List[Nothing]
    
     def apply[A](elems: A*): List[A] = {
       @tailrec
       def loop(accum: List[A], els: A*): List[A] = {
         val l = els.length
         if (l == 0) accum
         else loop(els(l - 1)::accum, els.take(l - 1): _*)
       }
       loop(Nil.asInstanceOf[List[A]], elems: _*)
     }
   }
  
   def incList(list: List[Int]): List[Int] = list.map(_ + 1)

   def shoutString(list: List[String]): List[String] = list.map(_ + "!")

   /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */

    /**
      * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
      *
      */

    /**
      * Конструктор, позволяющий создать список из N - го числа аргументов
      * Для этого можно воспользоваться *
      * 
      * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
      * def printArgs(args: Int*) = args.foreach(println(_))
      */

    /**
      *
      * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
      */

    /**
      *
      * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
      */


    /**
      *
      * Реализовать метод filter для списка который будет фильтровать список по некому условию
      */

    /**
      *
      * Написать функцию incList котрая будет принимать список Int и возвращать список,
      * где каждый элемент будет увеличен на 1
      */


    /**
      *
      * Написать функцию shoutString котрая будет принимать список String и возвращать список,
      * где к каждому элементу будет добавлен префикс в виде '!'
      */

 }
