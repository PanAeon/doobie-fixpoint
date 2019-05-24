package example


import doobie._
import doobie.implicits._

import cats._
import cats.effect._
import cats.implicits._

import scala.concurrent.ExecutionContext
import doobie.h2.H2Transactor

import language.higherKinds

import atto._, Atto._

// FROM:  https://github.com/tpolecat/cofree
// see also matroshka
// sellout/recursion-scheme-talk

object Model {

// When I don't know what type will fit I introduce a type parameter
// pattern functor
case class ProfF[A](
    name: String,
    university: String,
    age: Int,
    students: List[A]) {
   override def toString = s"ProfF($name, $university, $age,  «${students.length}»)"
} 

object ProfF {
  
  implicit val profFunctor : Functor[ProfF] = new Functor[ProfF] {
    def map[A,B](fa: ProfF[A])(f: A => B) : ProfF[B] =
      fa.copy(students = fa.students.map(f))
  }
  
  implicit val profFTraverse : Traverse[ProfF] = new Traverse[ProfF] {
     /** As seen from <$anon: cats.Traverse[example.Model.ProfF]>, the missing signatures are as follows.  *  
      *  For convenience, these are usable as stub implementations. 
      *   */   
    
    // Members declared in cats.Foldable   
    def foldLeft[A, B](fa: ProfF[A],b0: B)(f: (B, A) => B): B = {
      fa.students.foldLeft(b0)(f)
    }
    def foldRight[A, B](fa: ProfF[A],lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa.students.foldRight(lb)(f)      
    // Members declared in cats.Traverse  
    def traverse[G[_], A, B](fa: ProfF[A])(f: A => G[B])(implicit ev: Applicative[G]): G[ProfF[B]] = {
      fa.students.traverse(f).map(xs => fa.copy(students = xs)) // G[List[B]] => G[ProfF[B]] // need unit?
    }
  }
}

// wow!!!
case class Prof1(value: ProfF[Prof1])
case class IdProf1(value: ProfF[IdProf1])
// generalize

type \/[A,B] = Either[A,B]
case class Fix[F[_]](unfix: F[Fix[F]])
case class Cofree[F[_], A](head:A, tail: F[Cofree[F, A]]) // associates an arbitrary label with each value. Comonad if F is a functor. recursive types
case class Free[F[_], A](resume: A \/ F[Free[F, A]]) // monad, if F is a functor. corecursive types
// tpolecat: anything I invent that is sufficiently polymorphic and plausibly useful
// probably already exists in scalaz
// lazyness and stack safety

// TODO: checkout matroshka

case class CofreeF[F[_], A,B](head: A, tail: F[B])
case class FreeF[F[_], A,B](resume: A \/ F[B])

type Cofree1[F[_], A] = Fix[({type λ[X] = CofreeF[F, A, X]})#λ]
type Free1[F[_], A] = Fix[({type λ[X] = FreeF[F, A, X]})#λ]

// Ex. defined flatMap for Free, and build it out, so you can do little DSL and interpreter

type Prof = Fix[ProfF]
type IdProf = Cofree[ProfF, Int]

} 

import Model._

object instances {
  
  
  val p1 : Prof = 
    Fix(ProfF("name", "", 13, List(
        Fix(ProfF("name", "", 13, List.empty)),
        Fix(ProfF("name", "", 13, List.empty))
      ))
    )
    
  val p2: IdProf = 
    Cofree(1, ProfF("name", "", 13, List(
        Cofree(2, ProfF("name", "", 13, List.empty)),
        Cofree(3, ProfF("name", "", 13, List.empty))
      ))
    )
    
}

object parsing {
   val data =
    """|Simeon Denis Poisson, École Polytechnique, 1800
       |  Gustav Peter Lejeune Dirichlet, Rheinische Friedrich-Wilhelms-Universität Bonn, 1827
       |    Rudolf Otto Sigismund Lipschitz, Universität Berlin, 1853
       |      C. Felix (Christian) Klein, Rheinische Friedrich-Wilhelms-Universität Bonn, 1868
       |        William Edward Story, Universität Leipzig, 1875
       |          Solomon Lefschetz, Clark University, 1911
       |            Albert William Tucker, Princeton University, 1932
       |              Marvin Lee Minsky, Princeton University, 1954
       |                Gerald Jay Sussman, Massachusetts Institute of Technology, 1973
       |                  Guy Lewis Steele, Massachusetts Institute of Technology, 1980
       |                    Philip Lee Wadler, Carnegie Mellon University, 1984
       |        C. L. Ferdinand (Carl Louis) Lindemann, Friedrich-Alexander-Universität Erlangen-Nürnberg, 1873
       |          David Hilbert, Universität Königsberg, 1885
       |            Wilhelm Ackermann, Georg-August-Universität Göttingen, 1925
       |            Haskell Curry, Georg-August-Universität Göttingen, 1930
       |            Hermann Weyl, Georg-August-Universität Göttingen, 1908
       |              Saunders Mac Lane, Georg-August-Universität Göttingen, 1934
       |                Steven Awodey, The University of Chicago, 1997
       |                William Howard, The University of Chicago, 1956
       |  Michel Chasles, École Polytechnique, 1814
       |    H. A. (Hubert Anson) Newton, Yale University, 1850
       |      E. H. (Eliakim Hastings) Moore, Yale University, 1885
       |        Oswald Veblen, The University of Chicago, 1903
       |          Alonzo Church, Princeton University, 1927
       |            Alan Mathison Turing, Princeton University, 1938
       |            Stephen Cole Kleene, Princeton University, 1934
       |              Robert Lee Constable, University of Wisconsin-Madison, 1968
       |                Robert William Harper, Cornell University, 1985
       |                  Benjamin Crawford Pierce, Carnegie Mellon University, 1991
       |""".stripMargin
       
  def parseProfFix(n:Int) : Parser[Fix[ProfF]] = for {
    _ <- manyN(n, char(' '))
    name <- stringOf(notChar(','))
    _ <- token(char(','))
    university <- stringOf(notChar(','))
    _ <- token(char(','))
    age <- int
    _ <- char('\n')
    xs <- many(parseProfFix(n+2))
  } yield Fix(ProfF(name, university, age, xs))
  
  def parseProfPos(n:Int) : Parser[Cofree[ProfF, (Int, Int)]] = for {
    p0 <- pos
    _ <- manyN(n, char(' '))
    name <- stringOf(notChar(','))
    _ <- token(char(','))
    university <- stringOf(notChar(','))
    _ <- token(char(','))
    age <- int
    _ <- char('\n')
    xs <- many(parseProfPos(n+2))
    p1 <- pos
  } yield Cofree((p0,p1), ProfF(name, university, age, xs ))
}
object Hello extends App {
  import parsing._
  
//  parseProfFix(0).parseOnly(data).option.foreach(drawFix(_))
  
//  parseProfPos(0).parseOnly(data).option.foreach(drawCofree(_))
  
  def drawFix[F[_]: Traverse](fa: Fix[F], indent: Int = 0): Unit = {
    print(" " * indent)
    println(fa.unfix)
    fa.unfix.traverse[Id, Unit](fix => drawFix(fix, indent+1))
  }
  
  def drawCofree[F[_]: Traverse, A](fa: Cofree[F, A], indent: Int = 0): Unit = {
    print(" " * indent)
    print(fa.head)
    print (" † ")
    println(fa.tail)
    fa.tail.traverse[Id, Unit](x => drawCofree(x, indent+1))
  }
    
  
  def insertNode(op: Option[Int], p: ProfF[_]): ConnectionIO[Unit] = {
    sql"""
      insert into prof (parent, name, year)
      values ($op, ${p.name}, ${p.age})
      """.update.run.map(_ => ())
  }
  
  def insertTree(op: Option[Int], fp: Fix[ProfF]): ConnectionIO[Cofree[ProfF, Int]] = {
    for {
      _ <- insertNode(None, fp.unfix)
      _id <- sql"SELECT LAST_INSERT_ID()".query[Int].unique
      xs  <- fp.unfix.traverse(insertTree(Some(_id), _))
               
    } yield Cofree(_id, xs)  
  }
  
  def readNode(id: Int) : ConnectionIO[ProfF[Int]] = for {
    x <- sql"".query[(String, Int)].unique
    ss <- sql"".query[Int].to[List]
  } yield ProfF(x._1, "uni", x._2, ss)
  
  // generic read && assemble in memory looks super interesting
  
  def readTree(id:Int) : ConnectionIO[Cofree[ProfF, Int]] = {
    readNode(id).flatMap { pi =>
      pi.map(readTree)
        .sequence
        .map(Cofree(id, _))
    }
  }
  
  // generalize ...
  def unfoldCM[M[_] : Monad, F[_]: Traverse, A](id:A)(f : A => M[F[A]]) : M[Cofree[F, A]] = {
    f(id).flatMap(_.traverse(unfoldCM(_)(f)).map(Cofree(id, _)))
  }
  
  def readTree1(id:Int) : ConnectionIO[Cofree[ProfF, Int]] =
    unfoldCM(id)(readNode)
  // and now it is unfoldCM (comonadic, )
  
  // the more polymorphic your code is, the fewer ways you can write it
  
  
  
  
  
  
//  def draw[F[_]: Traverse, A](fa: Cofree[F, A], indent: Int = 0): Unit = {
//    
//      print(" " * indent)
//      println(fa.head + " :< " + fa.tail)
//      fa.tail.traverse(draw(_, indent + 1))
//  }
  
  
  implicit val cs = IO.contextShift(ExecutionContext.global)
  
  
  val program1 = 42.pure[ConnectionIO]
  
//  val xa = Transactor.fromDriverManager[IO](
//    "org.postgresql.Driver", // driver classname
//    "jdbc:postgresql:world", // connect URL (driver-specific)
//    "postgres",              // user
//    ""                       // password
//  )
  
   val transactor: Resource[IO, H2Transactor[IO]] =
    for {
      ce <- ExecutionContexts.fixedThreadPool[IO](32) // our connect EC
      te <- ExecutionContexts.cachedThreadPool[IO]    // our transaction EC
      xa <- H2Transactor.newH2Transactor[IO](
              "jdbc:h2:mem:test;DB_CLOSE_DELAY=-1", // connect URL
              "sa",                                   // username
              "",                                     // password
              ce,                                     // await connection here
              te                                      // execute JDBC operations here
            )
    } yield xa

    
  val ddl = sql"""
    CREATE TABLE prof (
    id integer NOT NULL auto_increment,
    parent integer,
    name character varying NOT NULL,
    year integer NOT NULL
   )
  """

  def run(args: List[String]): IO[ExitCode] =
    transactor.use { xa =>

      // Construct and run your server here!
      for {
        _ <- ddl.update.run.transact(xa)
        tree <- insertTree(None, parseProfFix(0).parseOnly(data).option.get).transact(xa)
//        m <- sql"SELECT LAST_INSERT_ID()".query[Int].unique.transact(xa)
        n <- sql"select 42".query[Int].unique.transact(xa)
        _ <- IO(drawCofree(tree))
      } yield ExitCode.Success

    }
    
    run(List.empty).unsafeRunSync
}

