package object lens {

  def const[A,B](a: A): B => A = { _ => a }

  case class User(name: String, age: Int)
  case class Project(owner: User)

  trait NaiveLens[S,A] {
    def view(s: S): A
    def over(s: S)(f: A => A): S
    def set(s: S)(a: A): S = over(s)(const(a))
  }

  val luke = User(name = "Luke", age = 31)
  val proj = Project(luke)

  val naiveAgeLens = new NaiveLens[User, Int] {
    def view(u: User) = u.age
    def over(u: User)(f: Int => Int) =
      u.copy(age = f(u.age))
  }

  type Lens[S,A,F[_]] = (A => F[A]) => S => F[S]

  trait Functor[F[_]] {
    def fmap[A,B](fa: F[A])(f: A => B): F[B]
  }

  case class Identity[A](runIdentity: A)

  implicit val idFunctor = new Functor[Identity] {
    def fmap[A,B](ia: Identity[A])(f: A => B): Identity[B] =
      Identity(f(ia.runIdentity))
  }

  def over[S,A](ln: Lens[S,A,Identity])(f: A => A)(s: S): S = {
    ln(a => Identity(f(a)))(s).runIdentity
  }

  case class Const[A,B](getConst: A)

  implicit def constFunctor[A]: Functor[({type l[x] = Const[A,x]})#l] =
    new Functor[({type l[x] = Const[A,x]})#l] {
      def fmap[B,C](ca: Const[A,B])(f: B => C): Const[A,C] =
        Const[A,C](ca.getConst)
    }

  def view[S,A](ln: Lens[S,A,({type l[x] = Const[A,x]})#l])(s: S): A =
    ln(a => Const(a))(s).getConst

  def set[S,A](ln: Lens[S,A,Identity])(a: A)(s: S): S =
    over(ln)(const(a))(s)

  def _1[A,B,F[_]](implicit F: Functor[F]): Lens[(A,B),A,F] =
    f => {  case (x,y) => F.fmap(f(x))(a => (a,y)) }

  val firstOfPair = view(_1[Int,Int,({type l[x] = Const[Int,x]})#l])((2,3))
}
