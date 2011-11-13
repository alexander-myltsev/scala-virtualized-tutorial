package org.scala_lang.virtualized.probability
import org.scala_lang.virtualized.{CoreDefs, CoreExps}

trait ProbDefsExps extends CoreDefs {
  case class VarInit[T](x: Exp[T]) extends Def[T]
  case class VarAssign[T](v: Exp[T], x: Exp[T]) extends Def[Unit]
  case class Obj[T](fields: Map[String, Exp[_]]) extends Exp[T]

  case class Case[T](val t:T, val prob:Double)
  
  case class WeightedCases[T](inp: List[Case[T]]) extends Def[T]
  case class CountedCases[T](inp: List[Case[T]]) extends Def[T]
}

trait EmbedProb extends ProbDefsExps {
  def __newVar[T](x: Exp[T]): Exp[T] = VarInit(x)
  def __new[T](args: (String, Exp[T] => Exp[_])*): Exp[T] = new Obj(args map {case (n, rhs) => (n, rhs(null))} toMap)
}

object Test extends App  {
  object Example extends EmbedProb with ProbCodeGen {
    def prog = {
      // a fair European roulette wheel
      var roulette = new CountedCases(List(Case("Even", 18), Case("Odd", 18), Case("Zero", 1)))
      roulette
      
      // drivers
      /*
      val trafficLight = WeightedCases(Case("Red", 0.5), Case("Yellow", 0.1), Case("Green", 0.4))
      def cautiousDriver(light:String) = Dist(light, List(
        "Red" -> "Stop", 
        "Yellow" -> WeightedCases(Case("Stop", 0.5), Case("Drive", 0.1)),
        "Green" -> "Drive"))
      */
    }
  }

  Example.emitFun("prog")(Example.prog) // output to console
}

trait ProbCodeGen extends ProbDefsExps {
  object ProbCounter {
    val rnd = new java.util.Random(1)
    
    abstract class Distribution[T] {
      def sample:T
      def expectation(h:(T)=>Double):Double
    }
    case class Always[T](x:T) extends Distribution[T] {
      def sample = x
      def expectation(h:(T=>Double)) = h(x)
    }
    case class CoinFlip[T] (p:Double, d1:Distribution[T], d2:Distribution[T]) extends Distribution[T] {
      assert(p >= 0.0 && p <= 1.0)
      def sample = if (rnd.nextDouble < p) d1.sample else d2.sample
      def expectation (h:T=>Double) = p * d1.expectation(h) + (1.0 - p) * d2.expectation(h)
    }
    
    def countedCasesProb[T](inp: List[Case[T]]) = {
      val total:Double = inp map (x => x.prob) sum
      val cases:List[Case[T]] = inp map (x => Case(x.t, x.prob / total))
      weightedCasesProb(cases)
    }
    
    def weightedCasesProb[T](inp: List[Case[T]]) = {
      assert(!inp.isEmpty)
      def coinFlips(w:Double, cs:List[Case[T]]): Distribution[T] = cs match {
        case Nil => throw new Exception("no coinFlips")
        case List(c) => Always(c.t)
        case c :: cs => CoinFlip(c.prob / (1.0 - w), Always(c.t), coinFlips(w + c.prob, cs))
      }
      coinFlips(0.0, inp)
    }
  }


  var nesting = 0
  var indent = true
  
  def emitPlain(s: String, more: Boolean = false) = {
    if (indent) print(" " * (nesting * 2))
    if (more) print(s) else println(s)
    indent = !more
  }

  def emitFun[T](name: String)(a: => Exp[T]) = {
    emitPlain("def " + name + "() = ", true)
    emitBlock(reifyBlock(a))
  }
  
  def emitBlock[T](a: Block[T], more: Boolean = false, s: Sym[T] = null) = a match {
    case Block(stms, e) =>
      emitPlain("{", false); nesting += 1
        stms foreach { case t: ScopeEntry[t] => emitNode[t](t.sym, t.rhs) }

        if(s == null) emitPlain(e.refStr)
        else emitPlain(s.refStr + " = " + e.refStr)
      nesting -= 1; emitPlain("}", more)
  }
  
  def emitNode[T](s: Sym[T], d: Def[T]): Unit = d match {
    case VarInit(x) => 
      emitPlain("var " + s.refStr + " = ", true); emitExpr(x); emitPlain("")
    case CountedCases(cases) =>
      emitPlain("var " + s.refStr + " = new " + CountedCases + "(", true)
      cases foreach { case c: Case[T] => emitPlain("[Case:" + c.t + ", " + c.prob + "] ", true) }
      emitPlain(") // Samples: ", true)
      val countedCases = ProbCounter.countedCasesProb(cases)
      for (i <- 1 to 5) emitPlain(countedCases.sample + ", ", true)
      // Expected payout of a $5 bet on Even, where you would get a $10 return
      //emitPlain("expectation: " + countedCases.expectation { case "Even" => 10.0; case "Odd" => 0.0; case "Zero" => 0.0 } )
      emitPlain("")
  }
  
  def emitExpr[T](expr: Exp[T]): Unit = expr match {
    case s@Sym(_) => emitPlain(s.refStr, true)
    case c@Const(_) => emitPlain(c.refStr, true)
  }
}