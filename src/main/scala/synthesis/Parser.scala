import scala.io.Source
import com.microsoft.z3._

object Parser {
  val ctx: Context = new Context()
  
  def parseProperty(propertyPath: String): List[Expr] = {
    val lines = Source.fromFile(propertyPath).getLines().toList
    lines.map(parseExpr)
  }
  
  def parseTrace(tracePath: String): List[Expr] = {
    val lines = Source.fromFile(tracePath).getLines().toList
    lines.map(parseAction)
  }
  
  private def parseExpr(line: String): Expr = {
    val solver = ctx.mkSolver()
    val reOnce = "♦\((.*?)\)".r
    val reExpr = "□\((.*?)\)".r

    val expr = line match {
      case reOnce(inner) => statemachine.once(parseZ3Expr(inner))
      case reExpr(inner) => ctx.mkForall(Array(), parseZ3Expr(inner), 1, null, null, null, null)
      case _ => throw new IllegalArgumentException(s"Invalid property format: $line")
    }
    expr
  }
  
  private def parseZ3Expr(expr: String): BoolExpr = {
    val eqPattern = "(\w+)\((.*?)\) → (.*?)".r
    expr match {
      case eqPattern(left, args, right) =>
        val leftExpr = ctx.mkBoolConst(left + "(" + args + ")")
        val rightExpr = ctx.mkBoolConst(right)
        ctx.mkImplies(leftExpr, rightExpr)
      case _ => ctx.mkBoolConst(expr)
    }
  }
  
  private def parseAction(line: String): Expr = {
    val eventPattern = "(\w+)\((.*?)\)@(\d+)".r
    line match {
      case eventPattern(event, params, time) =>
        val args = params.split(",").map(_.trim).filter(_.nonEmpty).map(parseParam)
        ctx.mkApp(ctx.mkFuncDecl(event, args.map(_.getSort), ctx.getBoolSort), args: _*)
      case _ => throw new IllegalArgumentException(s"Invalid trace format: $line")
    }
  }
  
  private def parseParam(param: String): Expr = {
    val kvPattern = "(\w+)=(\w+)".r
    param match {
      case kvPattern(key, value) => ctx.mkConst(key, ctx.getIntSort)
      case _ => ctx.mkConst(param, ctx.getIntSort)
    }
  }
}