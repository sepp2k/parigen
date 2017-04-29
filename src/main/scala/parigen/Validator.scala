package parigen

object Validator {
    case class Diagnostic(severity: Severity, ruleNameOpt: Option[String], message: String) {
        override def toString = ruleNameOpt match {
            case Some(ruleName) => s"$severity in rule $ruleName: $message"
            case None => s"$severity: $message"
        }
    }
    object Diagnostic {
        def apply(severity: Severity, ruleName: String, message: String): Diagnostic = Diagnostic(severity, Some(ruleName), message)
    }
    sealed abstract class Severity
    case object Warning extends Severity
    case object Error extends Severity

    def validate(grammar: Ast.Grammar) = {
        grammar.rules.flatMap {
            case Ast.Rule(name, flags, exp) =>
                validateExpression(name, flags, exp, List())
        }
    }

    def validateExpression(name: String, flags: Ast.RuleFlags, exp: Ast.Expression, acc: List[Diagnostic]): List[Diagnostic] = exp match {
        case Ast.CharacterClass(ranges, negated) =>
            if (flags.tokenRule && ranges.isEmpty && ! negated) Diagnostic(Warning, name, "Empty character class.") :: acc
            else if (flags.tokenRule) acc
            else Diagnostic(Error, name, "Character classes are only allowed in token rules.") :: acc

        case Ast.RuleName(_) =>
            if (flags.tokenRule) Diagnostic(Error, name, "References to other rules are not allowed in token rules.") :: acc
            else acc

        case Ast.Or(lhs, rhs) =>
            validateExpression(name, flags, rhs, validateExpression(name, flags, lhs, acc))

        case Ast.Concattenation(lhs, rhs) =>
            validateExpression(name, flags, rhs, validateExpression(name, flags, lhs, acc))

        case Ast.KleeneStar(Ast.StringLit("")) =>
            Diagnostic(Warning, name, "Epsilon rule inside repetition.") :: acc

        case Ast.KleeneStar(arg) =>
            validateExpression(name, flags, arg, acc)

        case Ast.StringLit(_) =>
            acc
    }
}