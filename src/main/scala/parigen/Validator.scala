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

    def validate(grammar: ast.Grammar) = {
        grammar.rules.values.flatMap {
            case ast.Rule(name, flags, exp) =>
                validateExpression(name, flags, exp, List())
        }
    }

    def validateExpression(name: String, flags: ast.RuleFlags, exp: ast.Expression, acc: List[Diagnostic]): List[Diagnostic] = exp match {
        case ast.CharacterClass(ranges, negated) =>
            if (flags.tokenRule && ranges.isEmpty && ! negated) Diagnostic(Warning, name, "Empty character class.") :: acc
            else if (flags.tokenRule) acc
            else Diagnostic(Error, name, "Character classes are only allowed in token rules.") :: acc

        case ast.RuleName(_) =>
            if (flags.tokenRule) Diagnostic(Error, name, "References to other rules are not allowed in token rules.") :: acc
            else acc

        case ast.Or(lhs, rhs) =>
            validateExpression(name, flags, rhs, validateExpression(name, flags, lhs, acc))

        case ast.Concattenation(lhs, rhs) =>
            validateExpression(name, flags, rhs, validateExpression(name, flags, lhs, acc))

        case ast.KleeneStar(ast.Epsilon) =>
            Diagnostic(Warning, name, "Epsilon rule inside repetition.") :: acc

        case ast.KleeneStar(arg) =>
            validateExpression(name, flags, arg, acc)

        case ast.StringLit(_) | ast.Epsilon =>
            acc
    }
}