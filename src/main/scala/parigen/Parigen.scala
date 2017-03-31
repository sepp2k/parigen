package parigen

object Parigen {
    def compile(src: String) = {
        Parser.parse(src) match {
            case Parser.Success(ast, _) =>
                (Some(ast), Validator.validate(ast))
            case Parser.NoSuccess(message, _) =>
                (None, List( Validator.Diagnostic( Validator.Error, None, message )))
        }
    }
}