module Adapter.Arguments where
import Types.CliArguments (CliArgs(..))
import Types.EertArguments (EertArgs(..))


buildArgs :: CliArgs -> EertArgs
buildArgs (CliArgs (file:_) _fallback) = EertArgs file
buildArgs (CliArgs []        fallback) = EertArgs fallback
