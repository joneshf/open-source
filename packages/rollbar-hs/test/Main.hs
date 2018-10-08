module Main where

import qualified Rollbar.Golden
import qualified Rollbar.Item.Data.Test
import qualified Rollbar.Item.MissingHeaders.Test
import qualified Rollbar.Item.Request.Test

main :: IO ()
main = do
    Rollbar.Golden.main
    Rollbar.Item.Data.Test.props
    Rollbar.Item.MissingHeaders.Test.props
    Rollbar.Item.Request.Test.props
